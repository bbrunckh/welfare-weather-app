# Define the Server logic
server <- function(input, output, session) {
  
  #----------------------------------------------------------------------------#
  # INITIALIZE REACTIVE VALUES
  #----------------------------------------------------------------------------#
  
  internalpanel <- reactiveVal(FALSE) # visibility of the password panel
  auth_message <- reactiveVal("")
  surveys <- reactiveVal(survey_list_master |> filter(external))
  
  #----------------------------------------------------------------------------#
  # SAMPLE UI
  #----------------------------------------------------------------------------#
  
  # Observe the action button click
  observeEvent(input$internal_user, {
    # Toggle the visibility state
    internalpanel(!internalpanel())
  })
  
  # # Render the password panel conditionally
  # output$dlw_token_input <- renderUI({
  #   if (internalpanel()) {
  #     tagList(
  #       passwordInput("dlw_token", "Enter datalibweb token:", ""),
  #       actionButton("authorize", "Authorize"),
  #       hr(),
  #     )
  #   }
  # })
  # 
  # # Authorization status
  # output$authorize_status <- renderText({
  #   req(input$authorize)
  #   auth_message()
  # })
  # 
  # # Observe the action button click
  # observeEvent(input$authorize, {
  #   surveys(survey_list_master |> filter(external))
  #   auth_message("Checking data access...")
  #   tryCatch({
  #     surveys(check_gmd_access(input$dlw_token))
  #       auth_message("Authorization complete")
  #     }, error = function(e){
  #       auth_message("Authorization failed")
  #       surveys(survey_list_master |> filter(external))
  #     })
  #     internalpanel(!internalpanel())
  # })
  
  # country selection
  output$sample_ui <- renderUI({
    req(surveys())
    selectizeInput(
      inputId = "country",
      label = "Country",
      choices = surveys()$countryname,
      multiple = TRUE,
      options = list(maxItems = 1, placeholder = "Select country")
    )
  })
  
  # Get available survey years for selected countries
  available_years <- reactive({
    req(input$country, surveys())
    selected_countries <- input$country
    years_list <- list()

    for (country in selected_countries) {
      years_list[[country]] <- surveys() |>
        filter(countryname == country) |> pull(year)
    }
    return(years_list)
  })

  # Survey year UI based on selected country
  output$survey_year_ui <- renderUI({
    req(input$country)
    selected_countries <- input$country
    years_data <- available_years()

    tagList(
      lapply(selected_countries, function(country_name) {
        selectizeInput(
          inputId = paste0("survey_year_", gsub(" ", "_", country_name)),
          label = paste("Survey years for", country_name, ":"),
          choices = years_data[[country_name]],
          selected = years_data[[country_name]], # Default to all available years
          multiple = TRUE,
          options = list(placeholder = paste("Select years for", country_name))
        )
      })
    )
  })

  #----------------------------------------------------------------------------#
  # WELFARE UI
  #----------------------------------------------------------------------------#

  # Dynamically render poverty line / welfare year input based on selected welfare outcome
  output$welfare_ui <- renderUI({
    req(input$welfare_outcome)
    if (input$welfare_outcome == "Log welfare ($/day, PPP)" | input$welfare_outcome == "Poor (PPP)") {
      tagList(
        selectizeInput(
          inputId = "ppp_year",
          label = "PPP year",
          choices = c("2011", "2017", "2021"),
          selected = "2021"
        ),
        if (input$welfare_outcome == "Poor (PPP)") {
          sliderInput(
            inputId = "poverty_line_ppp",
            label = "Poverty line ($/day, PPP)",
            min = 1.00,
            max = 15.00,
            value = 3.00,
            step = 0.05,
            pre = "$",
          )
        }
      )
    } else if (input$welfare_outcome == "Log welfare (LCU/day)" | input$welfare_outcome == "Poor (LCU)") {
      tagList(
        selectizeInput(
          inputId = "lcu_year",
          label = "LCU year",
          choices = c("2011", "2017", "2021"),
          selected = "2021"
        ),
        if (input$welfare_outcome == "Poor (LCU)") {
          numericInput(
            inputId = "poverty_line_lcu",
            label = "Poverty line (LCU)",
            value = 1000
          )
        }
      )
    }
  })

  # Reactive with info on selected welfare outcome
  welf_select <- reactive({
    req(input$welfare_outcome)
    data.frame(
      selected = input$welfare_outcome,
      year = ifelse(grepl("PPP", input$welfare_outcome),
        input$ppp_year, input$lcu_year
      ),
      varname = ifelse(grepl("PPP", input$welfare_outcome),
        paste0("welf_ppp_", input$ppp_year),
        paste0("welf_lcu_", input$lcu_year)
      ),
      label = ifelse(grepl("PPP", input$welfare_outcome),
        paste0("$ per person per day (", input$ppp_year, " PPP)"),
        paste0("LCU per person per day (", input$lcu_year, ")")
      ),
      pre = ifelse(grepl("PPP", input$welfare_outcome), "$", "LCU"),
      type = ifelse(grepl("Log", input$welfare_outcome), "Continuous", "Binary"),
      pline = ifelse(grepl("Poor", input$welfare_outcome) & grepl("PPP", input$welfare_outcome),
        input$poverty_line_ppp,
        ifelse(grepl("Poor", input$welfare_outcome) & grepl("LCU", input$welfare_outcome),
          input$poverty_line_lcu, NA
        )
      )
    )
  })

  #----------------------------------------------------------------------------#
  # LOAD SURVEY DATA
  #----------------------------------------------------------------------------#
  
  # Get survey data file paths for selected sample
  survey_data_files <- reactive({
    req(input$country)
    selected_countries <- input$country
    survey_files <- c()
    for (country in selected_countries) {
      year_input_id <- paste0("survey_year_", gsub(" ", "_", country))
      selected_years <- input[[year_input_id]]
      
      if (!is.null(selected_years) && length(selected_years) > 0) {
        country_data <- surveys() |>
          filter(countryname == country & year %in% selected_years) |>
          pull(wiseapp_pin)
        survey_files <- c(survey_files, country_data)
      }
    }
    return(paste0(pin_prefix, as.character(survey_files)))
  })
  
  # load survey data for selected sample
  survey_data <- reactive({
    req(input$country)
    
    # read relevant parquet files
    local_paths <- lapply(survey_data_files(), function(pin) {
      pin_download(board, pin)
    })
    survey_data <- read_parquet_duckdb(unlist(local_paths),
                                       options = list(union_by_name = TRUE),
                                       prudence = "lavish") 
    
    # filter by sample type
    if (input$sample_type == "Rural households"){
      survey_data <- filter(survey_data, urban ==0)
    } else if (input$sample_type == "Rural households"){
      survey_data <- filter(survey_data, urban ==1)
    }
    
    # selected welfare variable
    survey_data <- survey_data |>
      filter(welfare != 0, !is.na(welfare)) |>
      mutate(log_welf = log(.data[[welf_select()$varname]]))
             
    # add log welfare variable to varlist
    varlist <- varlist |>
      bind_rows(data.frame(varname = "log_welf",
                           label = paste0("Log ", welf_select()$label)))
    
    if(welf_select()$type == "Binary"){
      if (welf_select()$pre == "$"){ 
      survey_data <- survey_data |>
        mutate(poor = (welfare/.data[[paste0("cpi",welf_select()$year)]]/.data[[paste0("icp",welf_select()$year)]]/365) < welf_select()$pline)
      
      } else if (welf_select()$pre == "LCU"){
      survey_data <- survey_data |>
        mutate(poor = (welfare/.data[[paste0("cpi",welf_select()$year)]]/365) < welf_select()$pline)
      }
      # add poor welfare variable to varlist
      varlist <- varlist |>
        bind_rows(data.frame(varname = "poor",
                             label = paste0("Poor, living below ", 
                                            welf_select()$pre,
                                            sprintf("%.2f", welf_select()$pline),
                                            " per person per day ", 
                                            sub("^.*?\\(", "(",
                                                welf_select()$label))))
    }
    # logical data columns > integer, categorical and FE data columns > factors
    
    cats <- filter(varlist, !is.na(wiseapp), datatype == "Categorical") |> pull(varname)
    fe <- filter(varlist, !is.na(wiseapp) & wiseapp == "ID & Fixed effects" & !varname %in% c("countryname", "survname","hhid", "year", "timestamp", "int_date")) |>
      pull("varname")
    
    survey_data <- survey_data |>
      mutate(year = as.integer(year),
             countryyear = paste0(countryname, ", ", year),
             across(any_of(cats), as.factor),
             across(any_of(fe), as.factor)) |>
      collect()
  
    # add var labels
    add_labels <- function(data, labels) {
      for (i in 1:nrow(labels)) {
        var_name <- labels$varname[i]
        var_label <- labels$label[i]
        if (var_name %in% names(data)) {
          var_labelled <- labelled::set_variable_labels(data[[var_name]], var_label)
          data[[var_name]] <- var_labelled
        }
      }
      return(data)
    }
    survey_data <- add_labels(survey_data, varlist)
    
    return(survey_data)
  })
  
  # Loads survey interview location data
  survey_geo <- reactive({
    req(input$country)
    
    pin_names <- paste0(survey_data_files(), "_LOC")

    survey_geo <- do.call(rbind, lapply(pin_names, function(pin) pin_read(board, pin))) |>
      filter(case_when(
        input$sample_type == "All households" ~ !is.na(loc_id),
        input$sample_type == "Rural households" ~ urban == 0,
        input$sample_type == "Urban households" ~ urban == 1
      )) |>
        st_as_sf(wkt = "geom", crs = 4326)
    return(survey_geo)
  })
  
  #----------------------------------------------------------------------------#
  # SURVEY STATS OUTPUT
  #----------------------------------------------------------------------------#

  # tracker if the "Survey stats" tab has been added
  survey_tab_added <- reactiveVal(FALSE)

  # Observer for "Survey stats"
  observeEvent(input$survey_stats,
    {
      if (!survey_tab_added()) {
        # interview date plot
        output$interview_date <- renderPlot({
          req(survey_data())
          
          interview_dates <- survey_data() |>
            summarise(hh = n(), .by = c(countryname, countryyear, timestamp))

          ggplot(interview_dates, 
                 aes(x = timestamp ,y = hh, fill = countryname)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(
              title = "", x = "",
              y = "Number of households", fill = ""
            )
        })

        # interview location map
        output$map <- renderLeaflet({
          req(survey_geo())
          polygon_data <- survey_geo()

          # color palette
          gg_color_hue <- scales::hue_pal()
          n_colors <- length(input$country)
          default_colors <- gg_color_hue(n_colors)
          pal <- colorFactor(
            palette = default_colors,
            domain = polygon_data$code
          )

          # leaflet map
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(
              data = polygon_data,
              fillColor = "transparent", # No fill color
              weight = 1, # Line thickness
              opacity = 0.5, # Line opacity
              color = ~ pal(code),
              dashArray = "", # Solid line
              fillOpacity = 0.5, # No fill opacity
              highlight = highlightOptions(
                weight = 1,
                color = "#FF0000",
                dashArray = "",
                fillOpacity = 0.5,
                bringToFront = TRUE
              ),
              # 'int_dates' value label on hover
              label = ~ paste("Interview dates:", int_dates), # Create label content
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              )
            ) |>
            fitBounds(
              lng1 = min(st_bbox(polygon_data)[1]),
              lat1 = min(st_bbox(polygon_data)[2]),
              lng2 = max(st_bbox(polygon_data)[3]),
              lat2 = max(st_bbox(polygon_data)[4])
            )
        })

        # welfare distribution plot
        output$welfare_dist <- renderPlot({
          req(survey_data())

          p <- ggplot(survey_data() , aes(
            x = log_welf,
            y = countryyear,
            fill = code
          )) +
            geom_density_ridges(alpha = 0.7, scale = 2)

          # Add PPP poverty lines
          if (welf_select()$pre == "$") {
            povln <- pov_lines[pov_lines$ppp_year == welf_select()$year, "ln"]
            log_povln <- log(povln)
            p <- p + geom_vline(xintercept = log_povln) +
              annotate("text",
                x = log_povln[1] - 0.1, y = 1,
                label = paste0("$", sprintf("%.2f", povln[1]), "/day"),
                angle = 90, size = 4
              ) +
              annotate("text",
                x = log_povln[2] - 0.1, y = 1,
                label = paste0("$", sprintf("%.2f", povln[2]), "/day"),
                angle = 90, size = 4
              ) +
              annotate("text",
                x = log_povln[3] - 0.1, y = 1,
                label = paste0("$", sprintf("%.2f", povln[3]), "/day"),
                angle = 90, size = 4
              )
          }

          # User specified poverty line
          if (input$welfare_outcome == "Poor (PPP)" | input$welfare_outcome == "Poor (LCU)") {
            log_pline <- log(welf_select()$pline)
            p <- p + geom_vline(xintercept = log_pline, color = "darkred") +
              annotate("text",
                x = log_pline - 0.1, y = 1,
                label = paste0(welf_select()$pre, sprintf("%.2f", welf_select()$pline), "/day"),
                angle = 90, size = 4, color = "darkred"
              )
          }

          p + theme_minimal() +
            labs(
              title = "", x = paste0("Log ", welf_select()$label),
              y = "", fill = ""
            ) +
            theme(legend.position = "none")
        })

        # Render survey stat outputs
        appendTab(
          inputId = "step1_output_tabs",
          tabPanel(
            title = "Survey stats",
            value = "desc_stats",
            layout_columns(
              col_widths = c(6, 6),
              card(
                h4("Timing of interviews"),
                plotOutput("interview_date", height = "300px")
              ),
              card(
                h4("Location of interviews"),
                leafletOutput("map", height = "300px")
              )
            ),
            br(),
            card(
              h4("Welfare distribution"),
              plotOutput("welfare_dist", height = "300px")
            ),
            h4("Welfare summary stats"),
            output$data_table <- renderDT({
              req(survey_data())
                desc <- survey_data() |>
                  dplyr::select(countryyear, weight, welf_ppp_2021, welf_lcu_2021,
                    poor_300ln, poor_420ln, poor_830ln,any_of(c("log_welf", "poor")))

                sumtable(desc,
                  vars = colnames(desc)[-c(1,2)],
                  summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", "min(x)", "max(x)", "notNA(x)"),
                  summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
                  group = "countryyear",
                  group.long = TRUE,
                  group.weights = "weight",
                  labels = TRUE,
                  out = "return"
                )
              },
              rownames = FALSE
            ),
            h4("Household characteristics"),
            output$hh_stats <- renderDT(
              {
                req(survey_data())
                desc <- survey_data() |>
                  dplyr::select(countryyear, weight,
                    any_of(varlist[varlist$wiseapp == "HH characteristics" & !is.na(varlist$wiseapp), "varname"])
                  ) 

                sumtable(desc,
                  vars = colnames(desc)[-c(1,2)],
                  summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", "min(x)", "max(x)", "notNA(x)"),
                  summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
                  group = "countryyear",
                  group.long = TRUE,
                  group.weights = "weight",
                  labels = TRUE,
                  out = "return"
                )
              },
              rownames = FALSE
            ),
            h4("Area characteristics"),
            output$area_stats <- renderDT(
              {
                req(survey_data())
                desc <- survey_data() |>
                  dplyr::select(countryyear, weight,
                    any_of(varlist[varlist$wiseapp == "Area characteristics" & !is.na(varlist$wiseapp), "varname"])
                  ) 
                
                sumtable(desc,
                  vars = colnames(desc)[-c(1,2)],
                  summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", 
                           "min(x)", "max(x)", "notNA(x)"),
                  summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
                  group = "countryyear",
                  group.long = TRUE,
                  group.weights = "weight",
                  labels = TRUE,
                  out = "return"
                )
              },
              rownames = FALSE
            ),
          ),
          select = TRUE # Select (activate) the newly added tab
        )
        survey_tab_added(TRUE) # Mark the tab as added
      }
      if (survey_tab_added()) {
        # If tab already exists and one of the buttons is clicked again, just select it
        updateTabsetPanel(session, "step1_output_tabs", selected = "desc_stats")
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  ) # ignoreInit for actionButtons, ignoreNULL for list observation


  #----------------------------------------------------------------------------#
  # WEATHER UI
  #----------------------------------------------------------------------------#

  # Dynamically render weather variable constructor based on variable(s) chosen
  output$weather_construction_ui <- renderUI({
    req(input$weather_variable_selector)

    # Use lapply to generate UI for each selected weather variable
    ui_list <- lapply(seq_along(input$weather_variable_selector), function(i) {
      current_var_name <- input$weather_variable_selector[i]
      # Create a unique prefix for input IDs for this variable's UI block.
      id_prefix <- paste0(weather_list[weather_list$name==current_var_name,"varname"], "_")

      # --- Start of UI block for one weather variable ---

      tagList( # Added tagList to group the button and its conditional panel
        hr(),
        p(paste0(current_var_name, ":")),
        actionButton(
          paste0(id_prefix, "toggle"),
          "Configure"
        ),
        # Inner sidebar panel with collapsible content
        conditionalPanel(
          condition = paste0("input['", id_prefix, "toggle'] % 2 == 1"), # Toggle visibility based on button clicks
          # The content of the conditionalPanel was already a tagList, which is good.
          tagList(
            if (i > 1) hr(), # Add a horizontal rule to separate UI blocks, except before the first one
            # Reference period
            h5("Reference period"),
            sliderInput(paste0(id_prefix, "relativePeriod"), # Note the prepended id_prefix
              "Months before interview",
              min = 0,
              max = 12,
              value = c(1, 1)
            ),

            # Temporal aggregation of variable over reference period
            if (weather_list[weather_list$name == current_var_name, ]$units %in% c("days", "mm")) {
              selectInput(paste0(id_prefix, "temporalAgg"),
                "Aggregation over reference period:",
                choices = c("Mean", "Median", "Min", "Max", "Sum"),
                selected = "Sum"
              )
            } else {
              selectInput(paste0(id_prefix, "temporalAgg"),
                "Aggregation over reference period:",
                choices = c("Mean", "Median", "Min", "Max"),
                selected = "Mean"
              )
            },

            # Transformation
            if (weather_list[weather_list$name == current_var_name, ]$units %in% c("Dimensionless")) {
              radioButtons(paste0(id_prefix, "varConstruction"),
                "Transformation",
                choices = c("Standardized anomaly"),
                selected = "Standardized anomaly"
              )
            } else {
              radioButtons(paste0(id_prefix, "varConstruction"),
                "Transformation",
                choices = c(
                  "None",
                  "Deviation from mean",
                  "Standardized anomaly"
                ),
                selected = "None"
              )
            },

            # Continuous or binned
            radioButtons(paste0(id_prefix, "contOrBinned"),
              "Continuous or binned",
              # choices = c("Continuous", "Binned")
              choices = c("Continuous")
            ),
            helpText("A binned option will be added soon."),

            # Conditional UI for binned options
            conditionalPanel(
              condition = paste0("input['", id_prefix, "contOrBinned'] == 'Binned'"),
              tagList(
                sliderInput(paste0(id_prefix, "numBins"),
                  "Number of bins:",
                  min = 2,
                  max = 10,
                  value = 5
                ),
                radioButtons(paste0(id_prefix, "binningMethod"),
                  "Binning method:",
                  choices = c(
                    "Equal frequency",
                    "Equal size",
                    "K-means clustering"
                  )
                )
              )
            ),
            # Conditional UI for polynomial terms
            conditionalPanel(
              condition = paste0("input['", id_prefix, "contOrBinned'] == 'Continuous'"),
              checkboxGroupInput(
                inputId = paste0(id_prefix, "polynomial"),
                label = "Include polynomial terms",
                choices = c("Quadratic" = "a", "Cubic" = "b")
              ),
            )
          )
        )
      )
    }) # End of lapply
    # Return the list of UI elements. Shiny will render them sequentially.
    # do.call is essential here to unpack the list of tagLists into individual arguments for tagList()
    tagList(
      do.call(tagList, ui_list),
      hr(),
      actionButton("weather_stats", "Weather stats")
    )
  })

  #----------------------------------------------------------------------------#
  # LOAD WEATHER DATA
  #----------------------------------------------------------------------------#
  
  # Reactive expression that loads survey H3 data
  survey_h3 <- reactive({
    req(input$country)
    
    pin_names <- paste0(survey_data_files(), "_H3")
    local_paths <- lapply(pin_names, function(pin) pin_download(board, pin))
    
    survey_h3 <- read_parquet_duckdb(unlist(local_paths), 
                                     options = list(union_by_name = TRUE),
                                     prudence = "lavish") 
    
    # # create h3 panel ID
    # edge_list <- survey_h3 |>
    #   inner_join(survey_h3, by = "h3_7", relationship = "many-to-many") |>
    #   filter(loc_id.x != loc_id.y) |>
    #   select(from = loc_id.x, to = loc_id.y) |>
    #   distinct() |> collect() # Get unique pairs
    # 
    # loc_graph <- graph_from_data_frame(edge_list, directed = FALSE)
    # all_loc_ids <- as.character(unique(survey_h3$loc_id))
    # loc_graph <- add_vertices(
    #   loc_graph,
    #   nv = length(setdiff(all_loc_ids, V(loc_graph)$name)),
    #   attr = list(name = setdiff(all_loc_ids, V(loc_graph)$name))
    # )
    # components <- components(loc_graph)
    # group_mapping <- tibble(
    #   loc_id = as.numeric(names(components$membership)),
    #   h3_loc_id = components$membership
    # )
    # survey_h3 <- survey_h3 |>
    #   left_join(group_mapping, by = "loc_id") |>
    #   mutate(loc_id = as.character(loc_id),
    #          h3_loc_id = as.character(h3_loc_id)) |> 
    #   as_duckdb_tibble()
    
    return(survey_h3)
  })
  
  # selected weather variables
  weather_vars <- reactive({
    req(input$weather_variable_selector)
    weather_vars <- weather_list |>
      filter(name %in% input$weather_variable_selector) |>
      pull(varname)
    return(weather_vars)
  })
  
  # load selected weather variables for survey locations 
  weather_data <- reactive({
    req(input$country)
    
    # dates for weather vars (use 1990-2024 as default)
    survey_dates <- survey_data() |> pull(timestamp) |> na.omit()
    survey_date_min <- min(survey_dates) - months(12)
    survey_date_max <- max(survey_dates)
    
    weather_dates <- seq(min(survey_date_min, as.Date(paste0(min(input$yearRange),"-12-01"))) - months(12),
                         max(survey_date_max, as.Date(paste0(max(input$yearRange),"-12-01"))), 
                         by = "1 month")
    # get data
    pin_names <- paste0(unique(
      substr(survey_data_files(), 1, nchar(survey_data_files())-4)), "weather")
    local_paths <- lapply(pin_names, function(pin) pin_download(board, pin))
    
    weather <- read_parquet_duckdb(unlist(local_paths), 
                                   options = list(union_by_name = TRUE),
                                   prudence = "lavish") 
    
    # filter to selected weather variables, sample h3 cells & date range
    weather <- weather |>
      dplyr::select(h3_6, timestamp, all_of(weather_vars())) |>
      filter(h3_6 %in% survey_h3()$h3_6) |>
      filter(timestamp %in% weather_dates) |>
      distinct() 
    
    return(weather)
  })
  
  # configure weather variables 
  h3_weather <- reactive({
    
    # loop over selected weather variables
    for (i in weather_vars()){
      
      # get configuration for selected weather variable
      id_prefix <- paste0(i, "_")
      
      ref_period <- input[[paste0(id_prefix, "relativePeriod")]]
      ref_start <- if(!is.null(ref_period)) ref_period[1] else NA
      ref_end <- if(!is.null(ref_period)) ref_period[2] else NA
      
      temporal_agg <- input[[paste0(id_prefix, "temporalAgg")]] %||% ""
      transformation <- input[[paste0(id_prefix, "varConstruction")]] %||% ""
      cont_binned <- input[[paste0(id_prefix, "contOrBinned")]] %||% ""
      
      if(cont_binned == "Binned") {
        num_bins <- as.character(input[[paste0(id_prefix, "numBins")]] %||% "")
        binning_method <- input[[paste0(id_prefix, "binningMethod")]] %||% ""
      } 
      
    # Construct configured weather variable
    weather <- weather_data() |>
      group_by(h3_6)
    
    # get variable for each month in selected reference period
    for (l in seq(ref_start, ref_end)){
      colname <- paste0(i, "_",l)
      weather <- weather |> 
        mutate("{colname}" := lag(.data[[i]], n = l, order_by = timestamp)) 
    }
    
    # temporal aggregation over reference period
    weather <- weather |> 
      filter(!is.na(pick(paste0(i, "_", max(ref_start,ref_end))))) |> # drop NA
      ungroup()
    
    if (temporal_agg == "Mean") {
      weather <- weather |>
        mutate(haz = rowMeans(across(starts_with(paste0(i,"_")))))
    }
    if (temporal_agg == "Median") {
      weather <- weather |>
        mutate(haz = apply(dplyr::select(., starts_with(paste0(i, "_"))), 1, median, na.rm = TRUE))
    }
    if (temporal_agg == "Min") {
      weather <- weather |>
        mutate(haz = do.call(pmin, c(across(starts_with(paste0(i, "_"))), na.rm = TRUE)))
    }
    if (temporal_agg == "Max") {
      weather <- weather |>
        mutate(haz = do.call(pmax, c(across(starts_with(paste0(i, "_"))), na.rm = TRUE)))
    }
    if (temporal_agg == "Sum") {
      weather <- weather |>
        mutate(haz = rowSums(across(starts_with(paste0(i,"_")))))
    }
    
    # transform
    if (transformation == "None" || i %in% c("spi6", "spei6")) { 
      # do nothing
    } else {
      
      # calculate mean and sd for reference climate by month and H3 cell
      
      weather <- weather |>
        mutate(year = year(timestamp), 
               month = month(timestamp))
      
      climate_ref <- weather |>
        filter(year >=1991 & year <=2020) |>
        summarise(mean = mean(haz, na.rm = TRUE),
                  sd = sd(haz, na.rm = TRUE),
                  .by = c(h3_6, month))
      
      if (transformation == "Deviation from mean") {
        weather <- weather |>
          left_join(climate_ref) |>
          mutate(haz = (haz - mean))
        
      } else if (transformation == "Standardized anomaly") {
        weather <- weather |>
          left_join(climate_ref) |>
          mutate(haz = (haz - mean)/sd)
      }
    }
    
    # Binned 
    if (cont_binned == "Binned"){
      # !!Not implemented yet
    }
    
    # keep only the configured weather variable
    weather <- weather |>
      dplyr::select(h3_6, timestamp, haz) |>
      rename_with(~ paste0("haz_",i), .cols = starts_with("haz")) |>
      arrange(h3_6, timestamp) 
    
    if (i == weather_vars()[1]){h3_weather <- weather } else{
      h3_weather <- full_join(h3_weather, weather)}
    }
    return(h3_weather)
  })
  
  # Reactive expression that matches survey data to weather data
  loc_weather <- reactive({
    req(input$weather_variable_selector, input$country, survey_h3(), h3_weather())
    
    loc_weather <- survey_h3() |>
      left_join(h3_weather()) |>
      summarise(across(starts_with("haz"), ~sum(.x*pop_2020, na.rm = TRUE)/sum(pop_2020, na.rm = TRUE)),
                .by = c(code, year, survname, loc_id, timestamp)) |>
      mutate(loc_id = as.character(loc_id))
    
    return(loc_weather)
  })
    
    survey_weather <- reactive({
    req(input$weather_variable_selector, input$country, loc_weather())
    
    survey_weather <- survey_data() |>
      left_join(loc_weather()) |>
      mutate(year = as.factor(year)) |>
      group_by(code, year, survname) |>
      mutate(weight = weight / sum(weight)) |> # normalize weights per survey.
      ungroup()
    
    # add var labels
    add_labels <- function(data, labels) {
      for (i in 1:nrow(labels)) {
        var_name <- labels$varname[i]
        var_label <- labels$label[i]
        if (var_name %in% names(data)) {
          var_labelled <- labelled::set_variable_labels(data[[var_name]], var_label)
          data[[var_name]] <- var_labelled
        }
      }
      return(data)
    }
    survey_weather <- add_labels(survey_weather, varlist)
      
    return(survey_weather)
  })
  
  haz_vars <- reactive({
    paste0("haz_",weather_vars())
  })

  #----------------------------------------------------------------------------#
  # WEATHER STATS OUTPUT
  #----------------------------------------------------------------------------#

  # Reactive values to track if weather stats tab added
  weather_tab_added <- reactiveVal(FALSE)

  # Observer for "Weather stats" button
  observeEvent(input$weather_stats,
    {
      req(input$weather_stats > 0) # Ensure the button has been clicked
      
      # Weather distribution plots
      
      ############################################################################
      # TODO: Plots can be made dynamic based on number of selected weather variables
      ############################################################################
        
      output$weather_dist1 <- renderPlot({
        req(survey_weather(), haz_vars())
        
        p <- ggplot(survey_weather(), aes(
          x = .data[[haz_vars()[1]]],
          y = countryyear,
          fill = code
        )) +
          geom_density_ridges(alpha = 0.7, scale = 2)
        label <- filter(varlist, varname==haz_vars()[1], !is.na(varname)) |> pull(label)
        p + theme_minimal() +
          labs(
            title = "", x = paste0(label,"\n (as configured)"),
            y = "", fill = ""
          ) +
          theme(legend.position = "none")
      })
      
      output$weather_dist2 <- renderPlot({
        req(length(input$weather_variable_selector) > 1, survey_weather(), haz_vars())
        
        p <- ggplot(survey_weather(), aes(
          x = .data[[haz_vars()[2]]],
          y = countryyear,
          fill = code
        )) +
          geom_density_ridges(alpha = 0.7, scale = 2)
        label <- filter(varlist, varname==haz_vars()[2], !is.na(varname)) |> pull(label)
        p + theme_minimal() +
          labs(
            title = "", x = str_wrap(paste0(label,"\n (as configured)"), 40),
            y = "", fill = ""
          ) +
          theme(legend.position = "none")
      })
      
      # weather maps
      
      # binned scatter plots
      output$binscatter1 <- renderPlot({
        req(survey_weather(),haz_vars())
        
        p <- ggplot(survey_weather(), aes(x = .data[[haz_vars()[1]]],
                            y = log_welf)) +
          geom_point(alpha = 0.1) +
          stat_summary_bin(fun.y='mean', bins=20,
                           color='orange', size=2, geom='point')
        
        xlabel <- filter(varlist, varname==haz_vars()[1], !is.na(varname)) |> pull(label)
        
        p + theme_minimal() +
          labs(
            title = "", x = str_wrap(paste0(xlabel,"\n (as configured)"), 40),
            y = "Log welfare", fill = ""
          ) 
      })
      
      output$binscatter2 <- renderPlot({
        req(length(input$weather_variable_selector) > 1,survey_weather(),haz_vars())
        
        p <- ggplot(survey_weather(), aes(x = .data[[haz_vars()[2]]],
                            y = log_welf)) +
          geom_point(alpha = 0.1) +
          stat_summary_bin(fun.y='mean', bins=20,
                           color='orange', size=2, geom='point')
        
        xlabel <- filter(varlist, varname==haz_vars()[2], !is.na(varname)) |> pull(label)
        
        p + theme_minimal() +
          labs(
            title = "", x = str_wrap(paste0(xlabel,"\n (as configured)"), 40),
            y = "Log welfare", fill = ""
          ) 
      })
      
      # Add "Weather stats" tab if not already added
      if (!weather_tab_added()) {
        appendTab(
          inputId = "step1_output_tabs",
          tabPanel("Weather stats",
            value = "weather_desc",
            h4("Distribution of weather (household survey sample)"),
            layout_columns(
              col_widths = c(6, 6),
              card(
                plotOutput("weather_dist1", height = "300px")
              ),
              card(
                plotOutput("weather_dist2", height = "300px")
              )
            ),
            br(),
            
            h4("Weather over time and space"),
            helpText("Weather maps to be added..."),
          br(),
          
          h4("Welfare vs weather"),
          layout_columns(
            col_widths = c(6, 6),
            card(
              plotOutput("binscatter1", height = "300px")
            ),
            card(
              plotOutput("binscatter2", height = "300px")
            ),
          ),
          br(),
          
            h4("Weather summary stats"),
            helpText("Summary statistics are shown for the configured weather variables. Sample weights are used."),
            output$weather_stats <- renderDT(
              {
                req(survey_weather(), haz_vars())
                
                sumtable(survey_weather(),
                         vars = haz_vars(),
                         summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", 
                                  "min(x)", "max(x)", "notNA(x)"),
                         summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
                         group = "countryyear",
                         group.long = TRUE,
                         group.weights = "weight",
                         labels = TRUE,
                         out = "return"
                )
              },
              rownames = FALSE
            ),
          br(),
        
          ############################################################################
          # TODO: Weather variable list to be shown before weather variables is chosen (to aid selection)
          ############################################################################  
          
          h4("Weather variable list"),
          output$weather_stats <- renderDT({
            req(survey_weather(), haz_vars())
            weather_list
            }, rownames = FALSE
          ),
          ),
          select = TRUE # Select this tab when it's first added
        )
      }
      weather_tab_added(TRUE)
      # If the "Weather stats" tab is present (either just added or previously added), ensure it's selected
      if (weather_tab_added()) {
        updateTabsetPanel(session, "step1_output_tabs", selected = "weather_desc")
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  #------------------------------------------------------------------------------#
  # MODEL UI
  #------------------------------------------------------------------------------#

  hh_varlist <- reactive({
    req(input$country)
    filter(varlist, wiseapp == "HH characteristics" & datatype %in% c("Numeric","Binary")) |>
    dplyr::select(varname, label) |> filter(varname %in% colnames(survey_weather()))
  })
  
  area_varlist <- reactive({
    req(input$country)
    filter(varlist, wiseapp == "Area characteristics") |>
    dplyr::select(varname, label) |> filter(varname %in% colnames(survey_weather()))
  })
  
  fe_varlist <- reactive({
    req(input$country)
    fe_exclude <- c("countryname", "survname","hhid","psu","strata")
    filter(varlist, wiseapp == "ID & Fixed effects") |>
    dplyr::select(varname, label) |> filter(varname %in% colnames(survey_weather()) & !varname %in% fe_exclude)
  })

output$model_specs_ui <- renderUI({
  req(input$country, input$weather_variable_selector)

  conditionalPanel(
    condition = "input.model_covariates % 2 == 1",
    withMathJax(),
    tagList(

      ## static widgets shown for both model types -------------------------
      selectizeInput(
        "interactions",
        label   = "Interactions with \\(Haz_{kt}\\):",
        choices = hh_varlist() |>
                  dplyr::filter(varname %in%
                      c("urban","literacy","primarycomp","imp_wat_rec",
                        "imp_san_rec","electricity","agriland","male")) |>
                  dplyr::pull(label),
        selected = "Urban",
        multiple = TRUE,
        options  = list(maxItems = 1)
      ),

      selectizeInput(
        "fixedeffects",
        label   = "Fixed effects",
        choices = fe_varlist()$label,
        selected = fe_varlist() |>
                   dplyr::filter(varname == "year") |>
                   dplyr::pull(label),
        multiple = TRUE
      ),

      radioButtons(
        "modelspec",
        "Choose model type for covariate selection:",
        choices  = c("Linear regression", "Lasso"),
        selected = isolate(input$modelspec) %||% "Linear regression"
      ),

      helpText("XGBoost option will be added soon."),

      ## widgets that depend on the chosen model ---------------------------
      uiOutput("model_specific_inputs")   # rendered below
    )
  )
})

# second renderUI produces only the model-specific widgets
output$model_specific_inputs <- renderUI({
  req(input$modelspec)

  withMathJax(               # ⟵ one wrapper is enough
    if (input$modelspec == "Linear regression") {

      tagList(
        selectizeInput(
          "hhcov",
          label   = "Household characteristics \\(X_{hkt}\\)",
          choices = hh_varlist()$label,
          selected = hh_varlist() |>
                     dplyr::filter(varname %in% c("urban", "hhsize")) |>
                     dplyr::pull(label),
          multiple = TRUE
        ),
        selectizeInput(
          "areacov",
          label   = "Area characteristics \\(E_{kt}\\)",
          choices = area_varlist()$label,
          selected = area_varlist() |>
                     dplyr::filter(varname == "built_area") |>
                     dplyr::pull(label),
          multiple = TRUE
        )
      )

    } else if (input$modelspec == "Lasso") {

      helpText("No manual covariate selection needed: Lasso decides automatically.")
      helpText("Automatically selected variables will be shown in model results.")

    }
  )
})

  #------------------------------------------------------------------------------#
  # MODEL OUTPUTS
  #------------------------------------------------------------------------------#

  # Reactive values to track if model-related tabs have been added
  model_results_tab_added <- reactiveVal(FALSE)
  model_fit_tab_added <- reactiveVal(FALSE)

  # Observer for "Run model" button
  observeEvent(input$run_model,
    {
      req(input$run_model > 0) # Ensure the button has been clicked

      # Run model
      model_fit <- reactive({
      
        # welfare outcome
        if (welf_select()$type == "Continuous"){ out <- "log_welf"
        } else {out <- "poor"}
        
        # weather variables
        weather_vars <- dplyr::select(survey_weather(), starts_with("haz")) |> colnames()
        
        # add polynomials if specified
        for (w in weather_vars){
          id_prefix <- paste0(sub("haz_", "", w), "_")
          poly_input <- input[[paste0(id_prefix, "polynomial")]] %||% ""
          
          if(!is.null(poly_input) && length(poly_input) > 0) {
            poly_labels <- c("a" = "Quadratic", "b" = "Cubic")
            polynomials <- paste(poly_labels[poly_input], collapse = ", ")
            
            if (grepl("Quadratic", polynomials, fixed=TRUE)){
              weather_vars <- c(weather_vars, paste0("I(",w,"^2)"))
            }
            if (grepl("Cubic", polynomials, fixed=TRUE)){
              weather_vars <- c(weather_vars, paste0("I(",w,"^3)"))
            }
          }
        }

        # interaction terms
        interactions <- filter(varlist, label %in% input$interactions) |> pull(varname)
        if (length(interactions) > 0){
          term_matrix <- outer(interactions,weather_vars, 
                                FUN = function(inter, weather) {
                                  sprintf("(%s * %s)", inter, weather)
                                  })
          term_vector <- c(t(term_matrix))
          interaction_terms <- paste0(" + ", paste(term_vector, collapse = " + "))
          
        } else {interaction_terms <- " "}

        # fixed effects
        fe <- filter(varlist, label %in% input$fixedeffects) |> pull(varname) 
        # if (input$fixedeffects_interact==TRUE){ 
        #   fe <- paste0("(", paste(fe, collapse = " * "),")")
        #   }

        if (input$modelspec == "Linear regression"){
        
          # survey variables
          hh_cov <- filter(varlist, label %in% input$hhcov) |> pull(varname)
          area_cov <- filter(varlist, label %in% input$areacov) |> pull(varname)

          # construct formulas
          main_effects <- paste(c(weather_vars, hh_cov, area_cov, fe), collapse = " + ")

        } else if (input$modelspec == "Lasso") {

          ## survey variables ------------------------------------------------
          hh_cov   <- intersect(hh_varlist()$varname,   colnames(survey_weather()))
          area_cov <- intersect(area_varlist()$varname, colnames(survey_weather()))

          ## residuals from the weather/interactions/FE model ------------
          f_resid <- as.formula(
            paste(out, "~",
                  paste(c(weather_vars, interaction_terms, fe), collapse = " + "))
          )
          fit_resid <- if (out == "poor") glm(f_resid, data = survey_weather(),
                                              family = binomial())
                      else                lm(f_resid, data = survey_weather())

          # Drop any columns that have a proportion of NA greater than specified threshold
          X_ctrl  <- model.matrix(                           # household + area only
                        ~ -1 + .
                        , data = survey_weather()[ , c(hh_cov,
                                                       area_cov)]
                      )
          y_res   <- residuals(fit_resid, type = "response") # residuals from the model
          # drop rows in y_res that do not have corresponding rows in X_ctrl,
          # because of the model.matrix, which drops any rows with NA in any column

          cn <- colnames(X_ctrl)                    # design-matrix column names
          grp_ctrl <- ifelse(cn %in% hh_cov,   1L,   # household block
                            ifelse(cn %in% area_cov, 2L,  NA_integer_))

          ## cross-validated group-lasso ------------------------------
          cv_ctrl <- cv.grpreg(X_ctrl, y_res,
                              group  = grp_ctrl,
                              family = if (out == "poor") "gaussian" else "gaussian", # residuals are continuous always
                              seed   = 42)

          keep <- rownames(coef(cv_ctrl, s = "lambda.min"))[-1]   # drop intercept
          keep <- keep[coef(cv_ctrl, s = "lambda.min")[-1] != 0]

          main_effects <- paste(c(weather_vars, keep, fe), collapse = " + ")

        } # else if (input$modelspec == "XGBoost"){
        #   
        # }
        
        formula1 <- as.formula(paste(out, "~", paste(weather_vars, collapse = " + ")))
        formula2 <- as.formula(paste(out, "~", paste(c(weather_vars, fe), collapse = " + ")))
        formula3 <- as.formula(paste(out, "~", main_effects, interaction_terms))

        # helper that returns the right fit function ------------------------
        fit_fun <- if (out == "poor") {
          \(form) glm(form, data = survey_weather(), family = binomial())
        } else {
          \(form) lm(form,  data = survey_weather())
        }
        
        # generalised linear model fits
        fit1 <- fit_fun(formula1)
        fit2 <- fit_fun(formula2)
        fit3 <- fit_fun(formula3)
        
        model_fit <- list(fit1, fit2, fit3)  

      return(model_fit)
    }
  )
      
      # labelling
      label_lookup <- setNames(varlist$label, varlist$varname)
      labels_df <- filter(varlist, !is.na(varname)) |> dplyr::select(varname, label) 
      
      get_term_label <- function(term, labels_df) {
        if (grepl("^I\\((.*)\\^2\\)$", term)) {
          varname <- sub("^I\\((.*)\\^2\\)$", "\\1", term)
          base_label <- labels_df$label[labels_df$varname == varname]
          return(paste0(base_label, "^2"))
        } else if (grepl("^I\\((.*)\\^3\\)$", term)) {
          varname <- sub("^I\\((.*)\\^3\\)$", "\\1", term)
          base_label <- labels_df$label[labels_df$varname == varname]
          return(paste0(base_label, "^3"))
        } else {
          return(labels_df$label[labels_df$varname == term])
        }
      }
      create_named_vector <- function(coefs_to_plot, labels_df) {
        named_vector <- sapply(coefs_to_plot, function(coef) {
          sub_terms <- unlist(strsplit(coef, ":"))
          term_labels <- sapply(sub_terms, get_term_label, labels_df = labels_df, USE.NAMES = FALSE)
          final_label <- paste(term_labels, collapse = " * ")
          return(final_label)
        })
        final_named_vector <- setNames(names(named_vector), named_vector)
        return(final_named_vector)
      }
      
      outlab <- reactive({
        if (welf_select()$type == "Continuous"){var_label(survey_data()$log_welf)
        } else {var_label(survey_data()$poor)}
      })
        
      # regression table
      output$regtable <- renderUI({
        req(model_fit(), outlab())
        coefs <- names(coef(model_fit()[[3]]))
        weather_vars <- grep("haz", coefs, value = TRUE) 
        hh_cov <- filter(varlist, label %in% input$hhcov) |> pull(varname)
        area_cov <- filter(varlist, label %in% input$areacov) |> pull(varname)
        coefs_to_plot <- c(weather_vars, hh_cov, area_cov)
        named_coefs <- create_named_vector(coefs_to_plot, labels_df)
        
        ht <- export_summs(model_fit()[[1]], model_fit()[[2]], model_fit()[[3]], 
                     robust = "HC3",
                     model.names = c("No FE", "FE", "FE + controls"),
                     coefs = named_coefs, digits = 3)
        HTML(huxtable::to_html(ht))
      })
      
      # effect size plot
      output$coefplot <- renderPlot({
        req(model_fit(), outlab())
        coefs <- names(coef(model_fit()[[3]]))
        coefs_to_plot <- grep("haz", coefs, value = TRUE)
        named_coefs <- create_named_vector(coefs_to_plot, labels_df)
        
        ############################################################################
        # TODO: This is where we'd need to specify for OLS how many of the specifications need to be plotted
        #       e.g. if only FE model is specified, then only plot that one.
        ############################################################################
      
        plot_summs(model_fit()[[1]], model_fit()[[2]], model_fit()[[3]],
                   robust = "HC3", coefs = named_coefs,
                   model.names = c("No FE", "FE", "FE + controls")) +
          scale_y_discrete(labels = label_wrap(20)) +
          labs(x = str_wrap(paste0("Effect on ", outlab()),50))
      })
      
      # simple effects plot
      output$effectplot1 <- renderPlot({
        req(model_fit(), haz_vars(), outlab())
        effect_plot(model_fit()[[3]], pred = !!haz_vars()[1], 
                    interval = TRUE, plot.points = FALSE, line.colors = "orange",
                    x.label = str_wrap(label_lookup[haz_vars()[1]], 40), 
                    y.label = str_wrap(outlab(),40))
      })
      
      # simple effects plot
      output$effectplot2 <- renderPlot({
        req(length(input$weather_variable_selector) > 1, model_fit(), haz_vars(), outlab())
        effect_plot(model_fit()[[3]], pred = !!haz_vars()[2], 
                    interval = TRUE, plot.points = FALSE, line.colors = "orange",
                    x.label = str_wrap(label_lookup[haz_vars()[2]], 40),
                    y.label = str_wrap(outlab(),40))
      })
       # interactions plots
      
      mod <- reactive(filter(varlist, label %in% input$interactions) |> pull(varname))
      
      output$interactplot1 <- renderPlot({
        req(length(input$interactions) > 0, model_fit(), haz_vars(), outlab())
        interact_plot(model_fit()[[3]], pred = !!haz_vars()[1], modx = !!mod(),
                      interval = TRUE, plot.points = FALSE,
                      x.label = str_wrap(label_lookup[haz_vars()[1]], 40), 
                      y.label = str_wrap(outlab(),40)) +
          theme(legend.position = "bottom")
      })
      
      output$interactplot2 <- renderPlot({
        req(length(input$interactions) > 0, 
            length(input$weather_variable_selector) > 1, model_fit(), haz_vars(), outlab())
        interact_plot(model_fit()[[3]], pred = !!haz_vars()[2], modx = !!mod(),
                      interval = TRUE, plot.points = FALSE,
                      x.label = str_wrap(label_lookup[haz_vars()[2]], 40), 
                      y.label = str_wrap(paste0(outlab()),40)) +
          theme(legend.position = "bottom")
      })
      
      output$simslopes1 <- renderPlot({
        req(length(input$interactions) > 0, model_fit(), haz_vars())
        plot(sim_slopes(model_fit()[[3]], pred = !!haz_vars()[1], modx = !!mod()))
      })
      
      output$simslopes2 <- renderPlot({
        req(length(input$interactions) > 0, 
            length(input$weather_variable_selector) > 1, model_fit(), haz_vars())
        plot(sim_slopes(model_fit()[[3]], pred = !!haz_vars()[2], modx = !!mod()))
      })
        
      # Add "Results" tab if not already added
      if (!model_results_tab_added()) {
        appendTab(
          inputId = "step1_output_tabs",
          tabPanel("Results",
            value = "results",
            h4("Marginal effect of weather on welfare"),
            card(
              plotOutput("coefplot")
            ),
            br(),
            h4("Predicted welfare vs weather"),
            layout_columns(
              col_widths = c(6, 6),
              card(
                plotOutput("effectplot1", height = "300px")
              ),
              card(
                plotOutput("effectplot2", height = "300px")
              )
            ),
            br(),
            h4("Interactions & adaptation"),
            output$interactplots1 <- renderUI({
              if (length(input$interactions) > 0 & length(input$weather_variable_selector) > 1){
                tagList(
                  layout_columns(
                    col_widths = c(6, 6),
                    card(plotOutput("interactplot1", height = "300px")),
                    card(plotOutput("simslopes1", height = "300px")),
                    card(plotOutput("interactplot2", height = "300px")),
                    card(plotOutput("simslopes2", height = "300px")),
                    )
                  )
              } else if (length(input$interactions) > 0){
                tagList(
                  layout_columns(
                    col_widths = c(6, 6),
                    card(plotOutput("interactplot1", height = "300px")),
                    card(plotOutput("simslopes1", height = "300px")),
                  )
                )
              } else {output$no_interactions <- renderText({
                  "No interaction term specified."
                })
              }
            }),
            br(),
            h4("Regression results"),
            tableOutput("regtable"),
            br(),
            # h4("Features to add:"),
            # helpText("Some outputs depend on method and whether outcome is binary/continuous"),
            # h6("Plot more damage functions..."),
            # p("∆ Predicted poverty rate/gap/total welfare/gini vs weather (with CIs, by interaction)"),
            # h6("Plot marginal effect of interaction terms on welfare outcome vs weather?"),
            # p("Print the regression formula"),
            # br(),
          ),
          select = TRUE # Select this tab when it's first added
        )
        model_results_tab_added(TRUE)
      }

      # Add "Model fit" tab if not already added
      if (!model_fit_tab_added()) {
        
        # # Residuals vs weather plots
        output$resid_weather1 <- renderPlot({
          req(model_fit())
          model <- model_fit()[[3]]
          
          plot_data <- model.frame(model) |>
            dplyr::select(starts_with("haz_")) |>
            mutate(residuals = residuals(model))
          
          h <- colnames(dplyr::select(plot_data, starts_with("haz_")))[1]
          xlabel <- filter(varlist, varname==h, !is.na(varname)) |> pull(label)
          
          ggplot(plot_data, aes(x = .data[[h]],
                                y = residuals)) +
            geom_point(alpha = 0.1) +
            geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
            stat_summary_bin(fun.y='mean', bins=20,
                             color='orange', size=2, geom='point') + 
            theme_minimal() +
            labs(title = "", x = str_wrap(paste0(xlabel,"\n (as configured)"), 40),
                 y = "Residuals", fill = ""
            ) 
          })
        output$resid_weather2 <- renderPlot({
          req(length(input$weather_variable_selector) > 1, model_fit())
          
          model <- model_fit()[[3]]
          plot_data <- model.frame(model) |>
            dplyr::select(starts_with("haz_")) |>
            mutate(residuals = residuals(model))
          
          h <- colnames(dplyr::select(plot_data, starts_with("haz_")))[2]
          xlabel <- filter(varlist, varname==h, !is.na(varname)) |> pull(label)
          
          ggplot(plot_data, aes(x = .data[[h]],
                                     y = residuals)) +
            geom_point(alpha = 0.1) +
            geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
            stat_summary_bin(fun.y='mean', bins=20,
                             color='orange', size=2, geom='point') + 
            theme_minimal() +
            labs(title = "", x = str_wrap(paste0(xlabel,"\n (as configured)"), 40),
              y = "Residuals", fill = ""
            ) 
        })
          
        # Predicted welfare distribution
        output$pred_welf_dist <- renderPlot({
          req(model_fit(), outlab())
          
          model <- model_fit()[[3]]
          actual_values <- model.frame(model)[[1]] 
          predicted_values <- predict(model)
          if (welf_select()$type == "Binary") {
            predicted_values <- round(predicted_values) # If pr(poor) > 0.5 >> poor
          }
          
          plot_data <- data.frame(
            Type = rep(c("Survey", "Predicted"), each = length(actual_values)),
            Values = c(actual_values, predicted_values)
          )
          
          ggplot(plot_data, aes(x = Values, fill = Type)) +
            geom_histogram(aes(y = 100*..count../sum(..count..)),
                           position = "dodge", alpha = 0.7) +
            labs(
              x = str_wrap(outlab(),40),
              y = "Share of households (%)"
            ) +
            theme_minimal() +
            scale_fill_manual(values = c("Survey" = "steelblue", "Predicted" = "orange"))
          
        })
        
        # Relative contribution of variables to model fit
        output$relaimpo <- renderPlot({
          req(model_fit())
          
          model <- model_fit()[[3]]
          total_r2 <- summary(model)$r.squared
          rel_importance <- relaimpo::calc.relimp(model, type = "lmg")
          
          importance_df <- data.frame(
            Variable = names(rel_importance$lmg),
            Contribution = rel_importance$lmg
          ) |>
            left_join(dplyr::select(varlist, varname, label), 
                      join_by("Variable" == "varname")) |>
            mutate(label = if_else(is.na(label),Variable,label))
          
          ggplot(importance_df, 
                 aes(x = reorder(label, Contribution), 
                     y = Contribution, fill = Variable)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = round(Contribution, 3)), hjust = -0.3, size = 4) +
            geom_hline(yintercept = total_r2, linetype = "dashed", color = "red") +
            annotate("text", y = Inf, x = total_r2, 
                     label = paste("Total R² =", round(total_r2, 3)), 
                     hjust = 1.1, vjust = -0.5, color = "red") +
            coord_flip() + # Flip the coordinates for horizontal bars
            labs(
              x = "",
              y = "R-squared Contribution"
            ) +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_discrete(labels = label_wrap(20))
          
        })
        
        # Output: Model summary
        output$model_summary <- renderPrint({
          req(model_fit())
          summary(model_fit()[[3]])
        })
        
        # Output: Diagnostic plots
        output$diagnostic_plots <- renderPlot({
          req(model_fit())
          par(mfrow = c(2, 2))  # Set up a 2x2 plot layout
          plot(model_fit()[[3]])
        })
        
        # Output: Additional statistics
        output$additional_stats <- renderTable({
          req(model_fit())
          model <- model_fit()[[3]]
          data.frame(
            Stat = c("Observations", "Adjusted R-squared", "F-statistic"),
            Value = c(format(round(nobs(model)),big.mark = ","), 
                      round(summary(model)$adj.r.squared,3),
                      round(summary(model)$fstatistic[1]))
          )
        })
        appendTab(
          inputId = "step1_output_tabs",
          tabPanel("Model fit",
            value = "model_fit",
            
            h4("Fit statistics"),
            p("(Model with FE and controls)"),
            tableOutput("additional_stats"),
            
            h4("Residuals vs weather"),
            layout_columns(
              col_widths = c(6, 6),
              card(
                plotOutput("resid_weather1", height = "300px")
              ),
              card(
                plotOutput("resid_weather2", height = "300px")
              )
            ),
            
            h4("Contribution of Each Variable to Model R-squared"),
            plotOutput("relaimpo"),
            helpText("Using the Lindeman, Merenda, and Gold (LMG) method, which calculates the average increase in R-squared when a predictor is added to the model across all possible orderings of predictors."),
            
            h4("Predicted welfare"),
            plotOutput("pred_welf_dist"),
            
            h4("Diagnostic plots"),
            plotOutput("diagnostic_plots"),
            
            h4("Model summary"),
            verbatimTextOutput("model_summary"),
            
            br()
            # h4("Features to add:"),
            # helpText("Model fit diagnostics probably need to depend on whether outcome is binary of continuous, and on method selected"),
            # p("SHAP for XGBoost method"),
            # p("Plot correlation matrix (all variables used by model)"),
            # br(),
          )
        )
        model_fit_tab_added(TRUE)
      }
      
      # perform welfare simulations
      welf_sim <- reactive({
        req(model_fit())
        
        # merge weather data for different years
        sim_data <- augment(model_fit()[[3]], newdata = survey_weather()) |>
          filter(!is.na(.fitted)) |>
          mutate(month = month(timestamp)) |>
          dplyr::select(code, year, survname, loc_id, hhid, weight, .resid,
                 any_of(colnames(model.frame(model_fit()[[3]]))), -timestamp, 
                 -starts_with("haz"), -any_of(c('log_welf', 'welf'))) |>
          left_join(mutate(loc_weather(), 
                           month = month(timestamp), 
                           year = as.factor(year))) |>
          mutate(sim_year = year(timestamp),
                 error = .resid)
        
        # get fitted values
        welf_sim <- augment(model_fit()[[3]], newdata = sim_data) |>
          mutate(welf_pred = .fitted + error) # add error term
        
        return(welf_sim)
      })
      
      sim_weather <- reactive({
        mod_weather <- model.frame(model_fit()[[3]]) |>
          dplyr::select(starts_with("haz_")) |> 
          mutate(type = "Model fit")
        hist_weather <- welf_sim() |>
          dplyr::select(starts_with("haz_")) |> 
          mutate(type = "Historical")
        sim_weather <- bind_rows(mod_weather, hist_weather)
        return(sim_weather)
      })
      
      output$sim_weather1 <- renderPlot({
        req(model_fit())
        
        xlabel <- filter(varlist, varname==haz_vars()[1], !is.na(varname)) |> pull(label)
        ggplot(sim_weather(), aes(x = .data[[haz_vars()[1]]], fill = type)) +
          geom_density(alpha = 0.5) +
          labs(
            x = str_wrap(xlabel,40),
            y = "Density"
          ) +
          theme_minimal() +
          labs(fill = "") +
          theme(legend.position = "top")
      })
      
      output$sim_weather2 <- renderPlot({
        req(model_fit(), length(input$weather_variable_selector)>1)
        
        xlabel <- filter(varlist, varname==haz_vars()[2], !is.na(varname)) |> pull(label)
        ggplot(sim_weather(), aes(x = .data[[haz_vars()[2]]], fill = type)) +
          geom_density(alpha = 0.5) +
          labs(
            x = str_wrap(xlabel,40),
            y = "Density"
          ) +
          theme_minimal() +
          labs(fill = "") +
          theme(legend.position = "top")
      })
      
      output$sim_pov3_ep <- renderPlot({
        req(input$run_sim > 0, model_fit())
        
        if (welf_select()$varname =="welf_ppp_2021" & welf_select()$type == "Continuous"){
          plot_data <- welf_sim() |>
            summarise(pov300 = weighted.mean(welf_pred < log(3),weight),
                      .by = c(code, year, sim_year)) |>
            group_by(code, year) |>
            mutate(pov300d = pov300 - mean(pov300, na.rm = TRUE)) |>
            ungroup()
          
          ggplot(plot_data, aes(x = 100*pov300d)) +
            stat_ecdf(geom = "point", aes(y = 1 - ..y..), color = "lightblue") +
            stat_ecdf(geom = "line", aes(y = 1 - ..y..), color = "red", linewidth = 1) +
            geom_vline(xintercept = 0, color = "black", linetype = "dotted") +
            labs(
              x = "Change in $3.00 poverty rate (pp.)",
              y = "Annual Exceedance Probability (P(pov > y))",
              caption = "Change in poverty is relative to the mean (expected) poverty \n  over all simulated weather years.") +
            theme_minimal() +
            coord_flip()  
          
        } else NULL
      })
      
      output$sim_pov8_ep <- renderPlot({
        req(input$run_sim > 0, model_fit())
        
        if (welf_select()$varname =="welf_ppp_2021" & welf_select()$type == "Continuous"){
          plot_data <- welf_sim() |>
            summarise(pov830 = weighted.mean(welf_pred < log(8.3),weight), 
                      .by = c(code, year, sim_year)) |>
            group_by(code, year) |>
            mutate(pov830d = pov830 - mean(pov830, na.rm = TRUE)) |>
            ungroup()
          
          ggplot(plot_data, aes(x = 100*pov830d)) +
            stat_ecdf(geom = "point", aes(y = 1 - ..y..), color = "lightblue") +
            stat_ecdf(geom = "line", aes(y = 1 - ..y..), color = "red", linewidth = 1) +
            geom_vline(xintercept = 0, color = "black", linetype = "dotted") +
            labs(
              x = "Change in $8.30 poverty rate (pp.)",
              y = "Annual Exceedance Probability (P(pov > y))",
              caption = "Change in poverty is relative to the mean (expected) poverty \n  over all simulated weather years.") +
            theme_minimal() +
            coord_flip() 
          
        } else NULL
      })
      # output$sim_gini_ep <- renderPlot({
      #   req(input$run_sim > 0, model_fit())
      #     plot_data <- welf_sim() |>
      #       summarise(gini = Gini(welf_pred, weights = weight),
      #                 .by = c(code, year, survname, sim_year))
      # 
      #     ggplot(plot_data, aes(x = gini)) +
      #       stat_ecdf(geom = "step", pad = FALSE) +
      #       labs(
      #         x = "Gini",
      #         y = "Annual Exceedance Probability (P(pov > y))"
      #       ) +
      #       theme_minimal()
# 
#       })

      
      welf_sim_policy <- reactive({
        req(welf_sim(), input$run_policy_sim > 0)
        
      sim_policy <- welf_sim() 
      
      if (!is.null(input$policy_edu)){
        if (input$policy_edu == "Every household has at least primary education"){
          sim_policy <- mutate(sim_policy, primarycomp = 1)
        } 
      }
      if (!is.null(input$policy_infra)){
        if ("Every household has access to electricity in the dwelling" %in% input$policy_infra){
          sim_policy <- mutate(sim_policy, electricity = 1)
        }
        if ("Every household has access to improved drinking water" %in% input$policy_infra){
          sim_policy <- mutate(sim_policy, imp_wat_rec = 1)
        }
        if ("Every household has access to improved sanitation" %in% input$policy_infra){
          sim_policy <- mutate(sim_policy, imp_san_rec = 1)
        }
      }
      
      sim_policy <- augment(model_fit()[[3]], newdata = sim_policy) |>
        mutate(welf_pred_pol = .fitted + error) # add error term
      
      return(sim_policy)
    })
      
      output$sim_pov3_pol_ep <- renderPlot({
        req(input$run_sim > 0, model_fit(), welf_sim_policy())
        
        if (welf_select()$varname =="welf_ppp_2021" & welf_select()$type == "Continuous"){
          plot_data <- welf_sim_policy() |>
            summarise(pov300 = weighted.mean(welf_pred < log(3),weight), 
                      pov300_pol = weighted.mean(welf_pred_pol < log(3),weight), 
                      .by = c(code, year, sim_year)) |>
            group_by(code, year) |>
            mutate(pov300d = pov300 - mean(pov300, na.rm = TRUE),
                   pov300d_pol = pov300_pol - mean(pov300_pol, na.rm = TRUE)) |>
            ungroup()
          
          ggplot(plot_data) +
            stat_ecdf(geom = "point", 
                      aes(x = 100*pov300d_pol, y = 1 - ..y..), 
                      color = "lightblue") +
            stat_ecdf(geom = "line", 
                      aes(x = 100*pov300d_pol, y = 1 - ..y.., color = "policy"), 
                      linewidth = 1) +
            stat_ecdf(geom = "point", 
                      aes(x = 100*pov300d, y = 1 - ..y..), 
                      color = "lightblue") +
            stat_ecdf(geom = "line", 
                      aes(x = 100*pov300d, y = 1 - ..y.., color = "baseline"), 
                      linewidth = 1) +
            scale_color_manual(values = c("baseline" = "red", "policy" = "blue"), 
                               labels = c("Baseline","Policy")) +
            geom_vline(xintercept = 0, color = "black", linetype = "dotted") +
            labs(x = "Change in $3.00 poverty rate (pp.)",
                 y = "Annual Exceedance Probability (P(pov > y))",
                 color = "",
                 caption = "Change in poverty rate is relative to the mean (expected) \n poverty over all simulated weather years.") +
            coord_flip() + 
            theme_minimal() +
            theme(legend.position = "top")
          
        } else NULL
      })  
      
      
      # If the "Results" tab is present (either just added or previously added), ensure it's selected
      if (model_results_tab_added()) {
        updateTabsetPanel(session, "step1_output_tabs", selected = "results")
      }
    },
    ignoreInit = TRUE
  ) # ignoreInit for actionButtons

  #----------------------------------------------------------------------------#
  # Simulate welfare across the distribution of weather
  #----------------------------------------------------------------------------#
  

}
