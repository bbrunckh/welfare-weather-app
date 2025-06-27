source("global.R")

# Define the Server logic
server <- function(input, output, session) {
  #----------------------------------------------------------------------------#
  # SAMPLE UI
  #----------------------------------------------------------------------------#

  # Get available survey years for selected countries
  available_years <- reactive({
    req(input$country)
    selected_countries <- input$country
    years_list <- list()

    for (country in selected_countries) {
      years_list[[country]] <- survey_list[survey_list$countryname == country, "year"]
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

  #------------------------------------------------------------------------------#
  # LOAD SURVEY DATA
  #------------------------------------------------------------------------------#

  # Get survey data file paths for selected sample
  survey_data_files <- reactive({
    req(input$country)
    selected_countries <- input$country
    survey_files <- c()
    for (country in selected_countries) {
      year_input_id <- paste0("survey_year_", gsub(" ", "_", country))
      selected_years <- input[[year_input_id]]

      if (!is.null(selected_years) && length(selected_years) > 0) {
        country_data <- survey_list |>
          filter(countryname == country & year %in% selected_years) |>
          pull(filename)
        survey_files <- c(survey_files, country_data)
      }
    }
    return(paste0("data/surveys/", as.list(as.character(survey_files))))
  })
  
  # load survey data for selected sample
  survey_data <- reactive({
    
    # read relevant parquet files (lazily)
    req(input$country)
    survey_data <- read_parquet_duckdb(survey_data_files(),
                      options = list(union_by_name = TRUE)) 
    
    # filter by sample type
  if (input$sample_type == "Rural households"){
    survey_data <- filter(survey_data, urban ==0)
  } else if (input$sample_type == "Rural households"){
    survey_data <- filter(survey_data, urban ==1)
  }
    # logical data columns > integer, categorical data columns > factors
    
    # add var labels
    
  return(survey_data)
  })

  # Loads survey interview location data
  survey_geo <- reactive({
    req(input$country)
    files <- sub("\\.parquet$", "_LOC.gpkg", survey_data_files())
    do.call(rbind, lapply(files, st_read)) |>
      filter(case_when(
        input$sample_type == "All households" ~ !is.na(loc_id),
        input$sample_type == "Rural households" ~ urban == 0,
        input$sample_type == "Urban households" ~ urban == 1
      ))
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
        paste0("$/day (PPP, ", input$ppp_year, ")"),
        paste0("LCU/day (", input$lcu_year, ")")
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
          plot1 <- survey_data() |>
            collect() |>
            mutate(month = floor_date(int_date, "month")) |>
            summarise(hh = n(), .by = c(countryname, code, year, month))

          ggplot(plot1, aes(
            x = month,
            y = hh,
            fill = countryname
          )) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(
              title = "", x = "",
              y = "Number of households", fill = ""
            )
        })

        # interview location map
        output$map <- renderLeaflet({
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
          # summarise data
          welfare <- welf_select()

          plot2 <- survey_data() |>
            filter(welfare != 0, !is.na(welfare)) |>
            select(countryname, code, year, any_of(welfare$varname), weight) |>
            mutate(
              across(starts_with("welf"), ~ log(.x),
                .names = "log_welf"
              ),
              countryyear = paste0(countryname, ", ", year)
            ) |>
            collect()

          # Create plot
          p <- ggplot(plot2, aes(
            x = log_welf,
            y = countryyear,
            fill = code
          )) +
            geom_density_ridges(alpha = 0.7, scale = 2)

          # Add PPP poverty lines
          if (welfare$pre == "$") {
            povln <- pov_lines[pov_lines$ppp_year == welfare$year, "ln"]
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
            log_pline <- log(welfare$pline)
            p <- p + geom_vline(xintercept = log_pline, color = "darkred") +
              annotate("text",
                x = log_pline - 0.1, y = 1,
                label = paste0(welfare$pre, sprintf("%.2f", welfare$pline), "/day"),
                angle = 90, size = 4, color = "darkred"
              )
          }

          p + theme_minimal() +
            labs(
              title = "", x = paste0("Log ", welfare$label),
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
            output$data_table <- renderDT(
              {
                welfare <- welf_select()

                welf_desc <- survey_data() |>
                  select(
                    countryname, code, year, weight,
                    welf_ppp_2021,
                    poor_300ln, poor_420ln, poor_830ln,
                    welf_lcu_2021,
                    any_of(welfare$varname)
                  ) |>
                  mutate(
                    across(
                      c(poor_300ln, poor_420ln, poor_830ln),
                      ~ as.integer(.x)
                    ),
                    Survey = paste0(countryname, ", ", year)
                  ) |>
                  collect()

                lookup_vec <- setNames(varlist$label, varlist$varname)
                current_names <- colnames(welf_desc)
                names_to_change <- intersect(colnames(welf_desc), names(lookup_vec))
                match_indices <- match(names_to_change, colnames(welf_desc))
                var_label(welf_desc)[match_indices] <- lookup_vec[names_to_change]

                sumtable(welf_desc,
                  vars = grep("welf|poor",
                    colnames(welf_desc),
                    value = TRUE,
                    ignore.case = TRUE
                  ),
                  summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", "min(x)", "max(x)", "notNA(x)"),
                  summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
                  group = "Survey",
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
                categorical_cols <- filter(varlist, !is.na(wiseapp) & datatype == "Categorical") |>
                  pull("varname")
                binary_cols <- filter(varlist, !is.na(wiseapp) & datatype == "Binary") |>
                  pull("varname")

                hh_desc <- survey_data() |>
                  select(
                    countryname, code, year, weight,
                    any_of(varlist[varlist$wiseapp == "HH characteristics" & !is.na(varlist$wiseapp), "varname"])
                  ) |>
                  mutate(Survey = paste0(countryname, ", ", year)) |>
                  collect() |>
                  mutate(
                    across(where(is.logical), as.integer),
                    across(any_of(categorical_cols), as.factor)
                  )

                lookup_vec <- setNames(varlist$label, varlist$varname)
                current_names <- colnames(hh_desc)
                names_to_change <- intersect(colnames(hh_desc), names(lookup_vec))
                match_indices <- match(names_to_change, colnames(hh_desc))
                var_label(hh_desc)[match_indices] <- lookup_vec[names_to_change]

                sumtable(hh_desc,
                  vars = colnames(hh_desc)[6:length(hh_desc) - 1],
                  summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", "min(x)", "max(x)", "notNA(x)"),
                  summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
                  group = "Survey",
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
                area_desc <- survey_data() |>
                  select(
                    countryname, code, year, weight,
                    any_of(varlist[varlist$wiseapp == "Area characteristics" & !is.na(varlist$wiseapp), "varname"])
                  ) |>
                  mutate(Survey = paste0(countryname, ", ", year)) |>
                  collect()

                lookup_vec <- setNames(varlist$label, varlist$varname)
                current_names <- colnames(area_desc)
                names_to_change <- intersect(colnames(area_desc), names(lookup_vec))
                match_indices <- match(names_to_change, colnames(area_desc))
                var_label(area_desc)[match_indices] <- lookup_vec[names_to_change]

                sumtable(area_desc,
                  vars = colnames(area_desc)[6:length(area_desc) - 1],
                  summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", 
                           "min(x)", "max(x)", "notNA(x)"),
                  summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
                  group = "Survey",
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
    req(input$weather_variable_selector) # Ensure at least one variable is selected

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
    files <- sub("\\.parquet$", "_H3.parquet", survey_data_files())
    read_parquet_duckdb(files, options = list(union_by_name = TRUE),
                        prudence = "lavish") |>
      mutate(loc_id = as.character(loc_id))
  })
  
  # selected weather variables
  weather_vars <- reactive({
    req(input$weather_variable_selector, input$country)
    weather_vars <- weather_list |>
      filter(name %in% input$weather_variable_selector) |>
      pull(varname)
    return(weather_vars)
  })
  
  # load selected weather variables for survey locations (lazily)
  weather_data <- reactive({
    req(input$weather_variable_selector, input$country, input$survey_stats)
    
    # lookup weather data files
    codes <- filter(survey_list, countryname %in% input$country) |>
      pull(code) |> unique()
    files <- paste0("data/weather/",codes,"_weather.parquet")
    
    # selected weather variables
    weather_vars <- weather_vars()
    
    # sample locations
    survey_h3 <- survey_h3()
    
    # dates for weather vars (use 1991-2020 as default)
    survey_dates <- survey_data() |> pull(int_date) |> na.omit()
    survey_date_min <- floor_date(min(survey_dates), 
                                  "month") - months(12)
    survey_date_max <- floor_date(max(survey_dates), "month")
    
    weather_dates <- seq(min(survey_date_min, as.Date("1990-12-01")- months(12)),
                         max(survey_date_max, as.Date("2020-12-01")), 
                         by = "1 month")
    # get data
    weather <- read_parquet_duckdb(files, options = list(union_by_name = TRUE),
                                   prudence = "lavish") 
    
    # filter to selected weather variables, sample h3 cells & date range
    weather <- weather |>
      select(h3_6, timestamp, all_of(weather_vars)) |>
      filter(h3_6 %in% survey_h3$h3_6) |>
      filter(timestamp %in% weather_dates) |>
      distinct() 
    
    return(weather)
  })
  
  # configure weather variables 
  weather_timeseries <- reactive({
    
    weather_vars <- weather_vars()
    
    # loop over selected weather variables
    for (i in weather_vars){
      
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
        rowwise() |>
        mutate(haz = median(c_across(starts_with(paste0(i,"_"))))) |> 
        ungroup()
    }
    if (temporal_agg == "Min") {
      weather <- weather |>
        rowwise() |>
        mutate(haz = min(c_across(starts_with(paste0(i,"_"))))) |> 
        ungroup()
    }
    if (temporal_agg == "Max") {
      weather <- weather |>
        rowwise() |>
        mutate(haz = max(c_across(starts_with(paste0(i,"_"))))) |> 
        ungroup()
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
    
    # Binned !!Not implemented yet
    if (cont_binned == "Binned"){
    }
    
    # keep only the configured weather variable
    weather <- weather |>
      select(h3_6, timestamp, haz) |>
      rename_with(~ paste0("haz_",i), .cols = starts_with("haz")) |>
      arrange(h3_6, timestamp) 
    
    if (i == weather_vars[1]){weather_timeseries <- weather } else{
      weather_timeseries <- full_join(weather_timeseries, weather)}
    }
    return(weather_timeseries)
  })
  
  # Reactive expression that matches survey data to weather data
  survey_weather <- reactive({
    req(input$weather_variable_selector, input$country)
    
    survey_h3 <- survey_h3()
    survey_data <- survey_data()
    weather_timeseries <- weather_timeseries()
    
    survey_dates <- survey_data |>
      distinct(code, year, loc_id, int_date) |>
      filter(!is.na(loc_id) & loc_id !="" & !is.na(int_date)) |>
      mutate(timestamp = floor_date(int_date, "month")) 
    
    loc_weather <- survey_h3 |>
      left_join(survey_dates) |>
      left_join(weather_timeseries) |>
      summarise(across(starts_with("haz"), ~sum(.x*pop_2020)/sum(pop_2020)),
                .by = c(code, year, survname, loc_id, int_date)) 
    
    cats <- filter(varlist, !is.na(wiseapp) & datatype == "Categorical") |>
      pull("varname")
    fe <- filter(varlist, !is.na(wiseapp) & wiseapp == "ID & Fixed effects") |>
      pull("varname")
    welfare <- welf_select()$varname
    
    survey_weather <- loc_weather |>
      left_join(survey_data) |>
      mutate(year = as.integer(year),
             countryyear = paste0(countryname, ", ", year),
             log_welf = log(.data[[welfare]]))  |>
      collect() |>
      filter(countryname!="") |>
      mutate(across(where(is.logical), as.integer),
             across(any_of(cats), as.factor),
             across(any_of(fe), as.factor))
    
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
        
      output$weather_dist1 <- renderPlot({
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
        req(length(input$weather_variable_selector) > 1)
        
        p <- ggplot(survey_weather(), aes(
          x = .data[[haz_vars()[2]]],
          y = countryyear,
          fill = code
        )) +
          geom_density_ridges(alpha = 0.7, scale = 2)
        label <- filter(varlist, varname==haz_vars()[2], !is.na(varname)) |> pull(label)
        p + theme_minimal() +
          labs(
            title = "", x = paste0(label,"\n (as configured)"),
            y = "", fill = ""
          ) +
          theme(legend.position = "none")
      })
      
      # weather maps
      
      # binned scatter plots
      output$binscatter1 <- renderPlot({
        p <- ggplot(survey_weather(), aes(x = .data[[haz_vars()[1]]],
                            y = log_welf)) +
          geom_point(alpha = 0.1) +
          stat_summary_bin(fun.y='mean', bins=20,
                           color='orange', size=2, geom='point')
        
        xlabel <- filter(varlist, varname==haz_vars()[1], !is.na(varname)) |> pull(label)
        
        p + theme_minimal() +
          labs(
            title = "", x = paste0(xlabel,"\n (as configured)"),
            y = "Log welfare", fill = ""
          ) 
      })
      
      output$binscatter2 <- renderPlot({
        req(length(input$weather_variable_selector) > 1)
        
        p <- ggplot(survey_weather(), aes(x = .data[[haz_vars()[2]]],
                            y = log_welf)) +
          geom_point(alpha = 0.1) +
          stat_summary_bin(fun.y='mean', bins=20,
                           color='orange', size=2, geom='point')
        
        xlabel <- filter(varlist, varname==haz_vars()[2], !is.na(varname)) |> pull(label)
        
        p + theme_minimal() +
          labs(
            title = "", x = paste0(xlabel,"\n (as configured)"),
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
                weather_desc <- survey_weather() |>
                  select(countryyear, weight, starts_with("haz"))
                lookup_vec <- setNames(varlist$label, varlist$varname)
                current_names <- colnames(weather_desc)
                names_to_change <- intersect(colnames(weather_desc), names(lookup_vec))
                match_indices <- match(names_to_change, colnames(weather_desc))
                var_label(weather_desc)[match_indices] <- lookup_vec[names_to_change]
                
                sumtable(weather_desc,
                         vars = colnames(select(weather_desc, starts_with("haz"))),
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
    filter(varlist, wiseapp == "HH characteristics") |>
    select(varname, label) |> filter(varname %in% colnames(survey_data()))
  })
  
  area_varlist <- reactive({
    req(input$country)
    filter(varlist, wiseapp == "Area characteristics") |>
    select(varname, label) |> filter(varname %in% colnames(survey_data()))
  })
  
  fe_varlist <- reactive({
    req(input$country)
    filter(varlist, wiseapp == "ID & Fixed effects") |>
    select(varname, label) |> filter(varname %in% colnames(survey_data()) & !varname %in% c("countryname", "survname","hhid"))
  })

output$model_specs_ui <- renderUI({
  req(input$country, input$weather_variable_selector)
  
  hh_vars <- hh_varlist()$label
  
  hh_vars_default <- hh_varlist() |>
    filter(varname %in% c("urban", "hhsize", "primarycomp")) |>
    pull(label)

  area_vars <- area_varlist()$label
  area_vars_default <- area_varlist() |>
    filter(varname %in% c("built_area")) |>
    pull(label)
  
  interactions <- hh_varlist() |> filter(
    varname %in% c("urban", "literacy", "primarycomp", "imp_wat_rec", 
                   "imp_san_rec", "electricity", "agriland", "male")) |>
    pull(label)
  
  interactions_default <- c()
  
  fe_vars <- fe_varlist()$label
  fe_vars_default <- fe_varlist() |>
    filter(varname %in% c("year", "subnatid1"))|>
    pull(label)
  
  conditionalPanel(
    condition = "input.model_covariates % 2 == 1", 
    tagList(           
    
    selectizeInput(
      inputId = "hhcov",
      label = "Household characteristics (\\(X_{hkt}\\))",
      choices = hh_vars,
      selected = hh_vars_default,
      multiple = TRUE
    ),
    selectizeInput(
      inputId = "areacov",
      label = "Area characteristics (\\(E_{kt}\\))",
      choices = area_vars,
      selected = area_vars_default,
      multiple = TRUE
    ),
    selectizeInput(
      inputId = "interactions",
      label = "Interactions  with \\(Haz_{kt}\\):",
      choices = interactions,
      selected = interactions_default,
      multiple = TRUE,
      options = list(maxItems = 1)
    ),
    selectizeInput(
      inputId = "fixedeffects",
      label = "Fixed effects",
      choices = fe_vars,
      selected = fe_vars_default,
      multiple = TRUE
    ),
    checkboxInput( 
      inputId = "fixedeffects_interact", 
      label = "Interact fixed effects",
      value = FALSE
    ),
    
    helpText("Yet to be implemented..."),
    checkboxGroupInput( 
      inputId = "checkbox_group", 
      label = "Robustness checks", 
      choices = c( 
        "Include lagged weather" = "a", 
        "Include displaced weather" = "b",
        "Include future weather (placebo test)" = "c"
        ) 
      )
    )
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
        
      # if (input$modelspec == "Linear regression"){
      
        # welfare outcome
        out <- "log_welf"
        
        # weather variables
        weather_vars <- select(survey_weather(), starts_with("haz")) |> colnames()
        
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
        
        # survey variables
        hh_cov <- filter(varlist, label %in% input$hhcov) |> pull(varname)
        area_cov <- filter(varlist, label %in% input$areacov) |> pull(varname)
        interactions <- filter(varlist, label %in% input$interactions) |> pull(varname)
        
        # fixed effects - interacted
        fe <- filter(varlist, label %in% input$fixedeffects) |> pull(varname) 
        if (input$fixedeffects_interact==TRUE){ 
          fe <- paste0("(", paste(fe, collapse = " * "),")")
          }
        
        # construct formulas
        main_effects <- paste(c(weather_vars, hh_cov, area_cov, fe), collapse = " + ")
        
        if (length(interactions) > 0){
          term_matrix <- outer(interactions,weather_vars, 
                               FUN = function(inter, weather) {
                                 sprintf("(%s * %s)", inter, weather)
                                 })
          term_vector <- c(t(term_matrix))
          interaction_terms <- paste0(" + ", paste(term_vector, collapse = " + "))
          
        } else {interaction_terms <- " "}
        
        formula1 <- as.formula(paste(out, "~", paste(weather_vars, collapse = " + ")))
        formula2 <- as.formula(paste(out, "~", paste(c(weather_vars, fe), collapse = " + ")))
        formula3 <- as.formula(paste(out, "~", main_effects,interaction_terms))
        
        # linear model fits
        fit1 <- lm(formula1, survey_weather(), weight = weight)
        fit2 <- lm(formula2, survey_weather(), weights = weight)
        fit3 <- lm(formula3, survey_weather(), weights = weight)
        
        model_fit <- list(fit1, fit2, fit3)
        
          # } else if (input$modelspec == "Lasso"){
          #   
          # } else if (input$modelspec == "XGBoost"){
          #   
          # }
        return(model_fit)
        })
      
      # labelling
      label_lookup <- setNames(varlist$label, varlist$varname)
      labels_df <- filter(varlist, !is.na(varname)) |> select(varname, label) 
      
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
      
      # regression table
      output$regtable <- renderUI({
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
        coefs <- names(coef(model_fit()[[3]]))
        coefs_to_plot <- grep("haz", coefs, value = TRUE)
        named_coefs <- create_named_vector(coefs_to_plot, labels_df)
      
        plot_summs(model_fit()[[1]], model_fit()[[2]], model_fit()[[3]],
                   robust = "HC3", coefs = named_coefs,
                   model.names = c("No FE", "FE", "FE + controls"))
      })
      
      # simple effects plot
      output$effectplot1 <- renderPlot({
        effect_plot(model_fit()[[3]], pred = !!haz_vars()[1], 
                    interval = TRUE, plot.points = FALSE, line.colors = "orange",
                    x.label = label_lookup[haz_vars()[1]], y.label = "Log welfare")
      })
      
      # simple effects plot
      output$effectplot2 <- renderPlot({
        req(length(input$weather_variable_selector) > 1)
        effect_plot(model_fit()[[3]], pred = !!haz_vars()[2], 
                    interval = TRUE, plot.points = FALSE, line.colors = "orange",
                    x.label = label_lookup[haz_vars()[2]],y.label = "Log welfare"
        )
      })
       # interactions plots
      
      mod <- filter(varlist, label %in% input$interactions) |> pull(varname)
      
      output$interactplot1 <- renderPlot({
        req(length(input$interactions) > 0)
        interact_plot(model_fit()[[3]], pred = !!haz_vars()[1], modx = !!mod,
                      interval = TRUE, plot.points = FALSE,
                      x.label = label_lookup[haz_vars()[1]], 
                      y.label = "Log welfare") +
          theme(legend.position = "bottom")
      })
      
      output$interactplot2 <- renderPlot({
        req(length(input$interactions) > 0, 
            length(input$weather_variable_selector) > 1)
        interact_plot(model_fit()[[3]], pred = !!haz_vars()[2], modx = !!mod,
                      interval = TRUE, plot.points = FALSE,
                      x.label = label_lookup[haz_vars()[2]], 
                      y.label = "Log welfare") +
          theme(legend.position = "bottom")
      })
      
      output$simslopes1 <- renderPlot({
        req(length(input$interactions) > 0)
        plot(sim_slopes(model_fit()[[3]], pred = !!haz_vars()[1], modx = !!mod))
      })
      
      output$simslopes2 <- renderPlot({
        req(length(input$interactions) > 0, 
            length(input$weather_variable_selector) > 1)
        plot(sim_slopes(model_fit()[[3]], pred = !!haz_vars()[2], modx = !!mod))
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
              if (length(input$interactions) > 0){
                tagList(
                  layout_columns(
                    col_widths = c(6, 6),
                    card(plotOutput("interactplot1", height = "300px")),
                    card(plotOutput("interactplot2", height = "300px")),
                    card(plotOutput("simslopes1", height = "300px")),
                    card(plotOutput("simslopes2", height = "300px")),
                    )
                  )
              } else {
                output$no_interactions <- renderText({
                  "No interaction term specified."
                })
              }
            }),
            br(),
            h4("Regression results"),
            tableOutput("regtable"),
            br(),
            h4("Features to add:"),
            helpText("Some outputs depend on method and whether outcome is binary/continuous"),
            h6("Plot more damage functions..."),
            p("∆ Predicted poverty rate/gap/total welfare/gini vs weather (with CIs, by interaction)"),
            h6("Plot marginal effect of interaction terms on welfare outcome vs weather?"),
            p("Print the regression formula"),
            br(),
          ),
          select = TRUE # Select this tab when it's first added
        )
        model_results_tab_added(TRUE)
      }

      # Add "Model fit" tab if not already added
      if (!model_fit_tab_added()) {
        
        # # Residuals vs weather plots
        output$resid_weather1 <- renderPlot({
          model <- model_fit()[[3]]
          
          plot_data <- model.frame(model) |>
            select(starts_with("haz_")) |>
            mutate(residuals = residuals(model))
          
          h <- colnames(select(plot_data, starts_with("haz_")))[1]
          xlabel <- filter(varlist, varname==h, !is.na(varname)) |> pull(label)
          
          ggplot(plot_data, aes(x = .data[[h]],
                                y = residuals)) +
            geom_point(alpha = 0.1) +
            geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
            stat_summary_bin(fun.y='mean', bins=20,
                             color='orange', size=2, geom='point') + 
            theme_minimal() +
            labs(title = "", x = paste0(xlabel,"\n (as configured)"),
                 y = "Residuals", fill = ""
            ) 
          })
        output$resid_weather2 <- renderPlot({
          req(length(input$weather_variable_selector) > 1)
          
          model <- model_fit()[[3]]
          plot_data <- model.frame(model) |>
            select(starts_with("haz_")) |>
            mutate(residuals = residuals(model))
          
          h <- colnames(select(plot_data, starts_with("haz_")))[2]
          xlabel <- filter(varlist, varname==h, !is.na(varname)) |> pull(label)
          
          ggplot(plot_data, aes(x = .data[[h]],
                                     y = residuals)) +
            geom_point(alpha = 0.1) +
            geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
            stat_summary_bin(fun.y='mean', bins=20,
                             color='orange', size=2, geom='point') + 
            theme_minimal() +
            labs(title = "", x = paste0(xlabel,"\n (as configured)"),
              y = "Residuals", fill = ""
            ) 
        })
          
        # Predicted welfare distribution
        output$pred_welf_dist <- renderPlot({
          
          model <- model_fit()[[3]]
          actual_values <- model.frame(model)[[1]] 
          predicted_values <- predict(model)
          
          plot_data <- data.frame(
            Type = rep(c("Survey", "Predicted"), each = length(actual_values)),
            Values = c(actual_values, predicted_values)
          )
          
          ggplot(plot_data, aes(x = Values, fill = Type)) +
            geom_density(alpha = 0.5) +
            labs(
              x = "Log welfare",
              y = "Density"
            ) +
            theme_minimal() +
            scale_fill_manual(values = c("Survey" = "steelblue", "Predicted" = "orange"))
          
        })
        
        # Relative contribution of variables to model fit
        output$relaimpo <- renderPlot({
          
          model <- model_fit()[[3]]
          total_r2 <- summary(model)$r.squared
          rel_importance <- relaimpo::calc.relimp(model, type = "lmg")
          
          importance_df <- data.frame(
            Variable = names(rel_importance$lmg),
            Contribution = rel_importance$lmg
          ) |>
            left_join(select(varlist, varname, label), 
                      join_by("Variable" == "varname"))
          
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
            theme(legend.position = "none")
          
        })
        
        # Output: Model summary
        output$model_summary <- renderPrint({
          summary(model_fit()[[3]])
        })
        
        # Output: Diagnostic plots
        output$diagnostic_plots <- renderPlot({
          model <- model_fit()[[3]]
          par(mfrow = c(2, 2))  # Set up a 2x2 plot layout
          plot(model)
        })
        
        # Output: Residuals vs Fitted plot
        output$residuals_fitted <- renderPlot({
          model <- model_fit()[[3]]
          plot(model, which = 1)  # Residuals vs Fitted
        })
        
        # Output: Additional statistics
        output$additional_stats <- renderTable({
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
            
            h4("Predicted welfare distribution"),
            plotOutput("pred_welf_dist"),
            
            h4("Residuals vs Fitted plot"),
            plotOutput("residuals_fitted"),
            
            h4("Model summary"),
            verbatimTextOutput("model_summary"),
            
            h4("Diagnostic plots"),
            plotOutput("diagnostic_plots"),
            
            br(),
            h4("Features to add:"),
            helpText("Model fit diagnostics probably need to depend on whether outcome is binary of continuous, and on method selected"),
            p("SHAP for XGBoost method"),
            p("Plot correlation matrix (all variables used by model)"),
            br(),
          )
        )
        model_fit_tab_added(TRUE)
      }

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
  
  # prepare weather data
  
}
