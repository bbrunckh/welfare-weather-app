# Define the User Interface (UI)
ui <- navbarPage(
  tagList(
  "WISE-APP",
  tags$small(app_version, style = "color: #777777; margin-top: 0px; font-size: 0.5em;")
  ),
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML",
                type = "text/javascript")
  ),
  # Page 1: Overview
  tabPanel(
    "Overview",
    fluidPage(
      withMathJax(), # Initialize MathJax to render LaTeX equations
      
      h2("Welcome to the WISE-APP"),
      h5("Weather Impact Simulation and Evaluation for Adaptation Policy and Planning"),
      p("This tool is designed to help you understand the relationship between welfare and weather."),
      p("Follow the steps to perform simulations and explore policy impacts."),
    )
  ),
  
  # Page 2: Step 1 - Welfare and the weather
  tabPanel(
    "Step 1 - Model welfare",
    fluidPage(
      h4("How much does weather affect welfare? Who is most affected?"),
    sidebarLayout(
      sidebarPanel(
        bs_accordion(id = "step1_sidebar_accordion") |>
          bs_append(
            title = "1 Define sample",
            content = tagList(
              selectizeInput(
                inputId = "country",
                label = "Country",
                choices = survey_list$countryname,
                multiple = TRUE,
                options = list(maxItems = 3, placeholder = "Select country")
              ),
              uiOutput("survey_year_ui"), # Survey years - conditional on country selection
              radioButtons(
                inputId = "sample_type",
                label = "Sample",
                choices = c("All households", "Rural households", "Urban households"),
                selected = "All households"
              )
            )
          ) |>
          bs_append(
            title = "2 Define welfare outcome",
            content = tagList(
              selectizeInput(
                inputId = "welfare_outcome",
                label = "Welfare (\\(W_{hkt}\\))",
                choices = outcomes,
                selected = "Log welfare per day (PPP)"
              ),
              uiOutput("welfare_ui"), # Welfare year and poverty line - conditional
              helpText("Continous outcomes ($/day) are log transformed. Binary outcomes (Poor...) define the poverty status of a household."),
              
              actionButton("survey_stats", "Survey stats")
            )
          ) |>
          bs_append(
            title = "3 Define weather variable",
            content = tagList(
              selectizeInput(
                inputId = "weather_variable_selector",
                label = "Weather (\\(Haz_{kt}\\))",
                choices = weather_list$name,
                selected = "Monthly temperature",
                multiple = TRUE,
                options = list(maxItems = 2)
              ),
              uiOutput("weather_construction_ui"), # Conditional on weather variable
            )
          ) |>
          bs_append(
            title = "4 Specify model",
            content = tagList(
              radioButtons("modelspec", 
                           "Model:", 
                           # choices = c("Linear regression", "Lasso", "XGBoost")), 
                           choices = c("Linear regression")), 
              
              helpText("Lasso and XGBoost options will be added soon."),
              actionButton("model_covariates", "Define covariates"), # Changed label for clarity
              uiOutput("model_specs_ui"), # Conditional on survey data
              hr(),
              actionButton("run_model", "Run model"),
              hr(),
              helpText("Linear regression (OLS) will include all specified covariates."),
              helpText("Lasso will use the Post-Double Selection Lasso method to select the specified covariates that best predict the weather variable and the welfare outcome. The selected variables are then used in a linear regression (OLS)."),
              helpText("XGBoost will use the extreme gradient boosting method, building an ensemble of decision trees considering all specified covariates. SHAP (SHapley Additive exPlanations) are used to interpret the results. They explain how much each variable contributes to the predicted outcome.")
            )
          )
      ), 
      mainPanel(
        
          # Main panel now contains a tabsetPanel for outputs
          tabsetPanel(id = "step1_output_tabs"),
          h4("Welfare function"),
          h3("\\(W_{hkt} = f(Haz_{kt}, X_{hkt}, E_{kt}) + \\epsilon_{hkt}\\)"),
          p("\\(W_{hkt}\\): welfare of household \\(h\\) in location \\(k\\) at time \\(t\\)"),
          p("\\(Haz_{kt}\\): weather conditions in location \\(k\\) at time \\(t\\)"),
          p("\\(X_{hkt}\\): characteristics of household \\(h\\) in location \\(k\\) at time \\(t\\)"),
          p("\\(E_{kt}\\): characteristics of location \\(k\\) at time \\(t\\)"),
          p("\\(\\epsilon_{hkt}\\): error term"),
        )
    )
    )
  ),
  
  # Page 3: Step 2 - Welfare-weather simulations
  tabPanel(
    "Step 2 - Simulate welfare",
    fluidPage(
      h4("What welfare is expected given historical weather conditions?"),
      sidebarLayout(
        sidebarPanel(
          bs_accordion(id = "step2_sidebar_accordion") |>
            bs_append(
              title = "Simulation settings",
              content = tagList(
          sliderInput("yearRange", 
                      "Years defining the distribution of weather", 
                      min = 1950, 
                      max = 2024, 
                      value = c(1990, 2024),
                      sep = ""
                      ),
          # radioButtons("outofsample", 
          #             "Predict welfare when weather is outside the range used to fit the model", 
          #             choices = c("Yes", 
          #                         "No, use upper and lower bounds of weather in sample",
          #                         "No"),
          #             selected = "Yes"
          #             ),
          hr(),
          actionButton("run_sim", "Run simulation")
          )
          )
          ),
        mainPanel(
          h4("Predicted welfare"),
          h3("\\(\\widehat{W_{hkt}} = f(Haz_{kt}, \\widehat{X_{hkt}}, \\widehat{E_{kt}}) + \\widehat{\\epsilon_{hkt}}\\)"),
          br(),
          h4("Historical weather distribution vs weather used to fit model"),
          layout_columns(
            col_widths = c(6, 6),
            card(
              plotOutput("sim_weather1", height = "300px"),
            ),
            card(
              plotOutput("sim_weather2", height = "300px"),
            )
          ),
          br(),
          h4("Poverty rate  vs probability"),
          p("(conditional on the selected weather variable)"),
          layout_columns(
            col_widths = c(6, 6),
            card(
              plotOutput("sim_pov3_ep", height = "300px"),
            ),
            card(
              plotOutput("sim_pov8_ep", height = "300px"),
            )
          ),
          br(),
          hr(),
          h3("Under development - Step 2 will be updated soon."),
          p("Map showing weather over time used in simulation (animate?)"),
          p("Histogram/ridgeplot of weather distributions over time (linked to map) for (1) survey sample, (2) population (3) weather distribution used to train model"),
          p("Histogram/ridgeplot of predicted welfare distribution over time (linked to map) for (1) survey sample, (2) population (survey weights) (3) actual welfare distribution used to train model"),
          h6("Exceedance probability curves"),
          p("1. ∆ Poverty rate vs probability"),
          p("2. ∆ Poverty gap vs probability"),
          p("3. ∆ Total welfare vs probability"),
          p("4. ∆ Gini vs probability"),
          p("5. ∆ Prosperity gap vs probability"),
          h6("CDFs ... vs cumulative probability?"),
          h6("'vulnerability to poverty'"),
          p("Households with probability of being poor > X")
        )
      )
    )
  ),
  
  # Page 4: Step 3 - Policy and climate scenarios
  tabPanel(
    "Step 3 - Policy & climate scenarios",
    fluidPage(
      h4("What welfare is expected in alternate climate and policy scenarios?"),
      sidebarLayout(
        sidebarPanel(
          bs_accordion(id = "step3_sidebar_accordion") |>
            bs_append(
              title = "Policy scenario",
              content = tagList(
                checkboxGroupInput("policy_edu", 
                             "Education scenario", 
                             choices = c("Every household has at least primary education")
                ),
                hr(),
                checkboxGroupInput("policy_infra", 
                                   "Infrastructure scenario", 
                                   choices = c("Every household has access to electricity in the dwelling", 
                                               "Every household has access to improved drinking water source", 
                                               "Every household has access to improved sanitation")
                ),
                hr(),
                h4("Yet to be implemented"),
                h5("Adaptive SP - cash transfer"),
                # radioButtons("trigger_type", 
                #              "Trigger type", 
                #              choices = c("Modelled welfare loss", 
                #                          "Modelled increase in poverty gap", 
                #                          "Modelled increase in poverty rate", 
                #                          "Hazard intensity",  
                #                          "Hazard frequency"),
                #              select = "Hazard frequency"
                # ),
                # radioButtons("trigger", 
                #              "Trigger", 
                #              choices = c("1-5 year (historical) event", 
                #                          "1-10 year (historical) event", 
                #                          "1-20 year (historical) event"),
                #              selected = "1-10 year (historical) event"
                # ),
                # radioButtons("cash_value", 
                #              "Total value of cash transfers", 
                #              choices = c("Share of modelled welfare loss",
                #                          "Share of modelled increase in poverty gap",
                #                          "Fixed budget"),
                #              selected = "Share of modelled increase in poverty gap"
                # ),
                # radioButtons("cash_targeting", 
                #              "Targeting of cash transfers", 
                #              choices = c("Universal (fixed amount)",
                #                          "Poor (fixed amount)",
                #                          "Poor (proportional to welfare)",
                #                          "Poor (proportional to welfare loss)"),
                #              selected = "Universal (fixed amount)"
                # ),
                hr(),
                actionButton("run_sim", "Run policy simulation")
              )
            ) |>
            bs_append(
              title = "Climate scenario",
              content = tagList(
                h4("Yet to be implemented"),
                radioButtons("climate", 
                             "Climate", 
                             choices = c("2050 SSP2-4.5", 
                                         "2050 SSP5-8.5"),
                             selected = "2050 SSP5-8.5"
                ),
                radioButtons("climatemethod", 
                             "Method", 
                             choices = c("Delta"),
                             selected = "Delta"
                ),
                helpText("Delta: adds CMIP6 ensemble 'delta' fields to historical observations."),
                hr(),
                actionButton("run_sim", "Run climate simulation")
              )
            ) 
        ),
        mainPanel(
          h3("Under development - Step 3 will be updated soon."),
          # p("Map showing difference in weather between climate change scenario and historical climate used in simulation (animate?)"),
          # p("Histogram/ridgeplot of weather distributions over time (linked to map) for (1) historical climate (sample), (2) climate change scenario (sample) (3) weather distribution used to train model"),
          # p("Histogram/ridgeplot of predicted welfare distribution over time (linked to map) for (1) policy baseline, historical climate, (2) policy baseline, climate scenario, (3) policy scenario, historical climate, (4) policy scenario, climate scenario, (5) actual welfare distribution used to train model"),
          # h6("Exceedance probability curves (each scenario vs baseline)"),
          # p("1. ∆ Poverty rate vs probability"),
          # p("2. ∆ Poverty gap vs probability"),
          # p("3. ∆ Total welfare vs probability"),
          # p("4. ∆ Gini vs probability"),
          # p("5. ∆ Prosperity gap vs probability"),
          # h6("CDFs ... vs cumulative probability? (each scenario vs baseline)"),
          # h6("'vulnerability to poverty' (each scenario vs baseline)"),
          # p("Households with probability of being poor > X")
        )
      )
    )
  )
)
