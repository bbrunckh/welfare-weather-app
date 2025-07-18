# Check datalibweb subscriptions to GMD data

check_gmd_access <- function(dlw_token) {

    # Define base url
    base_url <- "https://datalibwebapiprod.ase.worldbank.org/dlw/api/v1/"
    
    # Define the endpoint for the request
    endpoint <- "SubscriptionRequest"
    
    # Build and perform the HTTP request, 
    response <- httr2::request(base_url) |>
      httr2::req_url_path_append(endpoint) |>
      httr2::req_auth_bearer_token(dlw_token) |>
      httr2::req_url_query(
        PageIndex = "1",
        PageSize = "3000",
        SearchAll = "Data",
        SearchColumn = "collection",
        SearchValue = "GMD"
      ) |>
      httr2::req_perform()
  
    # Process the response
    subscriptions_data <- response |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON()
    
    # Extract, filter, and select the required data
    subscriptions <- subscriptions_data$subscriptions |>
      dplyr::filter(region == "GMD") |>
      dplyr::select(country, year, surveyName, collection, classification) |>
      dplyr::mutate(code_year = paste0(country, "_", year))
    
    surveys <- survey_list_master |>
      dplyr::filter(wiseapp_pin %in% subscriptions$code_year)
  
  # Return the final data frame
  return(surveys)
  
}