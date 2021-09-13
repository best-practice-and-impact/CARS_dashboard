#'@title Ingest department data
#'
#'@description get department data from the smartcurvey API. 
#'
#'@return department column from CARS wave 3 data (character).
#'

ingest_department_data <- function() {
  
  export_id <- get_latest_export_id(get_export_list())  
  data <- convert_raw(ingest(export = export_id))
  
  # Discard top 2 rows as these are blank
  departments <- data$Q1..What.is.your.civil.service.grade..if.applicable..[3:length(data$Q1..What.is.your.civil.service.grade..if.applicable..)]
  
  return(departments)
}

#'@title Ingest smartsurvey data
#'
#'@description Download smartsurvey export via the API. Download the exported data from smartsurvey using the API. Use convert_raw() to convert the API response to a data.frame.
#'
#'@param survey the survey ID (character string/numeric). Defaults to "790800".
#'@param export the export ID (character string/numeric).
#'@param token the API token (character string). Loaded from environment variable by default.
#'@param secret the secret API token (character string). Loaded from environment variable by default.
#'
#'@return the exported data as a dataframe
#'
#'@export

ingest <- function(survey = "961613",
                   export,
                   token = Sys.getenv("CARS_TOKEN"),
                   secret = Sys.getenv("CARS_SECRET")) {
  
  # Check input types
  if (!is.character(survey) && !is.numeric(survey) | !is.character(export) && !is.numeric(export)) {
    stop("Unexpected input - survey and export should lbe character or numeric variables.")
  }
  
  if (!is.character(token) | !is.character(secret)) {
    stop("Unexpected input - token and secret should be character variables.")
  }
  
  if (length(survey) > 1 | length(token) > 1 | length(token) > 1 | length(secret) > 1 ) {
    stop("Unexpected input - one or more of the supplied arguments contain multiple elements.")
  }
  
  # API request
  url <- paste0("https://api.smartsurvey.io/v1/surveys/", survey, "/exports/", export, "/download")
  
  query_string <- list(
    api_token = token,
    api_token_secret = secret
  )
  
  tryCatch(
    {
      r <- httr::GET(
        url, 
        query = query_string
      )    
    },
    error = function(e) {
      stop(paste("Error in API request: ", e))
    }
  )
  
  # Check request status code
  if (r$status_code != 200) {
    stop(paste0("Unsuccessful API request. Status code: ", r$status_code))
    return(r)
  }
  
  return(r)
}

#'@title Convert raw data to data.frame
#'
#'@description Convert raw smartsurvey data to data.frame . Extract contents (raw csv) from smartsurvey API request and convert to data.frame
#'
#'@param r api response object
#'
#'@return response content as a data.frame
#'
#'@export

convert_raw <- function(r) {
  
  if (class(r) != "response") {
    stop("Unexpected input - r is not a response object.")
  } else if (r$status_code != 200) {
    stop("Unsuccessful API request - no data.")
  }
  
  content <- rawToChar(r$content)
  
  data <- utils::read.table(
    text = content, 
    sep = ",", 
    header = TRUE, 
    fill = TRUE, 
    quote = "\"\"",
    na.strings = c("", ".", "NA", "-", "\"\"", "\".\"", "\"NA\"", "\"-\"")
  )
  
  return(data)
}

#'@title get list of exports
#'#'
#'@description retrieve a list of data exports of the CARS wave 3 data
#'
#'@param survey survey number (string)
#'@param token API token. This should be stored as an environment variable and NOT kept in this repository. The function will retrieve the token from the environment variable.
#'@param secret API secret. This should be stored as an environment variable and NOT kept in this repository. The function will retrieve the token from the environment variable.
#'
#'@return API request
#'

get_export_list <- function(survey = "961613", token = Sys.getenv("CARS_TOKEN"), secret = Sys.getenv("CARS_SECRET")) {
  
  # API request
  url <- paste0("https://api.smartsurvey.io/v1/surveys/", survey, "/exports/") 
  
  query_string <- list(
    api_token = token,
    api_token_secret = secret
  )
  
  tryCatch(
    {
      r <- httr::GET(
        url, 
        query = query_string
      )    
    },
    error = function(e) {
      stop(paste("Error in API request: ", e))
    }
  )
  
  return(r)
}

#'@title get latest export ID
#'
#'@description Retrieve the latest export's ID from the export list
#'
#'@param r API request object returned by get_export_list
#'
#'@return export ID (string)
#'

get_latest_export_id <- function(r) {
  
  if (r$status_code != 200) {
    stop(paste0("Unsuccessful API request - status code: ", r$status_code, "."))
  }
  
  export_list <- XML::xmlParse(rawToChar(r$content))
  export_list <- XML::xmlToList(export_list)
  latest_dataset_id <- export_list[1]$exports$Id
  
  return(latest_dataset_id)
  
}
