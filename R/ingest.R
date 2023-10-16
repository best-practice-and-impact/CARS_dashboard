#' @title Get tidy data from API
#'
#' @description API function for data ingest. Loads data from the smartsurvey API and convert to a tidy data frame.
#'
#' @param ... optional: arguments to be passed to \link{ingest}
#'
#' @return tidied CARS dataset
#'
#' @export

get_tidy_data_api <- function() {
  
  ingest()
  
  data <- ingest() %>%
    convert_raw()
  
  return(data)
}

#' @title Convert raw data to data frame
#'
#' @description Convert raw smartsurvey data to data.frame . Extract contents (raw csv) from smartsurvey API request and convert to data.frame
#'
#' @param r api response object
#'
#' @return response content as a data.frame

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
    quote = c("\"\"", "'"),
    na.strings = c("", ".", "NA", "-", "\"\"", "\".\"", "\"NA\"", "\"-\"")
  )
  
  # Fix apostrophes
  data[] <- lapply(data, function(x) gsub("@SQ@", "'", x))
  
  return(data)
}


#' @title Ingest smartsurvey data
#'
#' @description Download smartsurvey export via the API. Download the exported data from smartsurvey using the API. Use convert_raw() to convert the API response to a data.frame.
#' Note: the first API request in a session will typically fail.
#'
#' @param survey the survey ID (character string/numeric). Defaults to "1376897".
#' @param token the API token (character string). Loaded from environment variable by default.
#' @param secret the secret API token (character string). Loaded from environment variable by default.
#' @param proxies proxy addresses (string). Loads from the user environment by default. Expects a string in the format "ip.address:port; alt.ip.address:port".
#' @param export the export ID (character string/numeric).

#' @return the exported data as a dataframe

ingest <- function(survey = "1376897",
                   token = Sys.getenv("CARS_TOKEN"),
                   secret = Sys.getenv("CARS_SECRET"),
                   proxies = Sys.getenv("alt_proxy"),
                   export) {
  
  if (missing(export)) {
    export <- "latest"
  }
  
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
  
  if (!is.null(proxies)) {
    proxy_list <- stringr::str_split(proxies, ";")[[1]]
    
    for (proxy_address in proxy_list) {
      httr::set_config(httr::use_proxy(proxy_address))
      
      tryCatch(
        {
          r <- httr::GET(
            url,
            httr::authenticate(user = token, password = secret, type = "basic")
          )
        },
        error = function(e) {
          warning(paste("Error in API request, trying alternative proxy: ", e))
        }
      )
      
      if (exists("r") && r$status_code == 200) {
        break
      }
      
    }
    
  }
  
  return(r)
}
