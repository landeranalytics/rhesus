#' surveymonkey_api
#'
#' @param path URL path for the SurveyMonkey API.
#'
#' @return List representing the JSON response.
#' @export
#'
#' @examples
#' \dontrun{
#' surveymonkey_api("v3/surveys")
#' }
surveymonkey_api <- function(path) {
  url <- httr::modify_url("https://api.surveymonkey.com", path = path)

  resp <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", surveymonkey_token())),
    httr::user_agent("https://github.com/brooklynbagel/rhesus")
  )

  if (httr::http_type(resp) != "application/json") {
    stop("SurveyMonkey API did not return JSON", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text"),
    simplifyVector = FALSE
  )

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "surveymonkey_api"
  )
}

#' print.surveymonkey_api
#'
#' @param x SurveyMonkey API response object
#' @param ... other arguments ignored by this method
#'
#' @export
print.surveymonkey_api <- function(x, ...) {
  cat("<SurveyMonkey ", x$path, ">\n", sep = "")
  dplyr::glimpse(x$content)
  invisible(x)
}

#' surveymonkey_token
#'
#' @return SurveyMonkey access token, if there is one.
surveymonkey_token <- function() {
  token <- Sys.getenv("SURVEYMONKEY_TOKEN")

  if (identical(token, "")) {
    stop("Please set env var SURVEYMONKEY_TOKEN to your SurveyMonkey access token",
      call. = FALSE
    )
  }

  token
}
