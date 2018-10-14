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
    config = httr::config(token = surveymonkey_token()),
    httr::user_agent("https://github.com/landeranalytics/rhesus")
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

survemonkey <- httr::oauth_endpoint(
  request = NULL,
  authorize = "authorize",
  access = "token",
  base_url = "https://api.surveymonkey.com/oauth"
)

app <- httr::oauth_app(
  appname = "surveymonkey",
  key = "9o2PuADKTpOwhiRmZVoZYg",
  secret = "156353387834082281681050717180649815724"
)

.state <- new.env(parent = emptyenv())

#' surveymonkey_token
#'
#' @return SurveyMonkey OAuth2 token.
surveymonkey_token <- function() {
  token <- .state$token
  if (is.null(token)) {
    token <- httr::oauth2.0_token(survemonkey, app)
    .state$token <- token
  }

  token
}
