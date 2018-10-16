#' Make GET requests to the SurveyMonkey API.
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

#' Print a SurveyMonkey API response object.
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

#' OAuth endpoints for SurveyMonkey.
#'
#' See [httr::oauth_endpoint()] and
#' [SurveyMonkey API developer docs](https://developer.surveymonkey.com/api/v3/#oauth-2-0-flow)
#' for more details.
#'
#' @keywords internal
surveymonkey <- httr::oauth_endpoint(
  request = NULL,
  authorize = "authorize",
  access = "token",
  base_url = "https://api.surveymonkey.com/oauth"
)

#' OAuth application for SurveyMonkey.
#'
#' See [httr::oauth_app()] for more details.
#'
#' @keywords internal
app <- httr::oauth_app(
  appname = "surveymonkey",
  key = "9o2PuADKTpOwhiRmZVoZYg",
  secret = "156353387834082281681050717180649815724"
)

#' Environment to store credentials in.
#'
#' @keywords internal
.state <- new.env(parent = emptyenv())

#' Return a SurveyMonkey OAuth2 token.
#'
#' @keywords internal
#'
#' @return SurveyMonkey OAuth2 token.
surveymonkey_token <- function() {
  token <- .state$token
  if (is.null(token)) {
    token <- httr::oauth2.0_token(surveymonkey, app)
    .state$token <- token
  }

  token
}
