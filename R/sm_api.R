ua <- httr::user_agent("http://github.com/brooklynbagel/rhesus")

#' Access the SurveyMonkey API.
#'
#' Makes a request to the SurveyMonkey API.
#'
#' @param path URL path to use.
#'
#' @return An object of class `sm_api` containing the following:
#' * `content` - a list of the response contents parsed from [json][jsonlite::fromJSON()].
#' * `path` - the URL path used in the request.
#' * `response` - the [response object][httr::response].
#' @export
#'
#' @examples \dontrun{
#' sm_api("/v3/surveys")
#' }
sm_api <- function(path) {
  url <- httr::modify_url("https://api.surveymonkey.com", path = path)

  resp <- httr::GET(url, .state$token, ua)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return JSON.", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      glue::glue(
        "SurveyMonkey API request failed with error '{parsed$error$name}' [{httr::status_code(resp)}]",
        "{parsed$error$message}",
        "{parsed$error$docs}",
        .sep = "\n"
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "sm_api"
  )
}

#' Print a [sm_api][sm_api()] response object.
#'
#' @param x An [sm_api][sm_api()] response object.
#' @param ... Extra arguments needed for method reasons.
#'
#' @return The [sm_api][sm_api()], invisibly.
#' @export
#'
#' @importFrom utils str
#'
#' @examples
#' \dontrun{
#' surveys_raw <- sm_api("v3/surveys")
#' surveys_raw
#' }
print.sm_api <- function(x, ...) {
  cat("<SurveyMonkey ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}
