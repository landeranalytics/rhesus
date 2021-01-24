# Credit to Jenny Bryan and the {googlesheets} developers, and the {meetupr} developers
# https://github.com/jennybc/googlesheets/blob/master/R/gs_auth.R
# https://github.com/rladies/meetupr/blob/master/R/meetup_auth.R

# Environment to store OAuth token
.state <- new.env(parent = emptyenv())

# OAuth endpoint for SurveyMonkey
sm_endpoint <- httr::oauth_endpoint(
  NULL, "authorize", "token",
  base_url = "https://api.surveymonkey.com/oauth"
)

# OAuth app for SurveyMonkey using rhesus
sm_app <- httr::oauth_app(
  "surveymonkey",
  key = "-DugXXsJS2eT1T8EmTOSjw",
  secret = "294972726930074873744232140979229495591"
)

#' Authorize `rhesus`
#'
#' Authorize `rhesus` to list your surveys and read the responses from them.
#'
#' @param cache Logical indicating where `rhesus` should cache your OAuth token in the file `.httr-cache`.
#'
#' @return A token object of class [Token2.0][httr::Token2.0()], invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' surveymonkey_auth()
#' }
sm_auth <- function(cache = getOption("rhesus.httr_oauth_cache")) {
  .state$token <- httr::oauth2.0_token(sm_endpoint, sm_app, cache = cache)

  invisible(.state$token)
}
