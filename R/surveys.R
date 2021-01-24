#' List your surveys
#'
#' @return A `tibble` of surveys with the following columns:
#' * `id` - survey id.
#' * `title` - survey title.
#' * `nickname` - survey nickname.
#' * `href` - a URL to the survey.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' sm_surveys()
#' }
sm_surveys <- function() {
  surveys_raw <- sm_api("v3/surveys")

  surveys <- tibble::tibble(data = surveys_raw$content$data) %>%
    tidyr::hoist(.data$data, "id", "title", "nickname", "href")

  surveys
}
