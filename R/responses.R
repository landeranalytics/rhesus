#' Title
#'
#' @param survey_id
#'
#' @return
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
sm_responses <- function(survey_id) {
  path <- glue::glue("v3/surveys/{survey_id}/responses/bulk")
  resp <- sm_api(path)

  responses <- tibble::tibble(data = resp$content$data) %>%
    tidyr::hoist(.data$data, response_id = "id", "pages") %>%
    dplyr::select(-.data$data) %>%
    tidyr::unnest(.data$pages) %>%
    tidyr::hoist(.data$pages, "questions") %>%
    dplyr::select(-.data$pages) %>%
    tidyr::unnest(.data$questions) %>%
    tidyr::hoist(.data$questions, question_id = "id", "answers") %>%
    tidyr::nest(responses = c(.data$response_id, .data$answers))
}
