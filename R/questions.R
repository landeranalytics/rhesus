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
sm_questions <- function(survey_id) {
  path <- glue::glue("v3/surveys/{survey_id}/details")
  resp <- sm_api(path)

  questions <- tibble::tibble(pages = resp$content$pages) %>%
    tidyr::hoist(.data$pages, "questions") %>%
    dplyr::select(-.data$pages) %>%
    tidyr::unnest(.data$questions) %>%
    tidyr::hoist(.data$questions, question_id = "id", heading = list("headings", 1, "heading"), "family", "answers") %>%
    dplyr::select(-.data$questions)

  questions
}
