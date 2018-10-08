survey_responses <- function(id) {
  path <- glue::glue("v3/surveys/{id}/responses/bulk")
  resp <- surveymonkey_api(path)

  responses <- tibble::tibble(data = resp$content$data) %>%
    dplyr::transmute(
      response_id = purrr::map_chr(.data$data, "id"),
      pages = purrr::map(.data$data, "pages")
    ) %>%
    tidyr::unnest(.data$pages) %>%
    dplyr::mutate(questions = purrr::map(.data$pages, "questions")) %>%
    dplyr::select(-.data$pages) %>%
    tidyr::unnest(.data$questions) %>%
    dplyr::mutate(
      question_id = purrr::map_chr(.data$questions, "id"),
      answers = purrr::map(.data$questions, "answers")
    ) %>%
    dplyr::select(-.data$questions) %>%
    dplyr::mutate(responses = purrr::map(.data$answers, dplyr::bind_rows)) %>%
    dplyr::select(-.data$answers)

  responses
}
