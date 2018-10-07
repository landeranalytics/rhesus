survey_questions <- function(id) {
  path <- glue::glue("v3/surveys/{id}/details")
  resp <- surveymonkey_api(path)

  questions <- tibble::tibble(pages = resp$content$pages) %>%
    dplyr::transmute(questions = purrr::map(.data$pages, "questions")) %>%
    tidyr::unnest(.data$questions) %>%
    dplyr::transmute(
      question_id = purrr::map_chr(.data$questions, "id"),
      heading = .data$questions %>%
        purrr::map("headings") %>%
        purrr::flatten() %>%
        purrr::map_chr("heading"),
      family = purrr::map_chr(.data$questions, "family"),
      subtype = purrr::map_chr(.data$questions, "subtype"),
      answers = purrr::map(.data$questions, "answers")
    )

  questions
}
