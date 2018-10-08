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
      answers = .data$questions %>%
        purrr::map("answers") %>%
        purrr::map(function(x) {
          choices <- dplyr::bind_rows(x$choices)
          rows <- dplyr::bind_rows(x$rows)
          list(choices = choices, rows = rows)
        }),
      names = purrr::map(.data$answers, names)
    ) %>%
    tidyr::unnest(.data$answers, .data$names) %>%
    tidyr::spread(.data$names, .data$answers)

  questions
}
