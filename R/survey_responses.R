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

single_choice <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest()
  if ("text" %in% colnames(responses)) {
    responses <- responses %>%
      dplyr::rename(text_other = .data$text)
  } else {
    responses <- responses %>%
      tibble::add_column(text_other = NA_character_)
  }
  choices <- x$choices
  dplyr::left_join(responses, choices, by = c("choice_id" = "id")) %>%
    dplyr::mutate(text = dplyr::if_else(is.na(.data$text), .data$text_other, .data$text)) %>%
    dplyr::select(.data$response_id, .data$text)
}

multiple_choice <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest()
  if ("text" %in% colnames(responses)) {
    responses <- responses %>%
      dplyr::rename(text_other = .data$text)
  } else {
    responses <- responses %>%
      tibble::add_column(text_other = NA_character_)
  }
  choices <- x$choices
  dplyr::left_join(responses, choices, by = c("choice_id" = "id")) %>%
    dplyr::select(.data$response_id, .data$text, .data$text_other) %>%
    tibble::add_column(selected = TRUE) %>%
    tidyr::spread(.data$text, .data$selected)
}

open_ended_single <- function(x) {
  x$responses %>%
    tidyr::unnest()
}

open_ended_essay <- function(x) {
  x$responses %>%
    tidyr::unnest()
}

open_ended_multi <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest()
  rows <- x$rows
  dplyr::left_join(responses, rows,
    by = c("row_id" = "id"),
    suffix = c("_response", "_row")
  ) %>%
    dplyr::select(.data$response_id, .data$text_row, .data$text_response) %>%
    tidyr::spread(.data$text_row, .data$text_response)
}

matrix_rating <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest()
  choices <- x$choices
  rows <- x$rows
  if (all(choices$text == "") && all(rows$text == "")) {
    responses %>%
      dplyr::left_join(choices, by = c("choice_id" = "id")) %>%
      dplyr::select(.data$response_id, .data$weight)
  } else {
    responses %>%
      dplyr::left_join(choices, by = c("choice_id" = "id")) %>%
      dplyr::left_join(rows, by = c("row_id" = "id"), suffix = c("_choice", "_row")) %>%
      dplyr::select(.data$response_id, .data$text_choice, .data$text_row) %>%
      tidyr::spread(.data$text_row, .data$text_choice)
  }
}

matrix_ranking <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest()
  choices <- x$choices
  rows <- x$rows
  responses %>%
    dplyr::left_join(choices, by = c("choice_id" = "id")) %>%
    dplyr::left_join(rows, by = c("row_id" = "id"), suffix = c("_choice", "_row")) %>%
    dplyr::select(.data$response_id, .data$text_row, .data$position_choice) %>%
    tidyr::spread(.data$text_row, .data$position_choice)
}
