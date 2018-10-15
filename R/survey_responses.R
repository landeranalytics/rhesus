#' survey_responses
#'
#' @param id An id for the survey. Can be found using [list_surveys()].
#'
#' @return A wide tibble of responses.
#' @export
#'
#' @examples
#' \dontrun{
#' survey_responses(123456789)
#' }
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

  questions <- survey_questions(id)

  responses <- responses %>%
    tidyr::nest(-.data$question_id, .key = "responses") %>%
    dplyr::left_join(questions, by = "question_id")

  responses <- responses %>%
    dplyr::mutate(
      args = purrr::transpose(list(
        responses = .data$responses,
        choices = .data$choices,
        rows = .data$rows
      )),
      fn = dplyr::case_when(
        family == "single_choice" ~ list(single_choice),
        family == "multiple_choice" ~ list(multiple_choice),
        family == "open_ended" & subtype == "single" ~ list(open_ended_single),
        family == "open_ended" & subtype == "essay" ~ list(open_ended_essay),
        family == "open_ended" & subtype == "multi" ~ list(open_ended_multi),
        family == "matrix" & subtype == "rating" ~ list(matrix_rating),
        family == "matrix" & subtype == "ranking" ~ list(matrix_ranking),
        TRUE ~ list(function(x) NULL)
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(responses = purrr::invoke_map(.data$fn, x = .data$args)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(responses = purrr::map2(.data$responses, .data$heading, function(df, nm) {
      df %>%
        dplyr::rename_at(
          dplyr::vars(-.data$response_id),
          ~paste0(nm, "_", .)
        )
    }))

  responses <- responses$responses %>%
    purrr::reduce(dplyr::left_join, by = "response_id")

  responses
}

#' survey_questions
#'
#' @param id An id for the survey. Can be found using [list_surveys()].
#'
#' @return A tibble of the questions and their details. The following columns are included:
#' - `question_id` <chr>:  The id of the question.
#' - `heading` <chr>:  The question name.
#' - `family` <chr>:  The question family and subtype that define the question type.
#' - `subtype` <chr>:  The question family and subtype that define the question type.
#' - `choices` <list>:  A table of choice ids and attributes.
#' - `rows` <list>:  A table of row ids and attributes.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' survey_questions(12345679)
#' }
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

#' single_choice
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>:  Id for the response.
#' - `text` <chr>:  Text of the option chosen.
#' @keywords internal
single_choice <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest(.data$responses)
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

#' multiple_choice
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>:  Id for the response.
#' - `<choice name>` <lgl>:  Logical indicating if the choice was selected. One column per choice.
#' - `text_other` <chr>:  (Optional) Text given an "Other" choice if it exists.
#' @keywords internal
multiple_choice <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest(.data$responses)
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

#' open_ended_single
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>: Id for the response.
#' - `text` <chr>: Text response.
#' @keywords internal
open_ended_single <- function(x) {
  x$responses %>%
    tidyr::unnest(.data$responses)
}

#' open_ended_essay
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>: Id for the response.
#' - `text` <chr>: Text response.
#' @keywords internal
open_ended_essay <- function(x) {
  x$responses %>%
    tidyr::unnest(.data$responses)
}

#' open_ended_multi
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>: Id for the response.
#' - `<row name>` <chr>: Text entered for the row. One column per row.
#' @keywords internal
open_ended_multi <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest(.data$responses)
  rows <- x$rows
  dplyr::left_join(responses, rows,
    by = c("row_id" = "id"),
    suffix = c("_response", "_row")
  ) %>%
    dplyr::select(.data$response_id, .data$text_row, .data$text_response) %>%
    tidyr::spread(.data$text_row, .data$text_response)
}

#' matrix_rating
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>: Id for the response.
#' - `<row name>` <chr>: (If question is a matrix of choices) Text entered for the row. One column per question row.
#' - `weight>` <int>: (If question is a rating) Rating selected for the response.
#' @keywords internal
matrix_rating <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest(.data$responses)
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

#' matrix_ranking
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>: Id for the response.
#' - `<row name>` <int>: Ranking given for the row. One column per question row.
#' @keywords internal
matrix_ranking <- function(x) {
  responses <- x$responses %>%
    tidyr::unnest(.data$responses)
  choices <- x$choices
  rows <- x$rows
  responses %>%
    dplyr::left_join(choices, by = c("choice_id" = "id")) %>%
    dplyr::left_join(rows, by = c("row_id" = "id"), suffix = c("_choice", "_row")) %>%
    dplyr::select(.data$response_id, .data$text_row, .data$position_choice) %>%
    tidyr::spread(.data$text_row, .data$position_choice)
}
