#' Return a table of responses to a survey.
#'
#' Return a tibble of responses to a survey owned or shared by the user. Each
#' response has it's own `response_id`, and each response is it's own row. This
#' makes it a wide table with the column names depending on the question names,
#' types and for certain question types the answer options too.
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

#' Return a table of questions for a survey.
#'
#' Return a tibble of questions for a survey owned or shared by the user. There
#' is a `question_id` used to identify the question and a `heading` for
#' the question name, as well `family` and `subtype` which identify the type of
#' question. `choices` and `rows` are two fields (optionally) found in questions
#' and and have their own `id`s identifying them as well as attributes such as
#' `text` specifying what the survey taker sees.
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

#' Parse a list of arguments into a `single_choice` question response.
#'
#'
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

#' Parse a list of arguments into a `multiple_choice` response.
#'
#' @param x A named list with `responses`, `choices` and `rows` tibbles.
#'
#' @return A tibble with a single row per response and the following structure:
#' - `response_id` <chr>:  Id for the response.
#' - `<choice name>` <lgl>:  Logical indicating if the choice was selected. One column per choice.
#' - `text_other` <chr>:  (Optional) Text given an "Other" choice if it exists.
#' @keywords internal
multiple_choice <- function(x) {
  choices <- x$choices

  responses <- tidyr::unnest(x$responses, .data$responses)
  other_responses <- NULL

  if ("text" %in% names(responses)) {
    other_responses <- responses %>%
      dplyr::select(.data$response_id, .data$text) %>%
      dplyr::rename(text_other = .data$text) %>%
      tidyr::drop_na()
    responses <- responses %>%
      dplyr::select(.data$response_id, .data$choice_id) %>%
      tidyr::drop_na()
  }

  responses <- dplyr::left_join(responses, choices, by = c("choice_id" = "id")) %>%
    dplyr::select(.data$response_id, .data$text) %>%
    tibble::add_column(selected = TRUE) %>%
    tidyr::spread(.data$text, .data$selected, fill = FALSE)

  missing_cols <- setdiff(choices$text, names(responses))
  if (length(missing_cols) > 0) {
    missing_cols <- matrix(
      data = FALSE,
      nrow = nrow(responses), ncol = length(missing_cols)
    ) %>%
      magrittr::set_colnames(missing_cols) %>%
      tibble::as_tibble()
    responses <- dplyr::bind_cols(responses, missing_cols)
  }

  if (!is.null(other_responses)) {
    responses <- responses %>%
      dplyr::left_join(other_responses, by = "response_id") %>%
      tidyr::replace_na(list(text_other = ""))
  }

  responses
}

#' Parse a list of arguments into a `open_ended_single` response.
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

#' Parse a list of arguments into a `open_ended_essay` response.
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

#' Parse a list of arguments into a `open_ended_multi` response.
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

#' Parse a list of arguments into a `matrix_rating` response.
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

#' Parse a list of arguments into a `matrix_ranking` response.
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
