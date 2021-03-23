#' Title
#'
#' @param combined
#'
#' @return
#'
#' @examples
combine_matrix_single <- function(combined) {
  responses <- combined$responses[[1]]
  answers <- combined$answers[[1]]

  responses <- responses$responses[[1]] %>%
    tidyr::unnest(.data$answers) %>%
    tidyr::hoist(.data$answers, "choice_id", "row_id")

  choices <- tibble::tibble(choices = questions$answers[[1]]$choices) %>%
    tidyr::hoist(.data$choices, choice_id = "id", choice_text = "text") %>%
    dplyr::select(-.data$choices)

  rows <- tibble::tibble(rows = questions$answers[[1]]$rows) %>%
    tidyr::hoist(.data$rows, row_id = "id", row_text = "text") %>%
    dplyr::select(-.data$rows)

  responses %>%
    dplyr::left_join(choices, by = "choice_id") %>%
    dplyr::left_join(rows, by = "row_id") %>%
    tidyr::pivot_wider(id_cols = .data$response_id, names_from = .data$row_text, values_from = .data$choice_text)
}
