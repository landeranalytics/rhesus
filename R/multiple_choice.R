#' Title
#'
#' @param combined
#'
#' @return
#'
#' @examples
combine_multiple_choice <- function(combined) {
  responses <- combined$responses[[1]]
  answers <- combined$answers[[1]]

  responses <- responses %>%
    tidyr::unnest(.data$answers) %>%
    tidyr::hoist(.data$answers, "choice_id", other = "text") %>%
    dplyr::select(-.data$answers)

  other <- responses %>%
    tidyr::drop_na(.data$other) %>%
    dplyr::select(-.data$choice_id)

  choices <- tibble::tibble(choices = answers$choices) %>%
    tidyr::hoist(.data$choices, choice_id = "id", "text") %>%
    dplyr::select(-.data$choices)

  responses <- responses %>%
    dplyr::left_join(choices, by = "choice_id") %>%
    tidyr::drop_na(.data$text) %>%
    tibble::add_column(value = TRUE) %>%
    tidyr::pivot_wider(id_cols = .data$response_id, names_from = .data$text, values_fill = FALSE)

  responses %>%
    dplyr::left_join(other, by = "response_id")
}
