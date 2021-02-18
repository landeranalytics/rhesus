#' Title
#'
#' @param combined
#'
#' @return
#'
#' @examples
combine_single_choice <- function(combined) {
  responses <- combined$responses[[1]]
  answers <- combined$answers[[1]]

  responses <- responses %>%
    tidyr::unnest(.data$answers) %>%
    tidyr::hoist(.data$answers, "choice_id")

  answers <- tibble::tibble(choices = answers$choices) %>%
    tidyr::hoist(.data$choices, choice_id = "id", "text") %>%
    dplyr::select(-.data$choices)

  if ("other" %in% names(answers)) {
    other <- tibble::as_tibble(answers$other) %>%
      dplyr::select(choice_id = .data$id, .data$text)
    answers <- dplyr::bind_rows(answers, other)
  }

  responses %>%
    dplyr::left_join(answers, by = "choice_id") %>%
    dplyr::select(-.data$choice_id)
}
