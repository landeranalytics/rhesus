#' list_surveys
#'
#' @return A tibble listing all the accessible surveys with the following columns:
#' \itemize{
#'   \item id - Survey identifer.
#'   \item title - Survey name.
#'   \item href - Survey URL.
#'   \item nickname - Survey nickname.
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' list_surveys()
#' }
list_surveys <- function() {
  resp <- surveymonkey_api("v3/surveys")

  surveys <- tibble::tibble(data = resp$content$data) %>%
    dplyr::transmute(
      id = purrr::map_chr(.data$data, "id"),
      title = purrr::map_chr(.data$data, "title"),
      href = purrr::map_chr(.data$data, "href"),
      nickname = purrr::map_chr(.data$data, "nickname")
    )

  surveys
}
