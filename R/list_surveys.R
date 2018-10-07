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
