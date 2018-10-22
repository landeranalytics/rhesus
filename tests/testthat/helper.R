expect_levels <- function(object, levels, ...) {
  act <- quasi_label(rlang::enquo(object))

  act$levels <- levels(act$val)
  expect(
    all(act$levels == levels),
    paste(
      act$lab,
      "has levels",
      paste0("'", act$levels, "'", collapse = ", "),
      "not levels",
      paste0("'", levels, "'", collapse = ", ")
    )
  )

  invisible(act$val)
}
