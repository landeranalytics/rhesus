expect_levels <- function(object, levels, ...) {
  act <- quasi_label(rlang::enquo(object))

  act$levels <- levels(act$val)
  act$num_levels <- length(act$levels)

  num_levels <- length(levels)

  expect(
    act$num_levels == num_levels && all(act$levels == levels),
    paste0(
      act$lab,
      " has levels ",
      paste0("'", act$levels, "'", collapse = ", "),
      ", not levels ",
      paste0("'", levels, "'", collapse = ", "),
      "."
    )
  )

  invisible(act$val)
}
