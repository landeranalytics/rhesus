context("test-matrix_rating")

# Construct fake data similar to actual responses to test `matrix_rating` on.
library(tibble)

choices <- tribble(
  ~ description, ~ id,  ~ visible, ~ is_na, ~ text,    ~ position,
  "",            "684", TRUE,      FALSE,   "sushi",   1L,
  "",            "685", TRUE,      FALSE,   "pizza",   2L,
  "",            "686", TRUE,      FALSE,   "noodles", 3L
)

rows <- tribble(
  ~ visible, ~ text, ~ position, ~ id,
  TRUE,      "best", 1L,         "772",
  TRUE,      "okay", 2L,         "773",
  TRUE,      "meh",  3L,         "774"
)

responses_unnested <- tribble(
  ~ response_id, ~ choice_id, ~ row_id,
  # first respondent
  "480",         "684",      "772",
  "480",         "685",      "773",
  "480",         "686",      "774",
  # second respondent
  "681",         "685",      "772",
  "681",         "684",      "774"
)
responses <- tidyr::nest(responses_unnested, -response_id, .key = "responses")

test_that("matrix_rating returns a tibble", {
  x <- lst(responses, choices, rows)
  expect_s3_class(matrix_rating(x), c("data.frame", "tbl", "tbl.df"))
})
