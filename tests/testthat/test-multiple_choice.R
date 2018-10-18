context("test-multiple_choice")

load(file = test_path("test-multiple_choice.rdata"))

output1 <- multiple_choice(input1)
output2 <- multiple_choice(input2)
output3 <- multiple_choice(input3)

test_that("multiple_choice returns a single row per response", {
  all_unique <- function(x) all(table(x) == 1L)

  expect_true(all_unique(output1$response_id))
  expect_true(all_unique(output2$response_id))
  expect_true(all_unique(output3$response_id))
})

test_that("multiple_choice does fills unselected choices", {
  no_nas <- function(x) all(!is.na(x))

  expect_true(no_nas(output1))
  expect_true(no_nas(output2))
  expect_true(no_nas(output3))
})

test_that("multiple_choice has a column for all possible choices and optional text_other", {
  names <- c("response_id", "Apples", "Bananas", "Chocolate")

  expect_named(output1, names, ignore.order = TRUE)
  expect_named(output2, names, ignore.order = TRUE)
  expect_named(output3, c(names, "text_other"), ignore.order = TRUE)
})
