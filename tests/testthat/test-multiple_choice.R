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
