context("test-expect_levels")

test_that("expect_levels compares factor levels correctly", {
  # factor levels compare to factor levels
  f <- factor(c("a", "b", "c"))
  expect_success(expect_levels(f, factor(c("a", "b", "c"))))
  expect_failure(expect_levels(f, factor(c("a", "b", "c", "d"))),
                 "has levels 'a', 'b', 'c', not levels 'a', 'b', 'c', 'd'")
  expect_failure(expect_levels(f, factor(c("a", "b"))),
                 "has levels 'a', 'b', 'c', not levels 'a', 'b'")

  # ordered levels compare to ordered levels
  o <- ordered(c("a", "b", "c"))
  expect_success(expect_levels(o, ordered(c("a", "b", "c"))))
  expect_failure(expect_levels(o, ordered(c("a", "b", "c", "d"))),
                 "has levels 'a', 'b', 'c', not levels 'a', 'b', 'c', 'd'")
  expect_failure(expect_levels(o, ordered(c("a", "b"))),
                 "has levels 'a', 'b', 'c', not levels 'a', 'b'")
})
