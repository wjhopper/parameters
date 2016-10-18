context("Testing parameters functionality")

test_that("Invalid parameter objects cannot be created", {
  expect_error(new("parameter", value = 100, name = "test",
                   upper_bound = 10, lower_bound = 0))
})

test_that("Parameter values can be extracted and reassigned with value()", {
  p <- new("parameter", value = 5, name = "test",
           upper_bound = 10, lower_bound = 0)
  expect_equivalent(value(p), 5)

  value(p) <- 7.5
  expect_equivalent(value(p), 7.5)
  expect_error(value(p) <- -10)
})
