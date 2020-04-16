context("Example")

test_that("example", {
  expect_equal(1 + 1, 2)
})

test_that("example 2", {
  expect_identical(1 + 1, 2)
})
