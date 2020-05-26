context("Util functions")

test_that("%not_in% returns TRUE if components of 'x' are not found in 'y'", {
  expect_true("a" %not_in% c("b", "c"))
  expect_true(1   %not_in% 2:4)
  expect_true(1:3 %not_in% 4:6)
})

test_that("%not_in% returns FALSE if components of 'x' are found in 'y'", {
  expect_false("a" %not_in% c("a", "b", "c"))
  expect_false(1   %not_in% 1:4)
  expect_false(1:3 %not_in% 1:4)
})

test_that("seq_log() creates logarhithmic sequence", {
  expect_equal(seq_log(1, 3), c(10, 100, 1000))
  expect_equal(seq_log(0, 2), c(1, 10, 100))
  expect_equal(seq_log(-3, 3), c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
  expect_equal(seq_log(1), c(10))
})
