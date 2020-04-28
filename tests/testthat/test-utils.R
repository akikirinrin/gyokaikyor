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
