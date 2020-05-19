context("Parsing index Excel")

test_that("parse_index() works", {
  parsed <- parse_index("excel/index_maaji.xlsx", year = 2019)

  expect_is(parsed, "data.frame")

  expect_setequal(
    colnames(parsed),
    c("WHAT_IS_THIS", "Place", "Fname", "Sheet", "Year", "LogIndex")
  )
})
