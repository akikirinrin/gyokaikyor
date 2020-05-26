context("Parsing index Excel")

context("- parse_index()")

test_that("works for rawdata", {
  parsed <- parse_index("excel/index_maaji.xlsx", year = 2019, type = "rawdata")

  expect_is(parsed, "data.frame")

  expect_setequal(
    colnames(parsed),
    c("Month", "Place", "Fname", "Sheet", "Year", "LogIndex")
  )
})

test_that("works for geomean", {
  parsed <- parse_index("excel/index_maaji.xlsx", year = 2019, type = "geomean")

  expect_is(parsed, "data.frame")

  expect_setequal(
    colnames(parsed),
    c("Place", "Fname", "Sheet", "Year", "Geomean", "Period")
  )
})
