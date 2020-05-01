context("Function parse_legacy()")

test_that("", {
  expect_na_coercion(
    expect_is(parse_legacy.maiwashi_maaji("excel/gyokaikyo_dummy_maaji.xlsx",
                                          species = "マアジ", year = 2019),
              "data.frame")
  )
})
