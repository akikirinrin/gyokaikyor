context("Function parse_legacy()")

test_that("", {
  expect_na_coercion(
    expect_is(parse_legacy(list(fname = "excel/gyokaikyo_dummy_maaji.xlsx",
                                spcs = "maaji",
                                year = 2019)),
              "data.frame")
  )
})
