context("Parseing legacy Excel file")

context("- legacy()")

test_that("legacy object can be created", {
  expect_is(legacy(fname = "foo.xlsx",
                   year = 2020,
                   spcs = "maaji"),
            "maaji")

  expect_is(legacy(fname = "foo.xlsx",
                   year = 2020,
                   spcs = "bar"),
            "bar")
})

test_that("throw error without args", {
  expect_error(
    expect_is(legacy(year = 2020,
                     spcs = "maaji"),
              "maaji"),
    '"fname" is missing'
  )

  expect_error(
   expect_is(legacy(fname = "foo.xlsx",
                    spcs = "maaji"),
             "maaji"),
    '"year" is missing'
  )
})

context("- parse_legacy()")
test_that("", {
  expect_na_coercion(
    expect_is(parse_legacy(list(fname = "excel/gyokaikyo_dummy_maaji.xlsx",
                                spcs  = "maaji",
                                year  = 2019)),
              "data.frame")
  )
})

context("- xl2df()")

test_that("Excel is parsed into df", {
  expect_na_coercion(
    expect_is(
      xl2df(fname = "excel/gyokaikyo_dummy_maaji.xlsx",
            year  = 2019,
            spcs  = "maaji"),
      "data.frame"
    )
  )
})
