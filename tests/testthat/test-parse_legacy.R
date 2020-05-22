context("Parseing legacy Excel file")

context("- legacy()")

test_that("legacy object can be created", {
  expect_is(legacy(fname = "foo.xlsx",
                   year = 2020,
                   type = "catch",
                   spcs = "maaji"),
            "catch_maaji")

  expect_is(legacy(fname = "foo.xlsx",
                   year = 2020,
                   type = "foo",
                   spcs = "bar"),
            "foo_bar")
})

test_that("throw error without args", {
  expect_error(
    expect_is(legacy(year = 2020,
                     type = "catch",
                     spcs = "maaji"),
              "catch_maaji"),
    '"fname" is missing'
  )

  expect_error(
   expect_is(legacy(fname = "foo.xlsx",
                    type  = "catch",
                    spcs  = "maaji"),
             "catch_maaji"),
    '"year" is missing'
  )

  expect_error(
    expect_is(legacy(fname = "foo.xlsx",
                     year = 2020,
                     spcs = "maaji"),
              "maaji"),
    '"type" is missing'
  )

})

context("- parse_catch_legacy()")
test_that("", {
  expect_df_with_na_coercion(
    parse_catch_legacy(
      list(fname = "excel/gyokaikyo_dummy_maaji.xlsx",
           spcs  = "maaji",
           year  = 2019)
    )
  )
})

context("- parse_catch_at_age_legacy()")

test_that("parse_catch_at_age_legacy() works", {
  expect_df_with_na_coercion(
    parse_catch_at_age_legacy(
      list(fname = "excel/catch_at_age_maaji.xlsx",
           spcs  = "maaji",
           year  = 2019)
    )
  )
})

context("- xl2df() parses Excel files into df")

test_that("catch", {
  expect_df_with_na_coercion(
     xl2df(fname = "excel/gyokaikyo_dummy_maaji.xlsx",
           year  = 2019,
           type  = "catch",
           spcs  = "maaji")
  )
})

test_that("catch at age", {
  expect_df_with_na_coercion(
     xl2df(fname = "excel/catch_at_age_maaji.xlsx",
           year  = 2019,
           type  = "catch_at_age",
           spcs  = "maaji")
  )
})
