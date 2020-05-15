context("Tables")

test_that("df2table() works", {
  expect_is(df2table(load_catch_dummy_kagoshima(),
                       year = 2019),
            "knitr_kable")
  expect_error(df2table(iris, year = 2019))
})
