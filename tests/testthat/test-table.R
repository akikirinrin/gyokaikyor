context("Tables")

test_that("df2table() works", {
  expect_is(df2table(load_catch_dummy_kagoshima(),
                       year = 2019),
            "knitr_kable")
  expect_error(df2table(iris, year = 2019))
})

context("- options")

test_that("Japanese option works", {
  expect_is(df2table(load_catch_dummy_kagoshima(),
                     year = 2019,
                     Japanese = TRUE),
            "knitr_kable")
})

test_that("header option works", {
  expect_is(df2table(load_catch_dummy_kagoshima(),
                     year = 2019,
                     header = FALSE),
            "knitr_kable")
})
