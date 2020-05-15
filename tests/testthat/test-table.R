context("Tables")

test_that("make_table() works", {
  expect_is(make_table(load_catch_dummy_kagoshima(),
                       spcs = "マアジ",
                       year = 2019),
            "knitr_kable")
  expect_error(make_table(iris, spcs = "マアジ", year = 2019))
})
