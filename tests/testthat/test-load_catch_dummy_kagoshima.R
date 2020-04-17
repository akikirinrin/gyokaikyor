context("Load_catch_dummy_kagoshima")

test_that("load_catch_dummy_kagoshima exits", {
  expect_silent(load_catch_dummy_kagoshima)
})

test_that("load_catch_dummy_kagoshima is a data frame", {
  expect_is(load_catch_dummy_kagoshima(), "data.frame")
  expect_is(load_catch_dummy_kagoshima() %>%
              dplyr::pull(Catch_ton), "integer")
})

context("- Numerical test")

test_that("catch is created with one-by-one ascending number", {

  prefec_catch_sum_test(
    load_catch_dummy_kagoshima(),
    prefec = "鹿児島", location = "阿久根", fishery = "まき網",
    expected_data = 363378
  )

  prefec_catch_sum_test(
    load_catch_dummy_kagoshima(),
    prefec = "鹿児島", location = "枕崎", fishery = "まき網",
    expected_data = 1089282)

  prefec_catch_sum_test(
    load_catch_dummy_kagoshima(),
    prefec = "鹿児島", location = "山川", fishery = "まき網",
    expected_data = 1815186)

  prefec_catch_sum_test(
    load_catch_dummy_kagoshima(),
    prefec = "鹿児島", location = "内之浦", fishery = "まき網",
    expected_data = 2541090)

  prefec_catch_sum_test(
    load_catch_dummy_kagoshima(),
    prefec = "鹿児島", location = "阿久根", fishery = "その他",
    expected_data = 3266994)

  prefec_catch_sum_test(
    load_catch_dummy_kagoshima(),
    prefec = "鹿児島", location = "内之浦", fishery = "その他",
    expected_data = 3992898)

})
