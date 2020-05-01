prefec_catch_sum_test <-
  function(dat, prefec, location, fishery, expected_data) {
    our_data <-
      dat %>%
      dplyr::filter(Prefec == prefec &
                      Location == location &
                      Fishery == fishery
      ) %>%
      dplyr::pull(Catch_ton) %>%
      sum()

    testthat::expect_equal(our_data, expected_data)
  }

expect_na_coercion <- function(test) {
  testthat::expect_warning(test, "NAs introduced by coercion")
}
