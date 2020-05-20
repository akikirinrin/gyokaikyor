context("Parse length distribution for maaji")

parsed_data <-
  parse_legacy_maaji_length_dist(
    fname = "excel/gyokaikyo_dummy_maaji_length_dist.xlsx"
    )

test_that("function exists", {
  expect_is(parse_legacy_maaji_length_dist, "function")
})

test_that("all sheets are properly parsed", {

  expected_sheets <- c("鹿児島尾数", "宮～高", "徳～和",
                       "三～愛", "静～神", "千葉以北")

  expect_setequal(parsed_data %>%
                    dplyr::pull(Sheet) %>%
                    unique(),
                  expected_sheets
  )
})

test_that("Years, Months, and Regions are properly parsed", {
  expect_setequal(parsed_data %>%
                    dplyr::pull(Year) %>%
                    unique(),
                  c(2006:2020)
  )
  expect_setequal(parsed_data %>%
                    dplyr::pull(Month) %>%
                    unique(),
                  c(1:12)
  )

  expected_region <- c("鹿児島", "宮～高", "徳～和",
                       "三～愛", "静～神", "千葉以北")

  expect_setequal(parsed_data %>%
                    dplyr::pull(Region) %>%
                    unique(),
                  expected_region
  )
})

context("- Numerical test")

test_that("sum of freq for each year is correct", {
  expect_equal(parsed_data %>%
                 dplyr::filter(Region == "鹿児島") %>%
                 dplyr::pull(Frequency) %>%
                 sum(na.rm = TRUE),
               8691859.62)

  expect_equal(parsed_data %>%
                 dplyr::filter(Region == "宮～高") %>%
                 dplyr::pull(Frequency) %>%
                 sum(na.rm = TRUE),
               207917478.8)

  expect_equal(parsed_data %>%
                 dplyr::filter(Region == "徳～和") %>%
                 dplyr::pull(Frequency) %>%
                 sum(na.rm = TRUE),
               207808774.5)

  expect_equal(parsed_data %>%
                 dplyr::filter(Region == "三～愛") %>%
                 dplyr::pull(Frequency) %>%
                 sum(na.rm = TRUE),
               277053604.2)

  expect_equal(parsed_data %>%
                 dplyr::filter(Region == "静～神") %>%
                 dplyr::pull(Frequency) %>%
                 sum(na.rm = TRUE),
               346218328.7)

  expect_equal(parsed_data %>%
                 dplyr::filter(Region == "千葉以北") %>%
                 dplyr::pull(Frequency) %>%
                 sum(na.rm = TRUE),
               748686200.6)
})
