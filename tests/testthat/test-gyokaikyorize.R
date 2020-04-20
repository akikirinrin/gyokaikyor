context("gyokaikyorize() choose correct 'Fishery' and 'Location' from binary?")

context("- maaji")

test_that("gyokaikyorize.maaji()", {
  expect_equal(load_catch_dummy_kagoshima() %>% gyokaikyorize_maaji(),
               stockdbr::parse_legacy.maaji("excel/gyokaikyo_dummy_maaji.xlsx"))
})
