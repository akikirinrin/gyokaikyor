load_catch_dummy_kagoshima <- function() {

  year <- rep(1950:2020, each = 12)
  month <- rep(1:12, 2021 - 1950)

  akune <-
    tibble::tibble("Catch_ton" = seq(from = 1, to = 852),
                   "Location" = "阿久根",
                   "Fishery" = "まき網",
                   Year = year,
                   Month = month)

  makurazaki <-
    tibble::tibble("Catch_ton" = seq(from = 853, to = 1704),
                   "Location" = "枕崎",
                   "Fishery" = "まき網",
                   Year = year,
                   Month = month)

  yamagawa <-
    tibble::tibble("Catch_ton" = seq(from = 1705, to = 2556),
                   "Location" = "山川",
                   "Fishery" = "まき網",
                   Year = year,
                   Month = month)

  uchinoura <-
    tibble::tibble("Catch_ton" = seq(from = 2557, to = 3408),
                   "Location" = "内之浦",
                   "Fishery" = "まき網",
                   Year = year,
                   Month = month)


  akune_bouuke <-
    tibble::tibble("Catch_ton" = seq(from = 3409, to = 4260),
                   "Location" = "阿久根",
                   "Fishery" = "その他",
                   Year = year,
                   Month = month)

  uchinoura_bouuke <-
    tibble::tibble("Catch_ton" = seq(from = 4261, to = 5112),
                   "Location" = "内之浦",
                   "Fishery" = "その他",
                   Year = year,
                   Month = month)


  dplyr::bind_rows(akune, makurazaki, yamagawa, uchinoura,
                   akune_bouuke, uchinoura_bouuke) %>%
    dplyr::mutate(Prefec = "鹿児島",
                  Species = "マアジ") %>%
    dplyr::select(Year, Month, Prefec, Location,
                  Fishery, Species, Catch_ton)

}
