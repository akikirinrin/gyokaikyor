#' Parse pseudo database of gyokaikyo project into data frame
#'
#' Note that this function is for maiwashi and maaji
#'
#' @inheritParams parse_legacy.sabarui
#' @param fname Name of legacy catch database file of maiwashi and maaji
#' @export
parse_legacy.maiwashi_maaji <- function(fname, year, species) {
  spcs_expected <- c("マイワシ", "マアジ")
  if (species %not_in% spcs_expected) {
    stop(paste("Spcs name must be one of:",
               paste0(spcs_expected, collapse = ", ")))
  }
  lucifer::rebel(fname, sheet_regex = generate_prefec(),
                 cluster = list(dir = "v",
                                pos = 2,
                                regex = "月＼年",
                                offset = c(0, 0),
                                ends = list(row = "12",
                                            col = as.character(year)),
                                info = list(value_offset = c(1, -1),
                                            value_dim = c(1, 1))),
                 crop = list(direction = "v",
                             pos = 1,
                             regex = "以下入力データシート",
                             use_after = FALSE)) %>%
    dplyr::rename(month = `月\\年`,
                  fishery = key1) %>%
    dplyr::select(month, fishery, 漁法, fname, sheet,
                  dplyr::matches("\\d{4}")) %>%
    tidyr::gather(-month, -fishery, -fname, -sheet,
                  key = "year", value = "catch_ton") %>%
    dplyr::mutate(species = species,
                  year = as.integer(year),
                  month = as.integer(month),
                  prefec = sheet,
                  catch_ton = dplyr::if_else(species == "maaji" &
                                               prefec == "静岡",
                                             as.double(catch_ton) / 1000,
                                             as.double(catch_ton))) %>%
    dplyr::filter(!is.na(fishery) &
                    !is.na(year)) %>%
    finalize_df()
}
