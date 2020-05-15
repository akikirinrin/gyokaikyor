legacy <- function(fname, year, spcs) {
  structure(list(fname = fname, year = year, spcs = spcs), class = spcs)
}

#' Parse pseudo database of gyokaikyo project into data frame
#'
#' Note that this function is for maiwashi and maaji
#'
#' @inheritParams parse_legacy.sabarui
#' @param fname Name of legacy catch database file of maiwashi and maaji
parse_legacy <- function(x) {
    lucifer::rebel(x$fname, sheet_regex = generate_prefec(),
                   cluster = list(dir = "v",
                                  pos = 2,
                                  regex = "\\u6708\\uff3c\\u5e74", #月＼年
                                  offset = c(0, 0),
                                  ends = list(row = "12",
                                              col = as.character(x$year)),
                                  info = list(value_offset = c(1, -1),
                                              value_dim = c(1, 1))),
                   crop = list(direction = "v",
                               pos = 1,
                               regex = "以下入力データシート",
                               use_after = FALSE)) %>%
    dplyr::rename(
      month = stringi::stri_unescape_unicode("\\u6708\\\\\\u5e74"), #月\年
      fishery = key1
    ) %>%
    dplyr::select(month, fishery, 漁法, fname, sheet,
                  dplyr::matches("\\d{4}")) %>%
    tidyr::gather(-month, -fishery, -fname, -sheet,
                  key = "year", value = "catch_ton") %>%
    dplyr::mutate(species = x$spcs,
                  year = as.integer(year),
                  month = as.integer(month),
                  prefec = sheet) %>%
    dplyr::filter(!is.na(fishery) &
                    !is.na(year)) %>%
    finalize_df()
}

parse <- function(x) {
  UseMethod("parse")
}
parse.maaji <- function(x) {
  parse_legacy(x) %>%
    dplyr::mutate(Catch_ton = dplyr::if_else(Prefec == "静岡",
                                             as.double(Catch_ton) / 1000,
                                             as.double(Catch_ton)))
}

getdata <- function(fname, year, spcs) {
  legacy(fname, year = year, spcs = spcs) %>%
    parse()
}
