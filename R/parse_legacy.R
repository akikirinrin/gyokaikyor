#' Constructor for legacy Excel data
#'
#' @param fname Legacy Excel data
#' @param year Newest year reported
#' @param spcs Spcies name such as: \itemize{\item{'maaji'}}
legacy <- function(fname, year, type, spcs) {
  structure(list(fname = fname, year = year, spcs = spcs,
                 data = NULL), class = paste(type, spcs, sep = "_"))
}

#' Parse legacy Excel data into data frame
#'
#' @param legacy List which contains
#' \itemize{
#' \item{fname}
#' \item{spcs}
#' \item{year}
#' }
parse_catch_legacy <- function(legacy) {
  assert_that(has_name(legacy, c("fname", "spcs", "year")))
  lucifer::rebel(legacy$fname, sheet_regex = generate_prefec(sep = "|"),
                 cluster = list(dir = "v",
                                pos = 2,
                                regex = "\\u6708\\uff3c\\u5e74", #月＼年
                                offset = c(0, 0),
                                ends = list(row = "12",
                                            col = as.character(legacy$year)),
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
  dplyr::mutate(species = legacy$spcs,
                year = as.integer(year),
                month = as.integer(month),
                catch_ton = as.double(catch_ton),
                prefec = sheet) %>%
  dplyr::filter(!is.na(fishery) &
                  !is.na(year)) %>%
  finalize_df()
}

parse <- function(x) {
  UseMethod("parse")
}

#' Parse maaji object into data frame
#'
#' @param catch_maaji Object instanciated by \code{legacy()}
parse.catch_maaji <- function(catch_maaji) {
  parse_catch_legacy(catch_maaji) %>%
    dplyr::mutate(Catch_ton = dplyr::if_else(Prefec == "静岡",
                                             Catch_ton / 1000,
                                             Catch_ton))
}

#' Convert legacy Excel to data frome
#'
#' @inheritParams legacy
#' @export
xl2df <- function(fname, year, type, spcs) {
  legacy(fname, year = year, type = type, spcs = spcs) %>%
    parse()
}
