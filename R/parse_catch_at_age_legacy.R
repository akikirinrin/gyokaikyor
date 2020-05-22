#' Parse legacy Excel data into data frame
#'
#' @param legacy List which contains
#' \itemize{
#' \item{fname}
#' \item{spcs}
#' \item{year}
#' }
parse_catch_at_age_legacy <- function(legacy) {
  assert_that(has_name(legacy, c("fname", "spcs", "year")))

  tidy_caa <- function(name, pos, regex, offset) {
    lucifer::rebel(
      legacy$fname,
      sheet_regex = "\\u6642\\u671f\\u5225.+",
      cluster = list(
        dir = "v",
        pos = pos,
        regex = regex,
        offset = offset,
        ends = list(row = "3\\u6b73\\u4ee5\\u4e0a",
                    col = as.character(legacy$year))
      )
    ) %>%
      dplyr::rename(Age = topleft) %>%
      tidyr::pivot_longer(cols      = -Age,
                          names_to  = "Year",
                          values_to = "N_million_fishes") %>%
      dplyr::mutate(Age = factor(Age),
                    Year = as.integer(Year),
                    N_million_fishes = as.double(N_million_fishes),
                    Name = name)
  }

  dplyr::bind_rows(
    tidy_caa(
      name = "Major_ports",
      pos = 1,
      regex = "1~12\\u6708\\u5408\\u8a08\\uff08\\u9e7f\\u5150\\u5cf6\\u629c\\u304d\\uff09", # nolint
      offset = c(1, 0)
    ),
    tidy_caa(
      name = "Nourin_adjusted",
      pos = 2,
      regex = "\\u30c7\\u30fc\\u30bf\\uff08\\u767e\\u4e07\\u5c3e\\uff09\\u2190\\u8fb2\\u6797\\u7d71\\u8a08\\u3067\\u5f15\\u304d\\u5ef6\\u3070\\u3057\\u5f8c", # nolint
      offset = c(1, -1)
    )
  )
}

#' Parse maaji object into data frame
#'
#' @param catch_at_age_maaji Object instanciated by \code{legacy()}
parse.catch_at_age_maaji <- function(catch_at_age_maaji) {
  parse_catch_at_age_legacy(catch_at_age_maaji)
}
