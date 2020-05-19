fname <- "tests/testthat/excel/gyokaikyo_dummy_maaji_length_dist.xlsx"

parse_legacy_maaji_length_dist <- function(fname) {
  lucifer::rebel(
    path = fname,
    sheet_regex = "鹿児島尾数|.～.|千葉以北$",
    cluster = list(
      dir = "v",
      pos = 3,
      regex = "^年$",
      offset = c(0, 0),
      ends = list(
        col = "^44$",
        row = ".+"
      )
    )
  ) %>%
    dplyr::select(-海域コード) %>%
    tidyr::pivot_longer(cols = c(-年, -月, -海域, -fname, -sheet),
                        names_to = "Length",
                        values_to = "Frequency") %>%
    dplyr::mutate(Year = as.integer(stringr::str_extract(年, "\\d{4}")),
                  Month = as.integer(stringr::str_extract(月, "\\d?\\d")),
                  Length = as.integer(Length),
                  Frequency = as.numeric(Frequency),
                  Region = as.factor(海域),
                  Fname = fname,
                  Sheet = sheet) %>%
    dplyr::select(Year, Month, Region, Length, Frequency, Fname, Sheet)
}
