parse_index <- function(fname, year) {
  tidy_data <- function() {
    lucifer::rebel(fname, sheet_regex = "資源評価用table&graph",
                      cluster = list(dir = "v",
                                     pos = 2,
                                     regex = "データ参照（資源評価用dataから）",
                                     offset = c(1, -1),
                                     ends = list(col = as.character(year),
                                                 row = ".+")))
  }
  gather_years <- function(df) {
    tidyr::pivot_longer(df,
                        cols = c(-1, -2, -fname, -sheet),
                        names_pattern = "(2[0-9]{3})",
                        names_to = "Year")
  }
  rename_cols <- function(df) {
    colnames(df) <- stringr::str_to_title(colnames(df))
    df %>%
      dplyr::rename(WHAT_IS_THIS = `0`,
                    Place = `0.1`,
                    LogIndex = Value)
  }

  tidy_data() %>%
    gather_years() %>%
    rename_cols()
}
