parse_index <- function(fname, year, type) {
  tidy_data <- function(sht_rgx, clust_rgx, ofst,
                        col_hdrs = NULL, rowend, colend) {
    lucifer::rebel(fname, sheet_regex = sht_rgx,
                      cluster = list(dir = "v",
                                     pos = 2,
                                     regex = clust_rgx,
                                     offset = ofst,
                                     ends = list(col = colend,
                                                 row = rowend)),
                   col_headers = col_hdrs)
  }
  rename_cols <- function(df) {
    colnames(df) <- stringr::str_to_title(colnames(df))
    df %>%
      dplyr::rename(Month = `0`,
                    Place = `0.1`,
                    LogIndex = Value) %>%
      dplyr::mutate(
       Place    = stringr::str_remove(Place, "①|②|③|④|⑤|⑥"),
       Place    = factor(Place),
       Year     = as.integer(Year),
       LogIndex = as.double(LogIndex)
     )
  }

  if (type == "rawdata") {
    tidy_data(sht_rgx = "資源評価用table&graph",
              clust_rgx = "データ参照（資源評価用dataから）",
              ofst = c(1, -1),
              colend = as.character(year),
              rowend = ".+") %>%
      tidyr::pivot_longer(cols = c(-1, -2, -fname, -sheet),
                          names_pattern = "(2[0-9]{3})",
                          names_to = "Year") %>%
      rename_cols()
  } else if (type == "geomean") {
    tidy_data(sht_rgx = "漁海況用（'20年３月会議用）",
              clust_rgx = "宮崎南部定置網",
              ofst = c(0, -1),
              colend = ".+",
              rowend = as.character(year),
              col_hdrs = 1:3) %>%
      dplyr::rename(Year = topleft,
                    Fname = fname,
                    Sheet = sheet) %>%
      tidyr::pivot_longer(cols = c(-Year, -Fname, -Sheet),
                          names_to = "Place",
                          values_to = "Geomean") %>%
      tidyr::separate(Place, c("Place", "Period"), sep = "_(?=[0-9])") %>%
      dplyr::mutate(Geomean = as.double(Geomean))
  } else {
    stop("type must be either 'rawdata' or 'geomean'")
  }

}
