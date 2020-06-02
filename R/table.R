#' Generate catch table for report
#'
#' @param df Data frame with
#' \code{c("Year", "Species", "Prefec", "Month", "Catch_ton")}
#' @param spcs Character of species name to extract
#' @param year Year to extract in numeric
#' @param format Select format output
#' @param header TRUE to include header, FALSE to remove header
#' @param Japanese TRUE to convert Year, Month, Sum to Japanese
#' @inheritParams kableExtra::kable_styling
#' @examples
#' \dontrun{
#' make_table(load_catch_dummy_kagoshima(),
#'            spcs = "\u30de\u30a2\u30b8", year = 2019)
#' make_table(stockdbr::load_catch(), spcs = "\u30de\u30a2\u30b8", year = 2019)
#' }
#' @importFrom assertthat assert_that has_name
#' @export
df2table <- function(df, spcs = NULL, year, format = "html",
                     header = TRUE, Japanese = FALSE) {
  assert_that(
    has_name(df, c("Year", "Species", "Prefec", "Month", "Catch_ton"))
  )

  if (is.null(spcs)) spcs <- unique(df$Species)

  factorize_monthcol <- function(month_or_sum, Japanese = FALSE) {
    if (Japanese == TRUE) {
      factor(month_or_sum, levels = c(1:12, "合計"))
    }else{
      factor(month_or_sum, levels = c(1:12, "Sum"))
      }
    }

  filtrate <- function() {
    df %>%
      dplyr::filter(Species %in% spcs, Year == year) %>%
      dplyr::mutate(pref_reversed = forcats::fct_relevel(Prefec,
                                                         rev(levels(Prefec))),
                    Month = factorize_monthcol(Month))
  }

  make_tbl_body <- function() {
    filtrate() %>%
      dplyr::group_by(Year, Month, pref_reversed) %>%
      dplyr::summarize(CatchSum = sum(Catch_ton, na.rm = TRUE) %>%
                         round(1)) %>%
      tidyr::pivot_wider(names_from = pref_reversed, values_from = CatchSum) %>%
      dplyr::ungroup()
  }

  make_sumrow <- function() {
    make_tbl_body() %>%
      dplyr::select(-Year, -Month) %>%
      dplyr::summarize_all(sum, na.rm = TRUE) %>%
      dplyr::mutate(Year = year,
                    Month = dplyr::if_else(Japanese == TRUE,
                                           factorize_monthcol("合計",
                                                              Japanese == TRUE),
                                           factorize_monthcol("Sum")))
  }

  make_sumcol <- function() {
    filtrate() %>%
      dplyr::group_by(Year, Month) %>%
      dplyr::summarize(Sum = sum(Catch_ton, na.rm = TRUE) %>%
                       round(1)) %>%
      dplyr::ungroup() %>%
      dplyr::select(Sum)
  }

  make_alltotal <- function() {
      data.frame(Sum = sum(make_sumcol()))
  }

  top <- dplyr::bind_cols(make_tbl_body(), make_sumcol())
  btm <- suppressWarnings(dplyr::bind_cols(make_sumrow(), make_alltotal()))

  if (Japanese == TRUE) {
    suppressWarnings(
  tabledata <-
    dplyr::bind_rows(top, btm) %>%
    dplyr::rename(年 = Year, 月 = Month, 合計 = Sum)
    )
  }else{
  tabledata <-
    dplyr::bind_rows(top, btm)
  }

  if (header == FALSE) {
      tabledata %>%
        knitr::kable(booktabs = TRUE, format = format, col.names = NULL) %>%
        kableExtra::kable_styling(font_size = 7,
                                  bootstrap_options = "condensed")
  }else{
    tabledata %>%
      knitr::kable(booktabs = TRUE, format = format) %>%
      kableExtra::kable_styling(font_size = 7,
                                bootstrap_options = "condensed")
    }
}
