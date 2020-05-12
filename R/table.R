#' Generate catch table for report
#'
#' @param df Data frame with
#' \code{c("Year", "Species", "Prefec", "Month", "Catch_ton")}
#' @param spcs Character of species name to extract
#' @param year Year to extract in numeric
#' @inheritParams kableExtra::kable_styling
#' @examples
#' \dontrun{
#' make_table(load_catch_dummy_kagoshima(), spcs = "マアジ", year = 2019)
#' make_table(stockdbr::load_catch(), spcs = "マアジ", year = 2019)
#' }
#' @importFrom assertthat assert_that has_name
#' @export
make_table <- function(df, spcs, year, format = "html") {

  assert_that(
    has_name(df, c("Year", "Species", "Prefec", "Month", "Catch_ton"))
  )

  filter_mutate_ <- function(dat) {
    dat %>%
      dplyr::filter(Species %in% spcs, Year == year) %>%
      dplyr::mutate(pref_reversed = forcats::fct_relevel(Prefec,
                                                         rev(levels(Prefec))),
                    Month = as.character(Month)) # To create sumrow later
  }

  body <- df %>%
    filter_mutate_() %>%
    dplyr::group_by(Year, Month, pref_reversed) %>%
    dplyr::summarize(CatchSum = sum(Catch_ton, na.rm = TRUE) %>%
                       round(1)) %>%
    tidyr::pivot_wider(names_from = pref_reversed, values_from = CatchSum) %>%
    dplyr::ungroup()

  sumrow <- body %>%
    dplyr::select(-Year, -Month) %>%
    dplyr::summarize_all(sum, na.rm = TRUE) %>%
    dplyr::mutate(Year = year, Month = "Sum")

  sumcol <- df %>%
    filter_mutate_() %>%
    dplyr::group_by(Year, Month) %>%
    dplyr::summarize(Sum = sum(Catch_ton, na.rm = TRUE) %>%
                     round(1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(Sum)

  dplyr::bind_cols(body, sumcol) %>%
    dplyr::bind_rows(c(sumrow, data.frame(Sum = sum(sumcol)))) %>%
    knitr::kable(booktabs = TRUE, format = format) %>%
    kableExtra::kable_styling(font_size = 5)
}
