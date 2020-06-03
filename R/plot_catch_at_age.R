#' Plot catch at age data
#'
#' @param df Data frame
#' @param type "Nourin_adjusted" or "Major_port"
#' @import ggplot2
#' @export
plot_catch_at_age <- function(df, type) {
  assert_that(has_name(df, c("Year", "Age", "N_million_fishes")))

  type_expected <- c("Major_ports", "Nourin_adjusted")
  if (type %not_in% type_expected)
    stop("Argument 'type' should be one of: '",
         paste(type_expected, collapse = "', '"), "'.")

  df %>%
    dplyr::filter(Name == type) %>%
    ggplot(aes(Year, N_million_fishes, fill = Age)) +
    geom_bar(stat = "identity", color = "black", size = 0.3)
}
