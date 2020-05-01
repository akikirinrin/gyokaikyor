#' Draw a time series catch plot for each region
#'
#' @param data Tidy catch data sorted by the business requirements
#' @param Regional Name of a Region separated by a boundary_prefec
#'
#' @return ggplot figure
each_graph <- function(data, regional) {
  ggplot2::ggplot(dplyr::filter(data, Region == regional)) +
    ggplot2::geom_col(ggplot2::aes(Date, Catch, fill = Prefec)) +
    ggplot2::facet_wrap(~Region, nrow = 1) +
    ggplot2::theme_minimal()
}

#' Draw a time vs catch plot separated into two regions
#'
#' @param data Tidy catch data sorted by the business requirements
#' @param species_regex Species regex to extract target species
#' @param boundary_prefec Eastmost prefec of the Western region
#' @param unit unit of y-axis chosen from c("ton", "kiloton", "manton")
#'
#' @return ggplot figure
#' @export
regional_graph <- function(data, species_regex, boundary_prefec, unit = "ton") {

  species_list <- unique(data$Species)
  target_species <- stringr::str_extract(species_list, species_regex)

  dat <-
    data %>%
    dplyr::filter(Species %in% target_species) %>%
    dplyr::mutate(
      Region = stockdbr::set_region_by_two(Prefec,
                                           boundary_prefec = boundary_prefec),
      Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    dplyr::group_by(Prefec, Region, Species, Year, Month, Date) %>%
    dplyr::summarise(
      Catch = dplyr::if_else(unit == "kiloton",
                             sum(Catch_ton / 1000, na.rm = T),
                             dplyr::if_else(unit == "manton",
                                            sum(Catch_ton / 10000, na.rm = T),
                                            sum(Catch_ton, na.rm = T))))

  region_list <- unique(dat$Region)

  g1 <- ggplot2::ggplotGrob(each_graph(dat, regional = region_list[1]))
  g2 <- ggplot2::ggplotGrob(each_graph(dat, regional = region_list[2]))

  grid::grid.newpage()
  grid::grid.draw(rbind(g1, g2))

}
