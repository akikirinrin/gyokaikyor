#' Separate Prefecs into Eastern and Western regions
#'
#' @param prefec Prefecture name in Japanese from the Pacific block
#' @param boundary_prefec Eastmost prefecture of Western Region
#'
#' @return Regionalized prefectures based on the boundary_prefec
#' @export
set_region_by_two <- function(prefec, boundary_prefec) {

  j_prefecs <- c("北海道", "青森", "岩手", "宮城", "福島", "茨城", "千葉",
                 "神奈川", "静岡", "愛知", "三重", "和歌山", "大阪", "徳島",
                 "高知", "愛媛", "大分", "宮崎", "鹿児島")

  if (any(prefec %in% j_prefecs) == FALSE) {
    stop(message = "Non Pacific block prefec is given to 'prefec'")
  }

  if (any(boundary_prefec %in% j_prefecs) == FALSE) {
    stop(message = "Non Pacific block prefec is given to 'boundary_prefec'")
  }

  boundary_number <- stringr::str_which(j_prefecs, paste(boundary_prefec))

  western_prefecs <- j_prefecs[boundary_number:length(j_prefecs)]

  dplyr::if_else(prefec %in% western_prefecs,
                 paste0(boundary_prefec, "以西"),
                 paste0(j_prefecs[boundary_number - 1], "以東"))

}


#' Denial of `%in%`
#' @inheritParams base::match
#' @param y vector or NULL: the values to be matched against
`%not_in%` <- function(x, y) {
  all(!(x %in% y))
}

#' Generate logarithmic sequence
#'
#' @inheritParams base::seq
#' @export
seq_log <- function(from, to = from, by = 1) {
  purrr::map_dbl(seq(from, to, by = by), function(x) 10 ^ x)
}
