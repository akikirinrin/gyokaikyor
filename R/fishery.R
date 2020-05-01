#' Standardize duplicated- or molformed- fishery name
#'
#' @param fishery Column \code{Fishery} of catch data
standardize_fishery <- function(fishery) {
  fishery %>%
    stringr::str_replace_all("大型定置$",       "大型定置網") %>%
    stringr::str_replace_all("大中まき網",      "大中型まき網") %>%
    stringr::str_replace_all("1そうまき網",     "１そうまき網") %>%
    stringr::str_replace_all("船曳網?",         "船びき網") %>%
    stringr::str_replace_all("沖底|沖合底曳網", "沖合底びき網") %>%
    stringr::str_replace_all("^釣$",            "釣り") %>% # not found in nourin
    stringr::str_replace_all("棒受け網",        "棒受網") %>%
    stringr::str_replace_all("その他の漁業",    "その他") %>% # その他の網漁業 or その他の漁業 nolint
    stringr::str_replace_all("全漁業種",        "全漁法") # not found in nourin
}
