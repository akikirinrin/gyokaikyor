#' Finalize data.frame to be in required format
#'
#' @param dat Data to be processed
finalize_df <- function(dat) {
  ideal_columns <-
    data.frame(Year             = integer(),
               Month            = integer(),
               Prefec           = character(),
               Location         = character(),
               Fishery          = character(),
               Species          = character(),
               Meigara          = character(),
               Catch_ton        = double(),
               Fname            = character(),
               sheet            = character(),
               Note             = character(),
               stringsAsFactors = FALSE)

  processed <- dat %>%
    magrittr::set_colnames(stringr::str_to_title(colnames(dat))) %>%
    dplyr::bind_rows(ideal_columns)

  unknown_column <-
    colnames(processed)[!(colnames(processed) %in% colnames(ideal_columns))]

  do_exist_unknown_column <- length(unknown_column) > 0

  if (do_exist_unknown_column)
    warning(paste0("Column '",
                   paste(unknown_column, collapse = ", '"),
                   "' was removed."))

  processed %>%
    dplyr::select(colnames(ideal_columns)) %>%
    dplyr::mutate(Prefec = factor(Prefec,
                                  levels = generate_prefec())) %>%
    dplyr::mutate(Fishery = standardize_fishery(Fishery)) %>%
    standardize_spcsname()
}
