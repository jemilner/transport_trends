#' Get earliest value of col for a region
#'
#' @param data_tbl 
#' @param region A region id (integer)
#' @param col A column name in data_tbl 
get_baseline_region <- function(data_tbl, region, col) {
    data_tbl %>%
        filter(region_id == region) %>%
        filter(year == min(year)) %>%
        pull(col)
}

#' Get earliest value of col
#'
#' @param data_tbl 
#' @param col A column name in data_tbl 
get_baseline_national <- function(data_tbl, col) {
    data_tbl %>%
        filter(year == min(year)) %>%
        pull(col)
}