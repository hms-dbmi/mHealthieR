################################################################################
#                   Functions to aggregate data chunks                         #
################################################################################

# The following function solves the '.' is not a global variable when
# using magrittr (https://github.com/tidyverse/magrittr/issues/29)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Aggregate longitudinal data to another time unit
#'
#' This function is designed to aggregate longitudinal data to another, bigger
#' time unit
#'
#' @param data_tbl input data tibble.
#' @param aggregate_to unit to which the data should be aggregated to.
#' @param aggregation_method function which should be used to summarize
#'   the data per time frame.
#' @export
aggregate_time_periods <- function(data_tbl, aggregate_to = NULL,
                                   aggregation_method = sum){
  if(is.vector(data_tbl)){ # check if the input is a list and convert it to tibble
    data_tbl <- dplyr::bind_rows(data_tbl)
  }

  if(is.null(aggregate_to)){
    stop('Please, specify the unit which the data should be aggregated into.')
  }else if(aggregate_to == 'day'){
    col_names <- colnames(data_tbl) # keep the column names
    grouped_days <- data_tbl %>%
      dplyr::mutate(`days` = as.POSIXct(format(data_tbl[[2]], '%Y-%m-%d'))) %>%
      dplyr::group_by(data_tbl[[1]], `days`) %>%
      dplyr::summarize(aggregation_method(value , na.rm = TRUE)) %>%
      dplyr::ungroup()

    colnames(grouped_days) <- col_names
    return(grouped_days)
  }
}
