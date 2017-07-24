################################################################################
#                   Functions to aggregate data chunks                         #
################################################################################

# The following function solves the '.' is not a global variable when
# using magrittr (https://github.com/tidyverse/magrittr/issues/29)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


# This function suppressses notes arising from column names used
# with magrittr pipes and dplyr functions.
globalVariables(c('timepoints', 'values'))

#' Aggregate longitudinal data to another time unit
#'
#' This function is designed to aggregate longitudinal data to another, bigger
#' time unit.
#'
#' @param data_tbl input data tibble.
#' @param aggregate_to unit to which the data should be aggregated to.
#' @param aggregation_method function which should be used to summarize
#'   the data per time frame (sum or max).
#' @export
aggregate_time_periods <- function(data_tbl, aggregate_to = NULL,
                                   aggregation_method = sum){
  if(is.vector(data_tbl)){ # check if the input is a list and convert it to tibble
    data_tbl <- dplyr::bind_rows(data_tbl)
  }
  ####-----------------------------------------------------------------------
  # is evaluated when the second column contains time points
  aggr_func_timecol2 <- function(time_pattern = '%Y-%m-%d %H:%M:%S',
                                 time_res = 'secs',
                                 # according to lubridate abbreviations,
                                 bigger_time_chunk = c('years', 'months',
                                                       'days', 'hours',
                                                       'minutes')){
    col_names <- colnames(data_tbl) # keep the column names
    colnames(data_tbl) <- c('IDs', aggregate_to ,'values')
    # colnames for this func

    aggregated_data <- data_tbl %>%
      dplyr::mutate(timepoints = format(data_tbl[[2]], time_pattern)) %>%
      dplyr::group_by(data_tbl[[1]], timepoints) %>%
      dplyr::summarize(aggregation_method(values, na.rm = TRUE)) %>%
      dplyr::ungroup()

    colnames(aggregated_data) <- c('IDs', aggregate_to , 'values')
    aggregated_data[[1]] <- gsub("\\_.*", "", aggregated_data[[1]]) # trim IDs
    attr(aggregated_data, 'time_resolution') <- time_res
    if(!is.null(chunk_descr)){
      if(chunk_descr %in% bigger_time_chunk){
        attr(aggregated_data, 'chunk_description') <- chunk_descr
      }
    }else{
      attr(aggregated_data, 'chunk_description') <- time_res
    }
    return(aggregated_data)
  }

  ####-----------------------------------------------------------------------

  # is evaluated when the fourth column contains time points
  aggr_func_timecol4 <- function(time_pattern = '%Y-%m-%d %H:%M:%S',
                                 time_res = 'secs',
                                 # according to lubridate abbreviations,
                                 bigger_time_chunk = c('years', 'months',
                                                       'days', 'hours',
                                                       'minutes')
  ){
    col_names <- colnames(data_tbl) # keep the column names
    colnames(data_tbl) <- c('IDs', aggregate_to ,'values',
                            col_names[4:length(col_names)])
    # colnames for this func

    aggregated_data <- data_tbl %>%
      dplyr::mutate(timepoints = format(data_tbl[[4]], time_pattern)) %>%
      dplyr::group_by(data_tbl[[1]], timepoints) %>%
      dplyr::summarize(aggregation_method(values, na.rm = TRUE)) %>%
      dplyr::ungroup()

    colnames(aggregated_data) <- c('IDs', aggregate_to , 'values')
    aggregated_data[[1]] <- gsub("\\_.*", "", aggregated_data[[1]]) # trim IDs
    attr(aggregated_data, 'time_resolution') <- time_res
    if(chunk_descr %in% bigger_time_chunk){
      attr(aggregated_data, 'chunk_description') <- chunk_descr
    }else{
      attr(aggregated_data, 'chunk_description') <- time_res
    }
    return(aggregated_data)
  }

  ####-----------------------------------------------------------------------

  aggregation_func <- function(aggregation_sub_func = aggr_func_timecol2){
    ##### one segment for every time resolution
    if(aggregate_to == 'years'){
      aggregated_data <- aggregation_sub_func(time_pattern = '%Y',
                                              time_res = 'years',
                                              bigger_time_chunk = c('years'))
      aggregated_data[[2]] <- as.integer(aggregated_data[[2]])
    }else if(aggregate_to == 'months'){
      aggregated_data <- aggregation_sub_func(time_pattern = '%Y-%m',
                                              time_res = 'months',
                                              bigger_time_chunk = c('years'))
      aggregated_data[[2]] <- paste0(aggregated_data[[2]], '-01')
      aggregated_data[[2]] <- as.POSIXct(aggregated_data[[2]])
      aggregated_data[[1]] <- paste(aggregated_data[[1]],
                                    format(aggregated_data[[2]], '%Y'),
                                    sep = '_')
      aggregated_data[[2]] <- as.integer(format(aggregated_data[[2]], '%m'))
    }else if(aggregate_to == 'days'){
      aggregated_data <- aggregation_sub_func(time_pattern = '%Y-%m-%d',
                                              time_res = 'days',
                                              bigger_time_chunk = c('years',
                                                                    'months'))
      aggregated_data[[2]] <- as.POSIXct(aggregated_data[[2]])
    }else if(aggregate_to == 'hours'){
      aggregated_data <- aggregation_sub_func(time_pattern = '%Y-%m-%d %H',
                                              time_res = 'hours',
                                              bigger_time_chunk = c('years',
                                                                    'months',
                                                                    'days'))
      aggregated_data[[2]] <- paste0(aggregated_data[[2]], ':00')
      aggregated_data[[2]] <- as.POSIXct(aggregated_data[[2]])
    }else if(aggregate_to == 'minutes'){
      aggregated_data <- aggregation_sub_func(time_pattern = '%Y-%m-%d %H:%M',
                                              time_res = 'minutes',
                                              bigger_time_chunk = c('years',
                                                                    'months',
                                                                    'days',
                                                                    'hours'))
      # converts character time points to POSIXct
      aggregated_data[[2]] <- paste0(aggregated_data[[2]], ':00')
      aggregated_data[[2]] <- as.POSIXct(aggregated_data[[2]])
    }

    return(aggregated_data)
  }

  ####--------------- main function-------------------------------------------


  chunk_descr <- attributes(data_tbl)$chunk_description
  if(is.null(aggregate_to)){
    stop('Please, specify the unit which the data should be aggregated into.')
  }else if(is.null(attributes(data_tbl)$time_resolution)){
    if(attributes(add_time_resolution(data_tbl))$time_resolution == aggregate_to){
      stop('Data has already the chosen time resolution.')
    }
  }

  if(length(data_tbl) > 3){
    if(lubridate::is.POSIXct(data_tbl[[1,4]])){
      aggregated_data <- aggregation_func(aggregation_sub_func = aggr_func_timecol4)
    }
  }else if(lubridate::is.POSIXct(data_tbl[[1,2]])){
    aggregated_data <- aggregation_func(aggregation_sub_func = aggr_func_timecol2)
  }else{
    stop('Please, set time points as second column or fourth column in POSIXct format.')
  }

  return(aggregated_data)
}

