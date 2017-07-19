################################################################################
#                       Functions for data preprossing                         #
################################################################################

#' Create example data at a daily level.
#'
#' This function is used to generate mobile health sensor data as an example
#' data set for the use in functions of the mHealthieR package.
#'
#' @param percentage_NA Percentage of missing valuess (NAs) in example data.
#' @export
generate_day_data <- function(percentage_NA = 0.2){
  # create keys, values, and times
  keys <- rep(c(paste('key', c(1:10), sep = '')),10)

  set.seed(1)
  values <- sample(c(500:30000), size = 100)
  set.seed(1)
  values[sample(1:length(values),
                size = length(values) *percentage_NA)] <- NA

  times <- seq.POSIXt(from = as.POSIXct('2015-03-09'), by='day', length.out = 10) %>%
    lapply(., function(time){rep(as.POSIXct(time), 10)}) %>%
    Reduce(c, .)

  # concatinate these in a long tribble
  data_tb_long <- tibble::tibble(
    'keys'   = keys,
    'times'  = times,
    'values' = values
  )
}

#' Create example data at a second level.
#'
#' This function is used to generate mobile health sensor data as an example
#' data set for the use in functions of the mHealthieR package.
#'
#' @param percentage_NA Percentage of missing valuess (NAs) in example data.
#' @export
generate_second_data <- function(percentage_NA = 0.2){
  # create keys, values, and times
  keys <- rep(c(paste('key', c(1:10), sep = '')),10)

  set.seed(1)
  values <- sample(c(500:30000), size = 100)
  set.seed(1)
  values[sample(1:length(values),
                size = length(values) *percentage_NA)] <- NA

  times <- seq.POSIXt(from = as.POSIXct('2015-03-09 00:00:00'),
                      by='sec', length.out = 10) %>%
    lapply(., function(time){rep(as.POSIXct(time), 10)}) %>%
    Reduce(c, .)

  # concatinate these in a long tribble
  data_tb_long <- tibble::tibble(
    'keys'   = keys,
    'times'  = times,
    'values' = values
  )
}


#' Create example data with 500 days per individual.
#'
#' This function is used to generate mobile health sensor data as an example
#' data set for the use in functions of the mHealthieR package.
#'
#' @param percentage_NA Percentage of missing valuess (NAs) in example data.
#' @export
generate_year_data <- function(percentage_NA = 0.2){
  # create keys, values, and times
  keys <- rep(c(paste('key', c(1:3), sep = '')), 500)

  set.seed(1)
  values <- sample(c(500:30000), size = 1500)
  set.seed(1)
  values[sample(1:length(values),
                size = length(values) *percentage_NA)] <- NA

  times <- seq.POSIXt(from = as.POSIXct('2015-01-01'),
                      by='day', length.out = 500) %>%
    lapply(., function(time){rep(as.POSIXct(time), 3)}) %>%
    Reduce(c, .)

  # concatinate these in a long tribble
  data_tb_long <- tibble::tibble(
    'keys'   = keys,
    'times'  = times,
    'values' = values
  )
}



#' Create hourly example data.
#'
#' This function is used to generate mobile health sensor data as an example
#' data set for the use in functions of the mHealthieR package.
#'
#' @param percentage_NA Percentage of missing valuess (NAs) in example data.
#' @export
generate_hour_data <- function(percentage_NA = 0.2){
  # create keys, values, and times
  keys <- rep(c(paste('key', c(1:3), sep = '')), 500)

  set.seed(1)
  values <- sample(c(0:5000), size = 1500)
  set.seed(1)
  values[sample(1:length(values),
                size = length(values) *percentage_NA)] <- NA

  times <- seq.POSIXt(from = as.POSIXct('2015-01-01'),
                      by='hour', length.out = 500) %>%
    lapply(., function(time){rep(as.POSIXct(time), 3)}) %>%
    Reduce(c, .)

  # concatinate these in a long tribble
  data_tb_long <- tibble::tibble(
    'keys'   = keys,
    'times'  = times,
    'values' = values
  )
}



#' Distinguish between wide or long table.
#'
#' This function distinguishes, if the input is in wide or long table format.
#' It also determines, if the first column is the data or key column.
#'
#' @param data_tbl Data tibble which should be checked for the right format.
#' @export
check_format <- function(data_tbl){
  # function to check, if the data is in the right format and give suggestions
  is_POSIXct <- function(entry) inherits(entry, 'POSIXct')
  # func to check if a value is in date format
  if (length(data_tbl) > 3){
    if (is_POSIXct(data_tbl[[1,1]])){
      stop('Please, set your time points as column names.')
    }else if(mode(data_tbl[[1,1]]) != 'character'){
      stop('Please, set your IDs/keys as first column in character format.')
    }else{
      stop('Please, insert a table in the long table format.',
           '(column1 = keys (character), column2 = time points (POSIXct),',
           'column3 = values (numeric))')
     return('wide')
    }
  }else{
    if (is_POSIXct(data_tbl[[1,1]])){
      stop('Please, use your IDs/keys as first column in character format.')
    }else if(mode(data_tbl[[1,1]]) != 'character'){
      stop('Please, set your IDs/keys as first column in character format.')
    }else if(!is_POSIXct(data_tbl[[1,2]])){
      stop('Please, set your time points as second column in POSIXct format.')
    }else if(mode(data_tbl[[1,3]]) != 'numeric'){
      stop('Please, set your values as third column in numeric format.')
    }else{
    return('long')
    }
  }
}


#' Determine the time resolution of the input data tibble.
#'
#' This function is designed to determine the time resolution of the input data
#' tibble based on the first two unique time points. The determined time resolution
#' /(e.g. 'days'/) is then attached to the object as attribute 'time_resolution'.
#'
#' @param data_tbl Data tibble for which the time resolution should be determined.
#' @export
add_time_resolution <- function(data_tbl){
  uniq_timepoints <- sort(unique(data_tbl[[2]]))

  if(lubridate::is.POSIXct(uniq_timepoints[1])){
    time_diff <- uniq_timepoints[2] - uniq_timepoints[1]
    time_resolution <- attributes(time_diff)$units
    attr(data_tbl, 'time_resolution') <- time_resolution
  }else if(max(uniq_timepoints %in% c(8:365))){
    attr(data_tbl, 'time_resolution') <- 'yeardays'
  }else if(uniq_timepoints[1] %in% c(1:7)){
    attr(data_tbl, 'time_resolution') <- 'weekdays'
  }
  return(data_tbl)
}




