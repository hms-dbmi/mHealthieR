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
    'key'   = keys,
    'time'  = times,
    'value' = values
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
    'key'   = keys,
    'time'  = times,
    'value' = values
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
    'key'   = keys,
    'time'  = times,
    'value' = values
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
    'key'   = keys,
    'time'  = times,
    'value' = values
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
  is_date <- function(entry) inherits(entry, 'POSIXct')
  # func to check if a value is in date format
  if (length(data_tbl) > 3){
    if (is_date(data_tbl[[1,1]])){
      message('Please, set your time points as column names')
    }else if(mode(data_tbl[[1,1]]) != 'character'){
      message('Please, set your IDs/keys as first column.')
    }else{
      message('Please, insert a table in the long table format.')
      message('(column1 = keys, column2 = time points, column3 = values)')
     return('wide')
    }
  }else{
    if (is_date(data_tbl[[1,1]])){
      message('Please, use your IDs/keys as first column.')
    }else if(mode(data_tbl[[1,1]]) != 'character'){
      message('Please, set your IDs/keys as first column.')
    }else if(!is_date(data_tbl[[1,2]])){
      message('Please, set your time points as second column.')
    }else if(mode(data_tbl[[1,3]]) != 'numeric'){
      message('Please, set your values as third column.')
    }else{
    return('long')
    }
  }
}




