################################################################################
#                       Functions for data preprossing                         #
################################################################################

#' Create example data.
#'
#' This function is used to generate mobile health sensor data as an example
#' data set for the use in functions of the mHealthieR package.
#'
#' @param percentage_NA Percentage of missing valuess (NAs) in example data
#' @export
generate_data <- function(percentage_NA = 0.2){
  # create keys, values, and dates
  keys <- rep(c(paste('key', c(1:10), sep = '')),10)

  set.seed(1)
  values <- sample(c(500:30000), size = 100)
  set.seed(1)
  values[sample(1:length(values),
                size = length(values) *percentage_NA)] <- NA

  dates <- seq.Date(from = as.Date('2015-03-09'), by='day', length.out = 10) %>%
    lapply(., function(date){rep(as.Date(date), 10)}) %>%
    Reduce(c, .)

  # concatinate these in a long tribble
  data_tb_long <- tibble(
    'key'   = keys,
    'date'  = dates,
    'value' = values
  )
}



#' Distinguish between wide or long table.
#'
#' This function distinguishes, if the input is in wide or long table format.
#' It also determines, if the first column is the data or key column.
#'
#' @param tb Data tibble which should be checked for the right format.
#' @export
check_format <- function(tb){
  # function to check, if the data is in the right format and give suggestions
  is_date <- function(entry) inherits(entry, 'Date')
  # func to check if a value is in date format
  if (length(tb) > 3){
    if (is_date(tb[[1,1]])){
      message('Please, set your time points as column names')
    }else if(mode(tb[[1,1]]) != 'character'){
      message('Please, set your IDs/keys as first column.')
    }else{
      message('Please, insert a table in the long table format.')
      message('(column1 = keys, column2 = time points, column3 = values)')
     return('wide')
    }
  }else{
    if (is_date(tb[[1,1]])){
      message('Please, use your IDs/keys as first column.')
    }else if(mode(tb[[1,1]]) != 'character'){
      message('Please, set your IDs/keys as first column.')
    }else if(!is_date(tb[[1,2]])){
      message('Please, set your time points as second column.')
    }else if(mode(tb[[1,3]]) != 'numeric'){
      message('Please, set your values as third column.')
    }else{
    return('long')
    }
  }
}




