################################################################################
#                     Functions to extract data chunks                         #
################################################################################

# The following function solves the '.' is not a global variable when
# using magrittr (https://github.com/tidyverse/magrittr/issues/29)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



#' Extract core periods with a certain allowed percentage of NAs.
#'
#' This function extract periods starting with the first value per individual
#' and ending after a certain percentage of missing values (NAs) is reached.
#'
#' @param data_tbl input data tibble.
#' @param percentage_NA percentage of allowed missing values a core period can have.
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 days window).
#' @param identifier_column_index insert index of colum which contains IDs/keys.
#' @importFrom magrittr "%>%"
#' @export
extract_cores <- function(data_tbl,
                          impute_NA = FALSE,
                          percentage_NA = 0.1,
                          window_size = 3,
                          identifier_column_index = 1){
  colnames(data_tbl) <- c('keys', 'times', 'values')
  if (check_format(data_tbl) == "long"){
    # calculates the first core periods per patient
    time_res <- attributes(add_time_resolution(data_tbl))$time_resolution
    idents <- unique(data_tbl[[identifier_column_index]])
      # extracts all unique identifiers
    cores_list <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[identifier_column_index] == id),]  %>%
                  # extract values for a single id
                zoo::na.trim(.) # cuts of all NA from the start and end of the data_tbl
      element_df <- data.frame(column=integer(), times=character())
      element_list <- c()
      for(element in 1:nrow(sub_tb)){
        element_df <- rbind(element_df, sub_tb[element,])
        element_list <- append(element_list, sub_tb[element,'values'])
        if(mean(is.na(element_list)) <= percentage_NA){ # filter for % NAs in core
          core <- zoo::na.trim(element_df) # removes NAs at start and end of cores
        }
      }
      return(core)
    })


    if (impute_NA == TRUE){
      cores_list <- lapply(cores_list, function(core){
        core <- as.data.frame(core) %>%
                imputeTS::na.ma(. ,k= window_size, weighting = 'simple') %>%
                # simple moving average over e.g. 7 days window
                tibble::as.tibble(.)
      })
    }


    cores <- plyr::ldply(cores_list, data.frame) %>%
             # concatinate all cores into one tibble
             tibble::as.tibble(.)

    attr(cores, 'time_resolution') <- time_res
    attr(cores, 'chunk_description') <- 'cores'
    attr(cores, 'time_factor') <- time_res
    return(cores)
  }
}

#' Extract years from data tibble.
#'
#' This function extracts one data chunk per year.
#'
#' @param data_tbl data tibble.
#' @param partial_year if TRUE incomplete years are also included in the result.
#' @param percentage_NA if set to 0.1 -> 10\% missing values are allowed per year.
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 days window).
#' @param ... Any additional argument.
#' @export
extract_years <- function(data_tbl,
                          partial_year = FALSE,
                          percentage_NA = 0,
                          impute_NA = FALSE,
                          window_size = 3,
                          ...){
  if (check_format(data_tbl) == "long"){

    time_res <- attributes(add_time_resolution(data_tbl))$time_resolution
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),]
      # extract values for a single id and stores it in a list
    })

    years_lists <- lapply(sub_tbs, function(sub){
      sub$year_f <- as.integer(format(sub[[2]], '%Y'))#calculates year factor
      sub$yearday <- lubridate::yday(sub[[2]])
      sub <- zoo::na.trim(sub) # removes all NAs from start and end
      years <- split(sub, sub$year_f) # splits single weeks
      return(years)
    })

    if(impute_NA == TRUE){
      # impute missing values
      years_lists <- lapply(years_lists,function(years){
        years <- lapply(years, function(year){
          year <- as.data.frame(year) %>%
            imputeTS::na.ma(. ,k = window_size, weighting = 'simple') %>%
            # simple moving average over e.g. 7 days window
            tibble::as.tibble(.)
          return(year)
        })
        return(years)
      })
    }


    if( partial_year != TRUE){
      years_lists <- lapply(years_lists, function(years){
        years <- lapply(years, function(year){
          year_start <- as.Date(min(year[[2]]))
          year_end <- as.Date(max(year[[2]]))
          date_diff <- as.integer(year_end - year_start)
          if(lubridate::leap_year(year_end)){ # checks if the year is a leap year
            if(date_diff == 365){
              return(year)
            }
          }else{
            if(date_diff == 364){
              return(year)
            }
          }
        })
      })
    }

    years_lists <- unlist(years_lists, recursive = FALSE)
    years_list_filt <- Filter(Negate(is.null), years_lists) # drops empty lists

    years_list_filt <- lapply( years_list_filt, function(year){
      # filters for a defined number of missing days per year
      if(mean(is.na(year[,3])) <= percentage_NA){
        return(year)}
    })

    years_list_filt <- Filter(Negate(is.null), years_list_filt) # drops empty lists

    if(length(years_list_filt) == 0){
      message('Year extraction not possible for this data set under the set parameters.')
    }

    years_tbl <- plyr::ldply(years_list_filt, data.frame) %>%
      # concatinate all weeks into one tibble
      tibble::as.tibble(.)

    #create identifier with original ID + year code
    years_tbl$keyTimefactor <- paste(years_tbl[[2]], years_tbl[[5]], sep='_')

    # reorder the colums because the factor is needed on position 2 for clustering
    years_tbl <- cbind(years_tbl[c(7,6,4,3,2,5)])

    # add attributes
    attr(years_tbl, 'time_resolution') <- time_res
    attr(years_tbl, 'chunk_description') <- 'years'
    attr(years_tbl, 'time_factor') <- 'yeardays'


    return(years_tbl)
  }
}


#' Extract months from data tibble.
#'
#' This function extracts one data chunk per month.
#'
#' @param data_tbl data tibble.
#' @param partial_month If TRUE incomplete months are also included in the result.
#'    This means that also months which do not have at least one data point in the
#'    first and the last day of the month are included in the result.
#' @param percentage_NA If set to 0.1 -> 10\% missing values are allowed per month.
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size Number of values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 months window).
#' @param ... Any additional argument.
#' @export
extract_months <- function(data_tbl,
                           partial_month = TRUE,
                           percentage_NA = 0,
                           impute_NA = FALSE,
                           window_size = 3,
                           ...){
  if (check_format(data_tbl) == "long"){

    time_res <- attributes(add_time_resolution(data_tbl))$time_resolution
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),]
      # extract values for a single id and stores it in a list
    })

    months_lists <- lapply(sub_tbs, function(sub){
      sub$day_f <- as.integer(format(sub[[2]], '%d'))#calculates day factor
      sub$year_f <- as.integer(format(sub[[2]], '%Y'))#calculates year factor
      sub$month_f <- as.integer(format(sub[[2]], '%m'))#calculates month factor
      months <- split(sub, f = list(sub$year_f, sub$month_f ))# splits single weeks
      return(months)
    })

    # filters for empty months due to unequal length
    months_lists <- lapply(months_lists, function(months){
      months <- lapply(months, function(month){
        if(length(month[[2]]) != 0){
          return(month)
        }
      })
      months <- Filter(Negate(is.null), months) # drops empty lists
      return(months)
    })

    months_lists <- lapply(months_lists, function(lists){
      # removes all NAs from start and end
      lists <- lapply(lists, function(ls){
        ls_trimmed <- zoo::na.trim(ls)
        return(ls_trimmed)
      })
      return(lists)
    })

    if (impute_NA == TRUE){
      # impute missing values
      months_lists <- lapply(months_lists, function(months){
        months <- lapply(months, function(month){
          month <- as.data.frame(month) %>%
            imputeTS::na.ma(. ,k = window_size, weighting = 'simple') %>%
            # simple moving average over e.g. 7 data points window
            tibble::as.tibble(.)
          return(month)
        })
        return(months)
      })
    }


    if( partial_month != TRUE){
      months_lists <- lapply(months_lists, function(months){
        months <- lapply(months, function(month){
          month_end_data <- max(month$day_f)
          # calc last day of calendar month
          month_end_real <- min(month[[2]])
          lubridate::day(month_end_real) <- 1
          lubridate::month(month_end_real) <- lubridate::month(month_end_real) + 1
          lubridate::day(month_end_real) <- lubridate::day(month_end_real) -1
          month_end_real <- as.numeric(format(month_end_real, '%d'))

          if(min(month$day_f) == 1){
            if(max(month$day_f) == month_end_real){
              # filters for all months with at least one entry in the first
              # and last hour of the month
              return(month)
            }
          }
        })
      })
    }



    months_lists <- unlist(months_lists, recursive = FALSE)
    months_list_filt <- Filter(Negate(is.null), months_lists) # drops empty lists

    months_list_filt <- lapply( months_list_filt, function(month){
      # filters for a defined number of missing months per month
      if(mean(is.na(month[,3])) <= percentage_NA){
        return(month)}
    })

    months_list_filt <- Filter(Negate(is.null), months_list_filt) # drops empty lists

    if(length(months_list_filt) == 0){
      stop('Month extraction not possible for this data set under the set parameters.')
    }

    months_tbl <- plyr::ldply(months_list_filt, data.frame) %>%
      # concatinate all weeks into one tibble
      tibble::as.tibble(.)

    #create identifier with original ID + month code
    months_tbl$keyTimefactor <- paste(months_tbl[[2]], months_tbl[[6]],
                                      months_tbl[[7]], sep='_')

    # reorder the colums because the factor is needed on position 2 for clustering
    months_tbl <- cbind(months_tbl[c(8,5,4,3,2,6,7)])

    # add attributes
    attr(months_tbl, 'time_resolution') <- time_res
    attr(months_tbl, 'chunk_description') <- 'months'
    attr(months_tbl, 'time_factor') <- 'monthdays'

    return(months_tbl)
  }
}


#' Extract weeks from tibble.
#'
#' This function extracts week long data chunks starting with Monday as first
#' day of the week.
#'
#' @param data_tbl data tibble
#' @param partial_weeks if TRUE incomplete weeks are also included in the result
#' @param percentage_NA if set to 0.1 -> 10\% missing values are allowed per week
#' @param start_monday if TRUE only weeks starting on Monday will be in the output
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 days window).
#' @param ... Any additional argument
#' @export
extract_weeks <- function(data_tbl, partial_weeks = FALSE, percentage_NA = 0,
                          start_monday = TRUE, impute_NA = FALSE,
                          window_size = 3, ...){
  if (check_format(data_tbl) == "long"){

    time_res <- attributes(add_time_resolution(data_tbl))$time_resolution
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),] # extract values for a single id
    })

    weeks_lists <- lapply(sub_tbs, function(sub){
      sub$wday_f <- as.integer(format(sub[[2]], '%u'))#calculates weekday factor
      sub$year_week_f <- format(sub[[2]], format = "%Y%W") # adds year&week factor
      sub <- zoo::na.trim(sub) # removes all NAs from start and end
      weeks <- split(sub, sub$year_week_f) # splits single weeks
      return(weeks)
    })

    if (impute_NA == TRUE){
      # impute missing values
      weeks_lists <- lapply(weeks_lists,function(weeks_list){
        weeks_list <- lapply(weeks_list, function(week){
          week <- as.data.frame(week) %>%
            imputeTS::na.ma(. ,k = window_size, weighting = 'simple') %>%
            # simple moving average over e.g. 7 days window
            tibble::as.tibble(.)
          return(week)
        })
        return(weeks_list)
      })
    }

    if(partial_weeks != TRUE){
      # filters for whole weeks if partial_weeks parameter set to TRUE
      weeks_lists <- lapply(weeks_lists,function(weeks_list){
        weeks_list <- lapply(weeks_list, function(week){
          if(max(week[[4]]) == 7){
            return(week)
          }
        })
        weeks_list <- Filter(Negate(is.null), weeks_list)
        return(weeks_list)
      })
    }

    weeks_lists_filt <- lapply(weeks_lists, function(wks){
      # filters for a defined number of missing days per week
      lapply(wks, function(wk){ if(mean(is.na(wk[,3])) <= percentage_NA){
        return(wk)}})
    })

    weeks_list_filt <- unlist(weeks_lists_filt, recursive = FALSE)

    weeks_list_filt <- Filter(Negate(is.null), weeks_list_filt)
    # drops all empty lists

    if(start_monday == TRUE){
      weeks_list_filt <- lapply(weeks_list_filt, function(week){
        if(week[1,4] == 1){
          # filter for all weeks which start with a Monday (wday factor = 1)
          return(week)
        }
      })
    }

    weeks_list_filt <- Filter(Negate(is.null), weeks_list_filt)
    # drops all empty lists

    if(length(weeks_list_filt) == 0){
      message('Week extraction not possible for this data set under the set parameters.')
    }
  }
  weeks_tbl <- plyr::ldply(weeks_list_filt, data.frame) %>%
    # concatinate all weeks into one tibble
    tibble::as.tibble(.)

  #create identifier with original ID + year code
  weeks_tbl$keyTimefactor <- paste(weeks_tbl[[2]], weeks_tbl[[6]], sep='_')

  # reorder the colums because the factor is needed on position 2 for clustering
  weeks_tbl <- cbind(weeks_tbl[c(7,5,4,3,2,6)])

  # add atrributes
  attr(weeks_tbl, 'time_resolution') <- time_res
  attr(weeks_tbl, 'chunk_description') <- 'weeks'
  attr(weeks_tbl, 'time_factor') <- 'weekday'


  return(weeks_tbl)
}



#' Extract days from data tibble.
#'
#' This function extracts one data chunk per day.
#'
#' @param data_tbl data tibble.
#' @param partial_day If TRUE incomplete days are also included in the result.
#'    This means that also days which do not have at least one data point in the
#'    first and the last hour of the day are included in the result.
#' @param percentage_NA if set to 0.1 -> 10\% missing values are allowed per day.
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 days window).
#' @param ... Any additional argument.
#' @export
extract_days <- function(data_tbl,
                         partial_day = TRUE,
                         percentage_NA = 0,
                         impute_NA = FALSE,
                         window_size = 3,
                         ...){
  if (check_format(data_tbl) == "long"){

    time_res <- attributes(add_time_resolution(data_tbl))$time_resolution
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),]
      # extract values for a single id and stores it in a list
    })

    days_lists <- lapply(sub_tbs, function(sub){
      sub$day_f <- as.integer(format(sub[[2]], '%d'))#calculates day factor
      sub$hour_f <- as.integer(format(sub[[2]], '%H'))#calculates hour factor
      sub$year_f <- as.integer(format(sub[[2]], '%Y'))#calculates year factor
      sub$month_f <- as.integer(format(sub[[2]], '%m'))#calculates month factor
      days <- split(sub, f = list(sub$year_f, sub$month_f, sub$day_f))
      # splits single weeks
      return(days)
    })

    days_lists <- lapply(days_lists, function(lists){
      # removes all NAs from start and end
      lists <- lapply(lists, function(ls){
        ls_trimmed <- zoo::na.trim(ls)
        return(ls_trimmed)
      })
      return(lists)
    })

    if (impute_NA == TRUE){
      # impute missing values
      days_lists <- lapply(days_lists, function(days){
        days <- lapply(days, function(day){
          day <- as.data.frame(day) %>%
            imputeTS::na.ma(. ,k = window_size, weighting = 'simple') %>%
            # simple moving average over e.g. 7 data points window
            tibble::as.tibble(.)
          return(day)
        })
        return(days)
      })
    }

    if( partial_day != TRUE){
      days_lists <- lapply(days_lists, function(days){
        days <- lapply(days, function(day){
          day_start <- min(day[[2]])
          day_end <- max(day[[2]])
          date_diff <- as.integer(day_end - day_start)
          if(date_diff >= 23){
            # filters for all days with at least one entry in the first
            # and last hour of the day
            return(day)
          }
        })
      })
    }


    days_lists <- unlist(days_lists, recursive = FALSE)
    days_list_filt <- Filter(Negate(is.null), days_lists) # drops empty lists

    days_list_filt <- lapply( days_list_filt, function(day){
      # filters for a defined number of missing days per day
      if(mean(is.na(day[,3])) <= percentage_NA){
        return(day)}
    })

    days_list_filt <- Filter(Negate(is.null), days_list_filt) # drops empty lists

    if(length(days_list_filt) == 0){
      stop('Day extraction not possible for this data set under the set parameters.')
    }

    days_tbl <- plyr::ldply(days_list_filt, data.frame) %>%
      # concatinate all weeks into one tibble
      tibble::as.tibble(.)

    #create identifier with original ID + day code
    days_tbl$keyTimefactor <- paste(days_tbl[[2]], days_tbl[[7]],
                                    days_tbl[[8]], days_tbl[[5]], sep='_')

    # reorder the colums because the factor is needed on position 2 for clustering
    days_tbl <- cbind(days_tbl[c(9,5,4,3,2,6,7,8)])

    # add attributes
    attr(days_tbl, 'time_resolution') <- time_res
    attr(days_tbl, 'chunk_description') <- 'days'
    attr(days_tbl, 'time_factor') <- 'dayhours'

    return(days_tbl)
  }
}


#' Extract hours from data tibble.
#'
#' This function extracts one data chunk per hour.
#'
#' @param data_tbl data tibble.
#' @param partial_hour If TRUE incomplete hours are also included in the result.
#'    This means that also hours which do not have at least one data point in the
#'    first and the last minute of the hour are included in the result.
#' @param percentage_NA If set to 0.1 -> 10\% missing values are allowed per hour.
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size Values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 data point window).
#' @param ... Any additional argument.
#' @export
extract_hours <- function(data_tbl,
                          partial_hour = TRUE,
                          percentage_NA = 0,
                          impute_NA = FALSE,
                          window_size = 3,
                          ...){
  if (check_format(data_tbl) == "long"){

    time_res <- attributes(add_time_resolution(data_tbl))$time_resolution
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),]
      # extract values for a single id and stores it in a list
    })

    hours_lists <- lapply(sub_tbs, function(sub){
      sub$day_f <- as.integer(format(sub[[2]], '%d'))#calculates day factor
      sub$hour_f <- as.integer(format(sub[[2]], '%H'))#calculates hour factor
      sub$year_f <- as.integer(format(sub[[2]], '%Y'))#calculates year factor
      sub$month_f <- as.integer(format(sub[[2]], '%m'))#calculates year factor
      sub$minute_f <- as.integer(format(sub[[2]], '%M'))#calculates year factor
      hours <- split(sub, f = list(sub$year_f, sub$month_f, sub$day_f, sub$hour_f))
      # splits single weeks
      return(hours)
    })

    hours_lists <- lapply(hours_lists, function(lists){
      # removes all NAs from start and end
      lists <- lapply(lists, function(ls){
        ls_trimmed <- zoo::na.trim(ls)
        return(ls_trimmed)
      })
      return(lists)
    })

    if (impute_NA == TRUE){
      # impute missing values
      hours_lists <- lapply(hours_lists, function(hours){
        hours <- lapply(hours, function(hour){
          hour <- as.data.frame(hour) %>%
            imputeTS::na.ma(. ,k = window_size, weighting = 'simple') %>%
            # simple moving average over e.g. 7 data points window
            tibble::as.tibble(.)
          return(hour)
        })
        return(hours)
      })
    }

    if( partial_hour != TRUE){
      hours_lists <- lapply(hours_lists, function(hours){
        hours <- lapply(hours, function(hour){
          hour_start <- as.integer(format(min(hour[[2]]), '%M'))
          hour_end <- as.integer(format(max(hour[[2]]), '%M'))
          date_diff <- sqrt(as.integer(hour_end - hour_start)^2)
          # calc magnitude of diff
          if(date_diff >= 59){
            # filters for all hours with at least one entry in the first
            # and last minute of the hour
            return(hour)
          }
        })
      })
    }

    hours_lists <- unlist(hours_lists, recursive = FALSE)
    hours_list_filt <- Filter(Negate(is.null), hours_lists) # drops empty lists

    hours_list_filt <- lapply( hours_list_filt, function(hour){
      # filters for a defined number of missing hours per hour
      if(mean(is.na(hour[,3])) <= percentage_NA){
        return(hour)}
    })

    hours_list_filt <- Filter(Negate(is.null), hours_list_filt) # drops empty lists

    if(length(hours_list_filt) == 0){
      stop('Hour extraction not possible for this data set under the set parameters.')
    }

    hours_tbl <- plyr::ldply(hours_list_filt, data.frame) %>%
      # concatinate all weeks into one tibble
      tibble::as.tibble(.)

    #create identifier with original ID + hour code
    hours_tbl$keyTimefactor <- paste(hours_tbl[[2]], hours_tbl[[7]],
                                     hours_tbl[[8]], hours_tbl[[5]],
                                     hours_tbl[[6]], sep='_')

    # reorder the colums because the factor is needed on position 2 for clustering
    hours_tbl <- cbind(hours_tbl[c(10,9,4,3,2,7,8,5,6)])

    # add attributes
    attr(hours_tbl, 'time_resolution') <- time_res
    attr(hours_tbl, 'chunk_description') <- 'hours'
    attr(hours_tbl, 'time_factor') <- 'hourminutes'

    return(hours_tbl)
  }
}

#' Extract minutes from data tibble.
#'
#' This function extracts one data chunk per minute.
#'
#' @param data_tbl data tibble.
#' @param partial_minute If TRUE incomplete minutes are also included in the result.
#'    This means that also minutes which do not have at least one data point in the
#'    first and the last second of the minute are included in the result.
#' @param percentage_NA If set to 0.1 -> 10\% missing values are allowed per minute.
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size Values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 data point window).
#' @param ... Any additional argument.
#' @export
extract_minutes <- function(data_tbl,
                            partial_minute = TRUE,
                            percentage_NA = 0,
                            impute_NA = FALSE,
                            window_size = 3,
                            ...){
  if (check_format(data_tbl) == "long"){

    time_res <- attributes(add_time_resolution(data_tbl))$time_resolution
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),]
      # extract values for a single id and stores it in a list
    })

    minutes_lists <- lapply(sub_tbs, function(sub){
      sub$day_f <- as.integer(format(sub[[2]], '%d'))#calculates day factor
      sub$hour_f <- as.integer(format(sub[[2]], '%H'))#calculates hour factor
      sub$year_f <- as.integer(format(sub[[2]], '%Y'))#calculates year factor
      sub$month_f <- as.integer(format(sub[[2]], '%m'))#calculates month factor
      sub$minute_f <- as.integer(format(sub[[2]], '%M'))#calculates minute factor
      sub$second_f <- as.integer(format(sub[[2]], '%S'))#calculates second factor
      minutes <- split(sub, f = list(sub$year_f, sub$month_f, sub$day_f, sub$hour_f,
                                     sub$minute_f)) # splits single weeks
      return(minutes)
    })

    minutes_lists <- lapply(minutes_lists, function(lists){
      # removes all NAs from start and end
      lists <- lapply(lists, function(ls){
        ls_trimmed <- zoo::na.trim(ls)
        return(ls_trimmed)
      })
      return(lists)
    })

    if (impute_NA == TRUE){
      # impute missing values
      minutes_lists <- lapply(minutes_lists, function(minutes){
        minutes <- lapply(minutes, function(minute){
          minute <- as.data.frame(minute) %>%
            imputeTS::na.ma(. ,k = window_size, weighting = 'simple') %>%
            # simple moving average over e.g. 7 data points window
            tibble::as.tibble(.)
          return(minute)
        })
        return(minutes)
      })
    }

    if( partial_minute != TRUE){
      minutes_lists <- lapply(minutes_lists, function(minutes){
        minutes <- lapply(minutes, function(minute){
          minute_start <- as.integer(format(min(minute[[2]]), '%S'))
          minute_end <- as.integer(format(max(minute[[2]]), '%S'))
          date_diff <- sqrt(as.integer(minute_end - minute_start)^2)
          # calc magnitude of diff
          if(date_diff >= 59){
            # filters for all minutes with at least one entry in the first
            # and last second of the minute
            return(minute)
          }
        })
      })
    }

    minutes_lists <- unlist(minutes_lists, recursive = FALSE)
    minutes_list_filt <- Filter(Negate(is.null), minutes_lists) # drops empty lists

    minutes_list_filt <- lapply( minutes_list_filt, function(minute){
      # filters for a defined number of missing minutes per minute
      if(mean(is.na(minute[,3])) <= percentage_NA){
        return(minute)}
    })

    minutes_list_filt <- Filter(Negate(is.null), minutes_list_filt) # drops empty lists

    if(length(minutes_list_filt) == 0){
      stop('Minute extraction not possible for this data set under the set parameters.')
    }

    minutes_tbl <- plyr::ldply(minutes_list_filt, data.frame) %>%
      # concatinate all weeks into one tibble
      tibble::as.tibble(.)

    #create identifier with original ID + minute code
    minutes_tbl$keyTimefactor <- paste(minutes_tbl[[2]], minutes_tbl[[7]],
                                       minutes_tbl[[8]], minutes_tbl[[5]],
                                       minutes_tbl[[6]], minutes_tbl[[9]], sep='_')

    # reorder the colums because the factor is needed on position 2 for clustering
    minutes_tbl <- cbind(minutes_tbl[c(11,10,4,3,2,7,8,5,6,9)])

    # add attributes
    attr(minutes_tbl, 'time_resolution') <- time_res
    attr(minutes_tbl, 'chunk_description') <- 'minutes'
    attr(minutes_tbl, 'time_factor') <- 'minuteseconds'

    return(minutes_tbl)
  }
}


