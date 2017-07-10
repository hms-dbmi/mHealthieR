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
  if (check_format(data_tbl) == "long"){
    # calculates the first core periods per patient
    idents <- unique(data_tbl[[identifier_column_index]])
      # extracts all unique identifiers
    cores_list <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[identifier_column_index] == id),]  %>%
                  # extract values for a single id
                zoo::na.trim(.) # cuts of all NA from the start and end of the data_tbl
      element_df <- data.frame(column=integer(), time=character())
      element_list <- c()
      for(element in 1:nrow(sub_tb)){
        element_df <- rbind(element_df, sub_tb[element,])
        element_list <- append(element_list, sub_tb[element,'value'])
        if(mean(is.na(element_list)) <= percentage_NA){ # filter for % NAs in core
          core <- zoo::na.trim(element_df) # removes NAs at start and end of cores
        }
      }
      return(core)
    })
    if (impute_NA == TRUE){
      cores_list <- lapply(cores_list, function(core){
        core <- as.data.frame(core) %>%
                imputeTS::na.ma(. ,k=3, weighting = 'simple') %>%
                # simple moving average over e.g. 7 days window
                tibble::as.tibble(.)
      })
    }
    cores <- plyr::ldply(cores_list, data.frame) %>%
             # concatinate all cores into one tibble
             tibble::as.tibble(.)
    return(cores)
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
#' @param ... Any additional argument
#' @export
extract_weeks <- function(data_tbl, partial_weeks = FALSE, percentage_NA = 0,
                          start_monday = TRUE, ...){
  if (check_format(data_tbl) == "long"){
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),] # extract values for a single id
    })

    weeks_lists <- lapply(sub_tbs, function(sub){
      sub$wday_f <- as.integer(format(sub$time, '%u'))#calculates weekday factor
      sub$year_week_f <- format(sub$time, format = "%Y%W") # adds year&week factor
      sub <- zoo::na.trim(sub) # removes all NAs from start and end
      weeks <- split(sub, sub$year_week_f) # splits single weeks
      return(weeks)
    })

    if(partial_weeks != TRUE){
      # filters for whole weeks if partial_weeks parameter set to TRUE
      weeks_lists <- lapply(weeks_lists,function(weeks_list){
        weeks_list <- lapply(weeks_list, function(week){
          if(week[nrow(week),4] == 7){
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
  return(weeks_list_filt)
}


#' Extract year from data tibble.
#'
#' This function extracts data chunks per year.
#'
#' @param data_tbl data tibble
#' @param partial_year if TRUE incomplete years are also included in the result
#' @param percentage_NA if set to 0.1 -> 10\% missing values are allowed per year
#' @param ... Any additional argument
#' @export
extract_years <- function(data_tbl, partial_year = FALSE, percentage_NA = 0, ...){
  if (check_format(data_tbl) == "long"){
    idents <- unique(data_tbl[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- data_tbl[which(data_tbl[1] == id),] # extract values for a single id
    })

    years_lists <- lapply(sub_tbs, function(sub){
      sub$year_f <- as.integer(format(sub$time, '%Y'))#calculates year factor
      sub <- zoo::na.trim(sub) # removes all NAs from start and end
      years <- split(sub, sub$year_f) # splits single weeks
      return(years)
    })

    if( partial_year != TRUE){
      years_lists <- lapply(years_lists, function(years){
        years <- lapply(years, function(year){
          year_start <- as.Date(year[[1,2]])
          year_end <- as.Date(year[[nrow(year),2]])
          date_diff <- as.integer(year_end - year_start)
          if(leap_year(year_end)){ # checks if the year is a leap year
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
  return(years_list_filt)
  }
}



