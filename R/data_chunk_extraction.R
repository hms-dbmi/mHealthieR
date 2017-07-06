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
#' @param tb input data tibble.
#' @param percentage_NA percentage of allowed missing values a core period can have.
#' @param impute_NA If TRUE NAs will be imputed based on weighted mean in a
#'   certain window.
#' @param window_size values on both sides of the value which should be imputed
#'   (e.g. window_size =3 gives a 7 days window).
#' @param identifier_column_index insert index of colum which contains IDs/keys.
#' @importFrom magrittr "%>%"
#' @export

extract_cores <- function(tb,
                          impute_NA = FALSE,
                          percentage_NA = 0.1,
                          window_size = 3,
                          identifier_column_index = 1){
  if (check_format(tb) == "long"){
    # calculates the first core periods per patient
    idents <- unique(tb[[identifier_column_index]])
      # extracts all unique identifiers
    cores_list <- lapply(idents, function(id){
      sub_tb <- tb[which(tb[identifier_column_index] == id),]  %>%
                  # extract values for a single id
                zoo::na.trim(.) # cuts of all NA from the start and end of the tb
      element_df <- data.frame(column=integer(), date=character())
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
                as.tibble(.)
      })
    }
    cores <- plyr::ldply(cores_list, data.frame) %>%
             # concatinate all cores into one tibble
             as.tibble(.)
    return(cores)
  }
}



#' Extract weeks from tibble.
#'
#' This function extracts week long data chunks starting with Monday as first
#' day of the week.
#'
#' @param tb data tibble
#' @param part_week if TRUE incomplete weeks are also included in the result
#' @param weekNA if set to 1 -> 1 missing value is allowed per week
#' @param ... Any additional argument
#' @export
extract_weeks <- function(tb, part_week = FALSE, weekNA = 0, ...){
  if (check_format(tb) == "long"){
    idents <- unique(tb[[1]]) # extracts all unique identifiers
    sub_tbs <- lapply(idents, function(id){
      sub_tb <- tb[which(tb[1] == id),] # extract values for a single id
    })

    weeks_lists <- lapply(sub_tbs, function(sub){
    sub$wday_f <- as.integer(format(sub$date, '%u')) # calculates weekday factor
    sub <- zoo::na.trim(sub) # removes all NAs from start and end
    weeks <- split(sub,(as.numeric(rownames(sub)) -1) %/% 7)

    return(weeks)
      })

    if(part_week != TRUE){
      weeks_lists <- lapply(weeks_lists,function(weeks_list){
        weeks_list <- weeks_list[lapply(weeks_list,nrow) == 7]
        # filters for whole weeks
        return(weeks_list)
      })
    }
    weeks_lists_filt <- lapply(weeks_lists, function(wks){
      # filters for a defined number of missing days
      lapply(wks, function(wk){ if(sum(is.na(wk$value)) <= weekNA) return(wk)})
    })

    weeks_list_filt <- unlist(weeks_lists_filt, recursive = FALSE)

    weeks_list_filt <- Filter(Negate(is.null), weeks_list_filt)
    # drops all empty lists

    weeks_list_monday <- lapply(weeks_list_filt, function(week){
      if(week[1,4] == 1){
        # filter for all weeks which start with a Monday (wday factor = 1)
        return(week)
      }
    })
    weeks_list_monday_filt <- Filter(Negate(is.null), weeks_list_monday)
    # drops all empty lists
  }
  return(weeks_list_monday_filt)
}




