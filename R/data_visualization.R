################################################################################
#                        Functions to visualize data                           #
################################################################################

#' Plot cores over time.
#'
#' This function plots all core period over time in a binary fashion. 1 stands
#' for a value at a given day and 0 for a missing value.
#'
#' @param ... Any additional argument
#' @param core_tb tibble with cores in wide format which should be plotted
#' @export

plot_core <- function(core_tb, ...){
  # reformat the core_tb binary format
  bin_tb <- core_tb %>%
    dplyr::mutate(., value = ifelse(value >= 1, 1, 0)) %>%
    tidyr::spread(., date, value, fill = NA) %>%
    .[,-c(1)]

  bin_tb[is.na(bin_tb)] <- 0
  bin_heatmap <- ComplexHeatmap::Heatmap(bin_tb,
                                         na_col = "red",
                                         col = c('black','light grey'),
                                         show_column_names = FALSE,
                                         column_title = 'time points',
                                         row_title = 'individuals',
                                         column_title_side = "bottom",
                                         heatmap_legend_param =
                                           list(title = "existing\nvalues"),
                                         width = 2,
                                         cluster_rows = TRUE,
                                         cluster_columns = FALSE
  )
  message('Explaination: 1 stands for existing values ',
          'and 0 for a missing values')
  bin_heatmap
}


