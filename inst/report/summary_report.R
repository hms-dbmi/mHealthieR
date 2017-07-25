#' ---
#' title: Summary Report
#' author: " "
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output:
#'    html_document:
#'      theme: flatly
#'      toc: true
#'      highlight: zenburn
#'      self_contained: false
#' ---



#+ echo = FALSE, message = FALSE

# create appropriate data
create_data_tb_long_day_many <- function(resolution = 'hour'){
  percentage_NA <- 0.05
  keys <- rep(c(paste('key', c(1:4), sep = '')), 1000)

  set.seed(1)
  values <- sample(c(500:50000), size = 4000)
  set.seed(1)
  values[sample(1:length(values),
                size = length(values) *percentage_NA)] <- NA

  times <- seq.POSIXt(from = as.POSIXct('2015-03-09'), by= resolution, length.out = 1000) %>%
    lapply(., function(time){rep(as.POSIXct(time), 4)}) %>%
    Reduce(c, .)

  # concatinate these in a long tribble
  data_tb_long_days <- tibble::tibble(
    'keys'   = keys,
    'times'  = times,
    'values' = values
  )
  return(data_tb_long_days)
}

data_tb_long_large <- create_data_tb_long_day_many()
random_data <- data_tb_long_large[sample(nrow(data_tb_long_large), size = 3500),]

random_data_days <- aggregate_time_periods(random_data, aggregate_to = 'days')
weeks <- extract_weeks(random_data_days, impute_NA = TRUE)
clustering_result <- cluster_shapes(weeks)


#' This report displays the shape-based clustering result of mhealth data.
#'
#' ## Data Core Display
#' 1 represents an existing value at a certain time point and 0 a missing value.
#' All white fields represent missing timepoints.

#+ echo = FALSE, message = FALSE
plot_core(extract_cores(random_data, percentage_NA = 0.1))

#+ echo = FALSE, message = FALSE
#' ## Data clustering display options
#' ### Mean Plot
#+ echo = FALSE, message = FALSE
mean_plot <- plot_mean_shapes(clustering_result,
                              yaxis_unit = 'number of steps')
mean_plot

#' ### Detailed Plots
#+ echo = FALSE, message = FALSE
detail_plots <- plot_cluster_data(clustering_result = clustering_result,
                                  data_tbl = weeks)
detail_plots

#' ### Comparison of Detailed Plots
#+ echo = FALSE, message = FALSE
plots_grid <- plot_cluster_grid(cluster_plots_list = detail_plots,
                                clustering_result = clustering_result)

#' ### Number of Data Chunks per Cluster
#+ echo = FALSE, message = FALSE
cluster_histogram <- create_cluster_hist(clustering_result)
cluster_histogram


