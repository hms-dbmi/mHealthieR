################################################################################
#                      Functions for data clustering                           #
################################################################################

# The following function solves the '.' is not a global variable when
# using magrittr (https://github.com/tidyverse/magrittr/issues/29)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Determine the right number of senators for shape clustering.
#'
#' This function is designed to calculate the right number of senators for the
#' complexity reduction performed by the kmlShape reduceTraj() based on the
#' input data.
#'
#' @param data_tbl data tibble which the clustering should be performed on.
senators_function <- function(data_tbl){
  # calculates number of senator curves used to reduce the total number of trajectories

  # check if the input is a list and convert it to tibble
  if(is.vector(data_tbl)){
    data_tbl <- dplyr::bind_rows(data_tbl)
  }

  trajectories <- length(unique(data_tbl[[1]]))
  if(trajectories <= 100){
    return(trajectories)
  }else{
    senators <- round(trajectories * 0.1) # takes 10% of trajectories as senators
    return(senators)
  }
}

#' Determine the right number of curve description points for shape clustering.
#'
#' This function is designed to calculate the right number of points which
#' describes for the the shape for complexity reduction performed by the
#' kmlShape reduceTraj() based on the input data.
#'
#' @param data_tbl data tibble which the clustering should be performed on.
n_times_function <- function(data_tbl){
# sets number of points used to fit a curve to the trajectory

  # check if the input is a list and convert it to tibble
  if(is.vector(data_tbl)){
    data_tbl <- dplyr::bind_rows(data_tbl)
  }

  time_points <- length(unique(data_tbl[[2]]))
  if(time_points <= 250){
    return(time_points)
  }else if(time_points <= 500){
    n_time_points_half <- round(time_points/2)
    return(n_time_points_half)
  }else{
    n_time_points_percent <- round(time_points * 0.1)
    # represents all trajectories with 10% of the original number of data points
    return(n_time_points_percent)
  }
}


#' Cluster data according to shape via kmlShape algorithm.
#'
#' This function is designed to cluster longitudinal data. The clustering
#' algorithm implemented in the kmlShape and used here uses es shape-respecting
#' clustering distance (Frechet distance) and a shape-respecting mean.
#'
#' @param data_tbl data tibble which the clustering should be performed on.
#' @param shapes number of shape clusters.
#' @param n_senators number of senators used for complexity reduction.
#' @param n_times number of curve description points used for complexity reduction.
#' @param timescale_factor factor anywhere between 0 (DTW) and 1 (euclidean distance).
#' @param frechet_method method used to compute Frechet distance ('sum' or 'max').
#' @export
cluster_shapes <- function(data_tbl,
                           shapes = 3,
                           n_senators = senators_function(data_tbl),
                           n_times = n_times_function(data_tbl),
                           timescale_factor = 0.1,
                           frechet_method = 'sum'){
  # timescale_factor sets time scale for kmlShape. 0 = DTW till 1 = euclidean
  # frechet_method determines whether the Frechet distance and
  # Frechet path should be calculate with the 'sum' or 'max' method

  ### check if the input is a list and convert it to tibble
  if(is.vector(data_tbl)){
    data_tbl <- dplyr::bind_rows(data_tbl)
  }

  ### deal with times in POSIXct format and time factors
  if(lubridate::is.POSIXt(data_tbl[[1,2]])){
    data_tbl[[2]] <- as.integer(data_tbl[[2]])
    POSIXt_switch <- TRUE #switch later used to convert integers back to POSIXt
  }else{
    POSIXt_switch <- FALSE
  }

  ### calculate shapes with kmlShape
  data_tbl <- as.data.frame(data_tbl[c(1:3)])
  # trim input data in right format for clustering
  set.seed(1)
  Clds_long <- kmlShape::cldsLong(data_tbl)
  kmlShape::reduceTraj(Clds_long, nbSenators= n_senators, nbTimes=n_times)
  # complexity reduction
  cluster_result <- kmlShape::kmlShape(Clds_long, nbClusters = shapes,
                                       timeScale = timescale_factor,
                                       FrechetSumOrMax = frechet_method,
                                       toPlot = 'none')
                                      # add toPlot = 'none' to suppress plotting

  ### retrieve parts of clustering result
  clusters_assigned <- tibble::as_tibble(cluster_result@clusters)
  clusters_assigned$keys <- rownames(clusters_assigned)
  colnames(clusters_assigned) <- c('clusters', 'keys')
  clusters_assigned$clusters <- as.numeric(levels(clusters_assigned$clusters)
                                          )[clusters_assigned$clusters]


  mean_trajectories_tbl <- tibble::as_tibble(cluster_result@trajMeans)
  colnames(mean_trajectories_tbl) <- c('clusters', 'times', 'values')


  # switch time column back to POSIXct format
  if(POSIXt_switch){
    mean_trajectories_tbl[[2]] <- as.POSIXct(mean_trajectories_tbl[[2]],
                                             origin = '1970-01-01 00:00:00')
    #to convert numeric date back to POSIXct
  }

  cluster_result_list <- list(cluster_assignement = clusters_assigned,
                              detailed_result = cluster_result,
                              cluster_number = shapes,
                              mean_trajectories_tbl = mean_trajectories_tbl #,
                              #means_plot = cluster_means_plot,
                              #cluster_plots = cluster_plots_grid
                              )

  return(cluster_result_list)

}
