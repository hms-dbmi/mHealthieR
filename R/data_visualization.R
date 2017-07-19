################################################################################
#                        Functions to visualize data                           #
################################################################################

# The following function solves the '.' is not a global variable when
# using magrittr (https://github.com/tidyverse/magrittr/issues/29)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Plot cores over time.
#'
#' This function plots all core period over time in a binary fashion. 1 stands
#' for a value at a given day and 0 for a missing value.
#'
#' @param core_tbl tibble with cores in wide format which should be plotted.
#' @param ... Any additional argument.
#' @export

plot_core <- function(core_tbl, ...){
  # retrieve time resolution
  time_res <- attributes(core_tbl)$time_resolution

  # reformat the core_tbl binary format
  bin_tbl <- core_tbl %>%
    dplyr::mutate(., values = ifelse(.[[3]] >= 1, 1, 0))
  bin_tbl[is.na(bin_tbl)] <- 0

  bin_heatmap <- ggplot2::ggplot(data = bin_tbl,  ggplot2::aes(x = bin_tbl[[2]],
                                                               y = bin_tbl[[1]])) +
    ggplot2::geom_tile( ggplot2::aes(fill = factor(bin_tbl[[3]]))) +
    ggplot2::scale_fill_manual(breaks =factor(bin_tbl[[3]]),
                               values = c('grey20', 'grey85'),
                               name = "existing\nvalue") +
    ggplot2::xlab(paste0('Time Points [', time_res, ']')) +
    ggplot2::ylab('Individuals') +
    ggplot2:: ggtitle('Data Core Plot') +
    ggplot2::theme(plot.background =  ggplot2::element_blank(),
                   panel.grid.minor =  ggplot2::element_blank(),
                   panel.grid.major =  ggplot2::element_blank(),
                   panel.background =  ggplot2::element_blank(),
                   panel.border =  ggplot2::element_blank())

  # if over 50 individuals are in the data set -> remove y labels
  if (length(unique(bin_tbl[[1]])) > 50){
    bin_heatmap <- bin_heatmap +
      ggplot2::theme(axis.text.y =  ggplot2::element_blank(),
                     axis.ticks.y =  ggplot2::element_blank())
  }

  message('Explaination: 1 stands for existing values ',
          'and 0 for a missing values')
  bin_heatmap
}

#' Display mean shape summary.
#'
#' This function is designed to display all mean shapes (every cluster) in a
#' summary plot over time.
#'
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param color_palette Specify the colorbrewer palette used for the cluster.
#'   display in the following style c(number of colors in the palette, palette name).
#' @param yaxis_unit Unit of values displayed on the y axis over time.
#' @param ... Any additional argument.
#' @importFrom grDevices "colorRampPalette"
#' @export
plot_mean_shapes <- function(clustering_result = NULL,
                             color_palette = c(12, 'Paired'),
                             yaxis_unit = NULL,
                             ...){

  # Retrieve the shapes the clustering was calculated on
  shapes <- clustering_result$cluster_number

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                                           color_palette[2]))(shapes)


  # split mean shape tbl in list
  mean_traj_tbl <- clustering_result$mean_trajectories_tbl
  mean_traj_list <-split(mean_traj_tbl, mean_traj_tbl$clusters)

  # create y axis label
  if(is.null(yaxis_unit)){
    yaxis_label <- 'Values'
  }else{
    yaxis_label <- paste0('Values [', yaxis_unit, ']')
  }

  # create mean shape summary plot
  cluster_means_plot <- plotly::plot_ly(mean_traj_list[[1]],
                                        type = 'scatter',
                                        x= ~times,
                                        y= ~values,
                                        mode = 'lines',
                                        name = 'cluster 1',
                                        line = list(color = color_scale[1]))


  for (shape in c(2:shapes)){

    cluster_means_plot <- plotly::add_trace(p= cluster_means_plot,
                                            x = mean_traj_list[[shape]]$times,
                                            y = mean_traj_list[[shape]]$values,
                                            name = paste('cluster',shape),
                                            line = list(color = color_scale[shape]))
  }

  cluster_means_plot <- cluster_means_plot %>%
    plotly::layout(title = 'Mean Shapes',
                   xaxis = list(title = paste0('Time Points [',
                                               clustering_result$time_resolution,
                                               ']')),
                   yaxis = list(title = yaxis_label))

  cluster_plot_result <- list( mean_shapes_plot = cluster_means_plot)

  return(cluster_plot_result)
}


#' Plot data per cluster in separate plot incl. mean shape of the cluster.
#'
#' This function is designed to plot the actual data for every cluster plus
#' the mean shape of the cluster.
#'
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param data_tbl data tibble which the clustering was performed on.
#' @param color_palette Specify the colorbrewer palette used for the cluster.
#'   display in the following style c(number of colors in the palette, palette name).
#' @param ... Any additional argument.
#' @export
plot_cluster_data <- function(clustering_result = NULL,
                              data_tbl = NULL, # the clustering was performed on
                              color_palette = c(12, 'Paired'),
                              ...){

  # retrieve the shapes the clustering was calculated on
  shapes <- clustering_result$cluster_number

  # check if the input is a list and convert it to tibble
  if(is.vector(data_tbl)){
    data_tbl <- dplyr::bind_rows(data_tbl)
  }

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                                           color_palette[2]))(shapes)

  # check with real data is available
  if(is.null(data_tbl)){
    stop('Please specify the data tbl which the shape clustering was performed on.')
  }

  # split mean shape tbl in list
  mean_traj_tbl <- clustering_result$mean_trajectories_tbl
  mean_traj_list <-split(mean_traj_tbl, mean_traj_tbl$clusters)

  # get real data based on clusters
  cluster_assignement <- tibble::as_tibble(clustering_result$cluster_assignement)


  cluster_groups <- split(cluster_assignement,
                          cluster_assignement$clusters)
  cluster_groups_realdata <- lapply(cluster_groups, function(cluster){
    keys <- c(cluster$keys)
    group <- tibble::as_tibble(data_tbl[which(data_tbl[[1]] %in% keys),])
    return(group)
  })

  # create list with one plot per cluster
  cluster_plots_list <- list()
  lapply(c(1:shapes), function(shape){
    color <- color_scale[shape]
    df <- cluster_groups_realdata[[shape]]
    traj <- mean_traj_list[[shape]]
    clust_plot <- ggplot2::ggplot(df, ggplot2::aes(df[[2]], df[[3]])) +
      ggplot2::geom_line( ggplot2::aes( group = df[[1]]),
                          colour =  ggplot2::alpha(color,
                                                   alpha = 0.7)) +
      ggplot2::geom_hline(colour = 'black', yintercept = mean(df[[3]])) +
      # add line at mean value
      ggplot2::xlab(paste0('Time Points [', clustering_result$time_resolution, ']')) +
      ggplot2::ylab('Values') +
      ggplot2::ggtitle(paste('Cluster ', shape )) +
      ggplot2::theme(legend.position="none",
                     panel.background =  ggplot2::element_blank(),
                     panel.grid.major =  ggplot2::element_line(colour = 'grey87'),
                     panel.grid.minor =  ggplot2::element_line(colour = 'grey87')) +
      ggplot2::geom_line(data = traj,
                         ggplot2::aes(traj[[2]],traj[[3]]),
                         colour =  ggplot2::alpha('grey28', alpha = 0.85),
                         size = 2.5)

    clust_plot <- clust_plot +  ggplot2::annotate('text',
                                                  x = min(df[[2]]),
                                                  y = mean(df[[3]] + df[[3]]*0.075),
                                                  label = 'average') +
      ggplot2::annotate('text',
                        x = max(df[[2]]) - max(df[[2]])*0.05 ,
                        y = traj[[3]][length(traj[[3]])] - traj[[3]][length(traj[[3]])]*0.1,
                        label = 'mean\n trajectory')

    # add the single plot to the list
    cluster_plots_list[[shape]] <<- clust_plot
  })
  return(cluster_plots_list)
}



#' Plot join single cluster plots into one grid.
#'
#' This function is designed to take the actual data for every cluster plus
#' the mean shape of the cluster which are already plotted and join these
#' individuals plots into one grid.
#'
#' @param cluster_plots_list List with plots from plot_cluster_data.
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param color_palette Specify the colorbrewer palette used for the cluster.
#'   display in the following style c(number of colors in the palette, palette name).
#' @param ... Any additional argument.
#' @importFrom gridExtra "grid.arrange"
#' @export
# change plot appearance for grid display
plot_cluster_grid <- function(cluster_plots_list,
                              clustering_result,
                              color_palette = c(12, 'Paired'),
                              ...){
  shapes <- clustering_result$cluster_number
  cluster_plots_list <- cluster_plots_list

  # split mean shape tbl in list
  mean_traj_tbl <- clustering_result$mean_trajectories_tbl
  mean_traj_list <-split(mean_traj_tbl, mean_traj_tbl$clusters)

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                                           color_palette[2]))(shapes)

  cluster_plots_list_grid <- lapply(c(1:shapes), function(shape){
    plt <- cluster_plots_list[[shape]]
    plt$layers <- plt$layers[c(1)]
    plt <- plt +  ggplot2::geom_line(data = mean_traj_list[[shape]],
                                     ggplot2::aes(mean_traj_list[[shape]][[2]],
                                                  mean_traj_list[[shape]][[3]]),
                                     colour =  ggplot2::alpha('grey28', alpha = 0.85),
                                     size = 1)
    plot_build <-  ggplot2::ggplot_build(plt)
    plot_build$data[[1]]$colour <-  ggplot2::alpha(color_scale[shape], alpha = 0.3) # add more transparency
    plot_build$plot$theme$axis.title <-  ggplot2::element_text(size = 7) # lower axis title font size
    plot_build$plot$theme$axis.text <-  ggplot2::element_text(size = 7) # lower axis label font size
    plot_build$plot$theme$plot.title <-  ggplot2::element_text(size = 7,  # lower title font size
                                                               hjust = 0, # adjust title to left side
                                                               face = 'bold')
    plot_table <-  ggplot2::ggplot_gtable(plot_build)
    return(plot_table)
  })

  cluster_plots_grid <- do.call('grid.arrange', c(cluster_plots_list_grid,
                                                  ncol = floor(sqrt(length(cluster_plots_list)))))


  return(cluster_plots_grid)
}



#' Display number of data chunks per cluster in an interactive histogram.
#'
#' This functions displays the number of unique IDs per cluster. The IDs represent
#' e.g. weeks, years, core periods or all data of own patient.
#'
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param ... Any additional plotly arguments.
#' @export
create_cluster_hist <- function(clustering_result, ...){
  cluster_numbers <-  clustering_result$cluster_assignement$clusters
  ticks_vals <- sort(c(unique(cluster_numbers)))

  # create yaxis label
  if(is.null(clustering_result$chunk_description)){
    yaxis_label <- 'Number of Data Chunks'
  }else{
    yaxis_label <- clustering_result$chunk_description
  }

  cluster_hist <- plotly::plot_ly(x = cluster_numbers, type= 'histogram') %>%
    plotly::layout(
      title = 'Number of Data Chunks per Cluster',
      xaxis = list(title = 'Cluster',
                   ticks ="outside",
                   tickvals = ticks_vals,
                   ticktext = ticks_vals),
      yaxis = list(title = yaxis_label),
      bargap = 0.05)

}




