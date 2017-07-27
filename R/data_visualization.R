################################################################################
#                        Functions to visualize data                           #
################################################################################

# The following function solves the '.' is not a global variable when
# using magrittr (https://github.com/tidyverse/magrittr/issues/29)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# This function suppressses notes arising from column names used
# with magrittr pipes and dplyr functions.
globalVariables(c('cluster', 'freq', 'row_number', 'key', 'value'))

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
                                               clustering_result$time_factor,
                                               ']')),
                   yaxis = list(title = yaxis_label))


  return(cluster_means_plot)
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
      ggplot2::xlab(paste0('Time Points [', clustering_result$time_factor, ']')) +
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
    plot_build$data[[1]]$colour <-  ggplot2::alpha(color_scale[shape], alpha = 0.3)
    # add more transparency
    plot_build$plot$theme$axis.title <-  ggplot2::element_text(size = 7)
    # lower axis title font size
    plot_build$plot$theme$axis.text <-  ggplot2::element_text(size = 7)
    # lower axis label font size
    plot_build$plot$theme$plot.title <-  ggplot2::element_text(size = 7,
                                                               # lower title font size
                                                               hjust = 0,
                                                               # adjust title to left side
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

  return(cluster_hist)
}


#' Create violin plot per cluster per day.
#'
#' This function creates violin plots which show the distribution of values
#' either from the data where the clustering was performed on or some additional
#' data with the same identifiers. One violin plot gets created for every time
#' point summarized in one figure per cluster.
#'
#' @param data_tbl Data tibble where the clustering was performed on.
#' @param additional_data_tbl Data tibble where the clustering was performed on
#'   or any other data tibble with additional values but the same IDs and similar
#'   time points.
#' @param color_palette Customizable color palette vector with ColorBrewer parameters.
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param ... Any additional plotly arguments for ggplot2.
#' @export
create_violin_plots <- function(data_tbl, additional_data_tbl,
                                clustering_result = NULL,
                                color_palette = c(12, 'Paired'),
                                ...){
  # Retrieve the shapes the clustering was calculated on
  shapes <- clustering_result$cluster_number
  index_vec <- c(1:shapes)

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                                           color_palette[2]))(shapes)
  # get the additional data corresponding to the clusters calculated on the data_tbl
  cluster_rel <- clustering_result$cluster_assignement
  cluster_rel_list <- split(cluster_rel, cluster_rel$clusters)
  additional_real_data <- lapply(cluster_rel_list, function(ls){
    real_data <- additional_data_tbl[which(additional_data_tbl[[1]] %in% ls[[2]]),]
  })

  detail_violin_plots <- lapply(index_vec, function(index){
    real_df <- additional_real_data[[index]]
    real_df[[2]] <- as.factor(real_df[[2]])

    if(length(unique(real_df[[2]])) < 21){
      violin  <- ggplot2::ggplot(real_df, ggplot2::aes(x = real_df[[2]],
                                                       y = real_df[[3]])) +
        ggplot2:: geom_violin(trim = FALSE) +
        ggplot2::stat_summary(fun.data = "mean_sdl", geom = "crossbar",
                              width=0.06 ) +
        ggplot2::geom_hline(colour = 'black', yintercept = mean(real_df[[3]])) +
        ggplot2::labs(title = paste0('Value distribution over time for cluster ',
                                     index),
                      x = attributes(additional_data_tbl)$time_factor,
                      y = "values")

      violin <- violin +
        ggplot2::annotate('text',
                          x = min(as.numeric(levels(real_df[[2]]))) - min(
                            as.numeric(levels(real_df[[2]])))* 0.25 ,
                          y = mean(real_df[[3]] + real_df[[3]]*0.075),
                          label = 'average')

      return(violin)
    }
  })
  return(detail_violin_plots)
}


#' Create violin plot per cluster.
#'
#' This function creates violin plots which show the distribution of values
#' either from the data where the clustering was performed on or some additional
#' data with the same identifiers. One violin plot gets created for every cluster
#' summarized in one figure.
#'
#' @param data_tbl Data tibble where the clustering was performed on.
#' @param additional_data_tbl Data tibble where the clustering was performed on
#'   or any other data tibble with additional values but the same IDs and similar
#'   time points.
#' @param color_palette Customizable color palette vector with ColorBrewer parameters.
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param ... Any additional plotly arguments for ggplot2.
#' @export
create_cluster_violin_plot <- function(data_tbl, additional_data_tbl,
                                       clustering_result = NULL,
                                       color_palette = c(12, 'Paired'),
                                       ...){
  # Retrieve the shapes the clustering was calculated on
  shapes <- clustering_result$cluster_number
  index_vec <- c(1:shapes)

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                                           color_palette[2]))(shapes)
  # get the additional data corresponding to the clusters calculated on the data_tbl
  cluster_rel <- clustering_result$cluster_assignement
  cluster_rel_list <- split(cluster_rel, cluster_rel$clusters)
  additional_real_data <- lapply(cluster_rel_list, function(ls){
    real_data <- additional_data_tbl[which(additional_data_tbl[[1]] %in% ls[[2]]),]
  })

  # calculate value distribution per cluster
  clust_values_list <- lapply(index_vec, function(index){
    cluster_values <- additional_real_data[[index]]
    cluster_col <- length(cluster_values) + 1 # determine cluster col index
    cluster_values$cluster <- rep(index, nrow(cluster_values))
    cluster_values <- cluster_values[, c(1, cluster_col, 3)]
    return(cluster_values)
  })

  # concatinate cluster value dfs to one df
  clust_values <- plyr::ldply(clust_values_list, data.frame)


  clust_values[[2]] <- as.factor(clust_values[[2]]) # for plotting function
  violin  <- ggplot2::ggplot(clust_values, ggplot2::aes(x = clust_values[[2]],
                                                        y = clust_values[[3]])) +
    ggplot2::geom_violin(trim = FALSE,
                         ggplot2::aes(fill = factor(clust_values[[2]]))) +
    ggplot2::scale_fill_manual(breaks = factor(clust_values[[2]]),
                               values = color_scale,
                               name = 'cluster')+
    ggplot2::stat_summary(fun.data = "mean_sdl", geom = "crossbar", width=0.06 )+
    ggplot2::geom_hline(colour = 'black', yintercept = mean(clust_values[[3]])) +
    ggplot2::labs(title = 'Value distribution per cluster',
                  x = 'clusters' , y = "values")

  violin <- violin +
    ggplot2::annotate('text',
                      x = 0.5,
                      y = mean(clust_values[[3]] + clust_values[[3]]*0.075),
                      label = 'avg')

  return(violin)
}

#' Create Cluster Frequency per Individual Heatmap.
#'
#' This function calculates the cluster frequency per individual and displays
#' the percentages in a heatmap.
#'
#' @param data_tbl Data tibble where the clustering was performed on.
#' @param color_palette Customizable color palette vector with ColorBrewer parameters.
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param ... Any additional plotly arguments for ggplot2.
#' @export
create_cluster_freq_heatmap <- function(data_tbl,
                                        clustering_result = NULL,
                                        color_palette = c(12, 'Paired'),
                                        ...){
  # Retrieve the shapes the clustering was calculated on
  shapes <- clustering_result$cluster_number
  index_vec <- c(1:shapes)

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                                           color_palette[2]))(shapes)

  grey_color_scale <- colorRampPalette(c('#f0f0f0','#d9d9d9','#bdbdbd','#969696',
                                         '#737373','#525252','#252525',
                                         '#000000'))(100)

  # get the additional data corresponding to the clusters calculated on the data_tbl
  cluster_rel <- clustering_result$cluster_assignement
  cluster_rel_list <- split(cluster_rel, cluster_rel$clusters)
  clust_per_ind_list <- lapply(index_vec, function(index){
    # cluster number per individual
    ls <- cluster_rel_list[[index]]
    real_data <- data_tbl[which(data_tbl[[1]] %in% ls[[2]]),]
    key_chunk_rel <- real_data[,c(1,5)] %>%
      dplyr::group_by(.[[1]], .[[2]]) %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::ungroup() %>%
      .[,c(1,2)]
    key_clust_rel <- tibble::as.tibble(table(key_chunk_rel))
    key_clust_rel$cluster <- rep(index, nrow(key_clust_rel))
    colnames(key_clust_rel) <- c('chunkKey', 'key', 'freq', 'cluster')
    key_clust_rel <- key_clust_rel[,c('cluster','key', 'freq')]
    return(key_clust_rel)
  })

  clust_per_ind <- plyr::ldply(clust_per_ind_list, data.frame) %>%
    dplyr::group_by(.[[1]], .[[2]]) %>%
    dplyr::summarize(sum(freq, na.rm = TRUE)) %>%
    dplyr::ungroup()

  colnames(clust_per_ind) <- c('cluster','key', 'freq')
  clust_per_ind_df <- tidyr::spread(clust_per_ind, cluster, freq, fill = NA)
  clust_per_ind_df[is.na(clust_per_ind_df)] <- 0
  clust_per_ind_df <- as.data.frame(clust_per_ind_df)
  rownames(clust_per_ind_df) <- clust_per_ind_df[[1]]
  clust_per_ind_df <- clust_per_ind_df[,-1]


  # heatmap with cluster freq per individual
  clust_summary_scaled <- prop.table(as.matrix(clust_per_ind_df), margin = 1)
  clust_sum_heatmap <- iheatmapr::main_heatmap(clust_summary_scaled,
                                               name ='percentage',
                                               colors = grey_color_scale) %>%
    iheatmapr::add_col_title('Relativ Cluster Frequency<br>per Individual',
                             side = "top", font = list(size = 16)) %>%
    iheatmapr::add_col_title('Clusters',
                             side = "bottom", font = list(size = 14)) %>%
    iheatmapr::add_col_labels(side = "bottom",
                              textangle = 0) %>%
    iheatmapr::add_row_labels(size = 0.3,font = list(size = 12),
                              side = 'left') %>%
    iheatmapr::add_row_title('Individuals', side = 'left') %>%
    iheatmapr::add_row_clustering( side = 'right') %>%
    iheatmapr::add_col_annotation(data.frame('cluster' = colnames(clust_summary_scaled)),
                                  side = 'bottom',
                                  colors = list('cluster' = color_scale))
  return(clust_sum_heatmap)
}

#' Create Heatmap which displays values over time per data chunk and cluster.
#'
#' This function creates one heatmap per cluster which shows the values per
#' cluster for each individual data chunk over time.
#'
#' @param data_tbl Data tibble where the clustering was performed on.
#' @param color_palette Customizable color palette vector with ColorBrewer parameters.
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param ... Any additional plotly arguments for ggplot2.
#' @export
create_cluster_values_heatmap <- function(data_tbl,
                                          clustering_result = NULL,
                                          color_palette = c(12, 'Paired'),
                                          ...){
  # Retrieve the shapes the clustering was calculated on
  shapes <- clustering_result$cluster_number
  index_vec <- c(1:shapes)

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                                           color_palette[2]))(shapes)

  # get the additional data corresponding to the clusters calculated on the data_tbl
  cluster_rel <- clustering_result$cluster_assignement
  cluster_rel_list <- split(cluster_rel, cluster_rel$clusters)
  additional_real_data <- lapply(cluster_rel_list, function(ls){
    real_data <- data_tbl[which(data_tbl[[1]] %in% ls[[2]]),]
  })

  interactiv_additional_data <- lapply(additional_real_data, function(ls){
    ls <- ls[,c(1,2,3)]
    colnames(ls) <- c('key', 'timefactor', 'value' )
    ls_wide <- tidyr::spread(ls, key, value, fill = NA)
    rownames(ls_wide) <- ls_wide$timefactor
    ls_wide <- ls_wide[,-1]
    ls_wide <- t(ls_wide)
    return(ls_wide)
  })

  cluster_heatmaps <- lapply(index_vec, function(shape){
    interactiv_data <-(as.matrix(interactiv_additional_data[[shape]]))
    heatmap_color_scale <- colorRampPalette(c('#ffffff',
                                              color_scale[shape]))(max(interactiv_data))
    interactive_plot <- iheatmapr::main_heatmap(interactiv_data,
                                                name ='values',
                                                colors = heatmap_color_scale) %>%
      iheatmapr::add_col_summary(layout = list(title = paste0('avg per<br>',
                                                              attributes(data_tbl)$time_factor),
                                               font = list(size = 8))) %>%
      iheatmapr::add_col_title(paste("Patterns for cluster", shape),
                               side = "top", font = list(size = 16)) %>%
      iheatmapr::add_col_title(attributes(data_tbl)$time_factor,
                               side = "bottom", font = list(size = 14)) %>%
      iheatmapr::add_col_labels(side = "bottom",
                                textangle = 0) %>%
      iheatmapr::add_row_labels(size = 0.3,font = list(size = 12),
                                side = 'left') %>%
      iheatmapr::add_row_summary(groups = TRUE, type = "bar",
                                 layout = list(title = "avg<br>time chunk",
                                               font = list(size = 3)))
    return(interactive_plot)
  })
  return(cluster_heatmaps)
}


