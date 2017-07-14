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
  # reformat the core_tbl binary format
  bin_tb <- core_tbl %>%
    dplyr::mutate(., values = ifelse(.[[3]] >= 1, 1, 0)) %>%
    tidyr::spread(., times, values, fill = NA) %>%
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

#' Display mean shape summary.
#'
#' This function is designed to display all mean shapes (every cluster) in a
#' summary plot over time.
#'
#' @param clustering_result Clustering result list from cluster_shapes function.
#' @param color_palette Specify the colorbrewer palette used for the cluster.
#'   display in the following style c(number of colors in the palette, palette name).
#' @param ... Any additional argument.
#' @importFrom grDevices "colorRampPalette"
#' @export
plot_mean_shapes <- function(clustering_result = NULL,
                          color_palette = c(12, 'Paired'),
                          ...){

  # Retrieve the shapes the clustering was calculated on
  shapes <- clustering_result$cluster_number

  # create colors based on numbers of clusters
  color_scale <- colorRampPalette(RColorBrewer::brewer.pal(as.numeric(color_palette[1]),
                                        color_palette[2]))(shapes)


  # split mean shape tbl in list
  mean_traj_tbl <- clustering_result$mean_trajectories_tbl
  mean_traj_list <-split(mean_traj_tbl, mean_traj_tbl$clusters)

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

  cluster_means_plot <- cluster_means_plot %>% plotly::layout(title =
                                                                'Mean Shapes')

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
    clust_plot <- ggplot(df, aes(df[[2]], df[[3]])) +
      geom_line(aes( group = df[[1]]), colour = alpha(color, alpha = 0.7)) +
      geom_hline(colour = 'black', yintercept = mean(df[[3]])) +
      # add line at mean value
      xlab('times') + ylab('value') +
      ggtitle(paste('cluster ', shape )) +
      theme(legend.position="none",
            panel.background = element_blank(),
            panel.grid.major = element_line(colour = 'grey87'),
            panel.grid.minor = element_line(colour = 'grey87')) +
      theme_bw()+
      geom_line(data = traj,
                aes(traj[[2]],traj[[3]]),
                colour = alpha('grey28', alpha = 0.85),
                size = 2.5)

    clust_plot <- clust_plot + annotate('text',
                                        x = min(df[[2]]),
                                        y = mean(df[[3]] + df[[3]]*0.075),
                                        label = 'average') +
      annotate('text',
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
    plt <- plt + geom_line(data = mean_traj_list[[shape]],
                           aes(mean_traj_list[[shape]][[2]],
                               mean_traj_list[[shape]][[3]]),
                           colour = alpha('grey28', alpha = 0.85),
                           size = 1) +
                theme(panel.border = element_blank())
    plot_build <- ggplot_build(plt)
    plot_build$data[[1]]$colour <- alpha(color_scale[shape], alpha = 0.3) # add more transparency
    plot_build$plot$theme$axis.title <- element_text(size = 7) # lower axis title font size
    plot_build$plot$theme$axis.text <- element_text(size = 7) # lower axis label font size
    plot_build$plot$theme$plot.title <- element_text(size = 7,  # lower title font size
                                                     hjust = 0, # adjust title to left side
                                                     face = 'bold')
    plot_table <- ggplot_gtable(plot_build)
    return(plot_table)
  })

  cluster_plots_grid <- do.call('grid.arrange', c(cluster_plots_list_grid,
                                                 ncol = floor(sqrt(length(cluster_plots_list)))))


  return(cluster_plots_grid)
}





