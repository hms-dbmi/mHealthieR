################################################################################
#                     Function to create html report                          #
################################################################################

#' Create summary report for given data set.
#'
#' This function is used to generate a summary report based on the given data
#' set and all subsequent analysis. The report format with RMarkdown and
#' rendered to a html page which gets stored in the current working directory
#' and displayed.
#'
#' @param output_path Output path where the html report should be stored.
#' @param show_html If TRUE html report is directly displayed.
#' @param ... Further options for rmarkdown::render.
#' @importFrom utils "browseURL"
#' @export
create_report <- function(output_path = getwd(),
                          show_html = TRUE,
                          ...){
  report_path <- paste0(getwd(), '/R/summary_report.R' )
  rmarkdown::render(report_path, output_dir = output_path, clean = TRUE, ...)
  if(show_html == TRUE){
    browseURL(paste0(output_path,'/summary_report.html'))
  }
}
