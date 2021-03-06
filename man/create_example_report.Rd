% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_summary_report.R
\name{create_example_report}
\alias{create_example_report}
\title{Create summary report for given data set.}
\usage{
create_example_report(output_path = getwd(), show_html = TRUE, ...)
}
\arguments{
\item{output_path}{Output path where the html report should be stored.}

\item{show_html}{If TRUE html report is directly displayed.}

\item{...}{Further options for rmarkdown::render.}
}
\description{
This function is used to generate a summary report based on the given data
set and all subsequent analysis. The report format with RMarkdown and
rendered to a html page which gets stored in the current working directory
and displayed.
}
