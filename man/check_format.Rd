% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_handling.R
\name{check_format}
\alias{check_format}
\title{Distinguish between wide or long table.}
\usage{
check_format(data_tbl)
}
\arguments{
\item{data_tbl}{Data tibble which should be checked for the right format.}
}
\description{
This function distinguishes, if the input is in wide or long table format.
It also determines, if the first column is the data or key column.
}
