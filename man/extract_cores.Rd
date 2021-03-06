% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_chunk_extraction.R
\name{extract_cores}
\alias{extract_cores}
\title{Extract core periods with a certain allowed percentage of NAs.}
\usage{
extract_cores(data_tbl, impute_NA = FALSE, percentage_NA = 0.1,
  window_size = 3, identifier_column_index = 1)
}
\arguments{
\item{data_tbl}{input data tibble.}

\item{impute_NA}{If TRUE NAs will be imputed based on weighted mean in a
certain window.}

\item{percentage_NA}{percentage of allowed missing values a core period can have.}

\item{window_size}{values on both sides of the value which should be imputed
(e.g. window_size =3 gives a 7 days window).}

\item{identifier_column_index}{insert index of colum which contains IDs/keys.}
}
\description{
This function extract periods starting with the first value per individual
and ending after a certain percentage of missing values (NAs) is reached.
}
