% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmf_plotting_functions.R
\name{plot_pmf_factor_contributions}
\alias{plot_pmf_factor_contributions}
\title{Function to plot PMF factor contributions using data from 
\code{\link{read_pmf_factor_profiles}}.}
\usage{
plot_pmf_factor_contributions(
  df,
  variable = c("factor", "source"),
  by_model_run = TRUE
)
}
\arguments{
\item{df}{Tibble from \code{\link{read_pmf_factor_profiles}}.}

\item{variable}{Variable to plot, either \code{"factor"} or \code{"source"}.}

\item{by_model_run}{Should the plots be faceted by model runs?}
}
\value{
\strong{ggplot2} with bar geometries.
}
\description{
Function to plot PMF factor contributions using data from 
\code{\link{read_pmf_factor_profiles}}.
}
\author{
Stuart K. Grange
}
