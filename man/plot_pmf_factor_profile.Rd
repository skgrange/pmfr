% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmf_plotting_functions.R
\name{plot_pmf_factor_profile}
\alias{plot_pmf_factor_profile}
\title{Function to plot PMF factor profiles using data from 
\code{\link{read_pmf_factor_profiles}} and \code{\link{tidy_pmf_profiles}}.}
\usage{
plot_pmf_factor_profile(df, by_model_run = TRUE, x_label_angle = 0)
}
\arguments{
\item{df}{Tibble from \code{\link{tidy_pmf_profiles}}.}

\item{by_model_run}{Should the plots be faceted by model runs?}

\item{x_label_angle}{What angle should the x-axis labels be presented in? If
your labels are long, \code{45} can be useful.}
}
\value{
ggplot2 with bar geometries.
}
\description{
Function to plot PMF factor profiles using data from 
\code{\link{read_pmf_factor_profiles}} and \code{\link{tidy_pmf_profiles}}.
}
\author{
Stuart K. Grange
}
