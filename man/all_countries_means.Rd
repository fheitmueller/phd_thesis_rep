% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weights_and_indicators.R
\name{all_countries_means}
\alias{all_countries_means}
\title{calculate means}
\usage{
all_countries_means(data, weight = "weight")
}
\arguments{
\item{data}{is the standard dataframe}

\item{weight}{indicates whether time-constant or time-dynamic weights should be used. Depends on what kind of analysis is carried out. Defaults to the time dynamic weight.}
}
\value{
the dataframe containing the different weights
}
\description{
calculate means
}
