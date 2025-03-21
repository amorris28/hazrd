% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_or.R
\name{calc_or}
\alias{calc_or}
\title{Calculates the odds ratio for a dataset}
\usage{
calc_or(df, or_age, lower_interval, upper_interval)
}
\arguments{
\item{df}{a data.frame containing the columns phs, age, and status}

\item{or_age}{an integer specifying the age at which the odds ratio should be calculated}

\item{lower_interval}{a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be \code{-Inf}. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., \code{c(0.30, 0.70)}). The default is \code{0.2}.}

\item{upper_interval}{a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be \code{Inf}. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., \code{c(0.80, 0.98)}). The default is \code{0.80}.}
}
\value{
A numeric odds ratio
}
\description{
Internal function. Not intended for users.
}
