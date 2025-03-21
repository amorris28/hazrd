% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hr.R
\name{get_hr}
\alias{get_hr}
\title{Returns a hazard ratio and 95\% CI for Polygenic Hazard Scores}
\usage{
get_hr(
  data = NULL,
  phs = "phs",
  age = "age",
  status = "status",
  lower_interval = 0.2,
  upper_interval = 0.8,
  CI = FALSE,
  bootstrap_iterations = 1000,
  swc = FALSE,
  swc_popnumcases = NULL,
  swc_popnumcontrols = NULL
)
}
\arguments{
\item{data}{an optional data.frame containing the variables for phs, age, and status}

\item{phs}{an optional string specifying the column name in \code{data} containing the polygenic hazard score for each subject or the unquoted name of a vector containing these values. The default is "phs"}

\item{age}{an optional string specifying the column name in \code{data} containing the age of each subject or the unquoted name of a vector containing these values. For cases, this should be the age at event (e.g., diagnosis) and for controls this should be age of censoring (e.g., last observation). The default is "age"}

\item{status}{an optional string specifying the column name in \code{data} containing case-control status (0 = censored, 1 = event) or the unquoted name of a vector containing these values. The default is "status"}

\item{lower_interval}{a vector specifying the quantiles of the lower interval. If a single value is given, that will be used as the upper quantile and the lower quantile will be \code{-Inf}. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., \code{c(0.30, 0.70)}). The default is \code{0.2}.}

\item{upper_interval}{a vector specifying the quantiles of the upper interval. If a single value is given, that will be used as the lower quantile and the upper quantile will be \code{Inf}. If a vector of length 2 is provided then these will be used as the lower and upper quantiles of the interval (e.g., \code{c(0.80, 0.98)}). The default is \code{0.80}.}

\item{CI}{logical. if \code{TRUE} performs bootstrap and returns 95\% confidence intervals. Default = \code{FALSE}.}

\item{bootstrap_iterations}{Number of bootstrap iterations to run. Required if boot = \code{TRUE}. Default = 1000.}

\item{swc}{logical. if \code{TRUE} performs sample weight correction}

\item{swc_popnumcases}{an optional integer specifying the number of cases in a reference population for sample weight correction. Required if swc = \code{TRUE}.}

\item{swc_popnumcontrols}{an optional integer specifying the number of controls in a reference population for sample weight correction. Required if swc = \code{TRUE}.}
}
\value{
A numeric hazard ratio or a list containing HR and the 95\% confidence intervals from bootstrap
}
\description{
This function calculates the hazard ratio and optionally performs
bootstrap resampling to return 95\% confidence intervals.
The data can either be provided as a data.frame with columns containing
the phs, age, and status of each individual or separate vectors containing
each of these values. The columns in \code{data} default to 'phs', 'age', and
'status', but any arbitrary column names can be used if named explicitly.
}
\examples{

HR80_20 <- get_hr(test_data, CI = TRUE, bootstrap_iterations = 300)

}
