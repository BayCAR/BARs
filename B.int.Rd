% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/B.int.R
\name{B.int}
\alias{B.int}
\title{The B.int function}
\usage{
B.int(x, ddata, cdata, constant)
}
\arguments{
\item{x}{A variable for integraton.}

\item{ddata}{A data frame contains the posterior means and SDs for all the groups.}

\item{cdata}{A data frame contains the posterior means and SDs for all the groups.}

\item{constant}{A constant to be added to the Beta distribution to reflect the appropriate prior. For example, constant=1 reflects uniform (0,1), and constant=0.5 reflects Jeffery's prior.}
}
\description{
The B.int function
}
\examples{

# This is the numerical integration function. Do not need to be run independently.

}
