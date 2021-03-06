% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithms.R
\name{CMIM}
\alias{CMIM}
\title{Discrete version of the Conditional Mutual Information Maximisation selection, using the fast exact implementation by F. Fleuret.}
\usage{
CMIM(X, Y, k)
}
\arguments{
\item{X}{Predictor table, given as a matrix or data frame with only factor or logical columns. \code{NA}s are not allowed.}

\item{Y}{Decision attribute; must be either a factor or a logical vector. \code{NA}s are not allowed.}

\item{k}{Number of attributes to select; first element will be used. Must not exceed \code{ncol(X)}.}
}
\value{
A list with two elements: \code{selection}, a vector of names of the selected features (note that it may be shorter than \code{k}), and \code{scores}, a vector of feature scores.
}
\description{
Discrete version of the Conditional Mutual Information Maximisation selection, using the fast exact implementation by F. Fleuret.
}
\examples{
CMIM(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
}
\references{
"Fast Binary Feature Selection using Conditional Mutual Information Maximisation" F. Fleuret, JMLR (2004)
}
