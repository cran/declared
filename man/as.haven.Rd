% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/haven.R
\name{as.haven}
\alias{as.haven}
\title{Coerce to haven / labelled objects}
\usage{
as.haven(x, ...)
}
\arguments{
\item{x}{A declared labelled vector}

\item{...}{Other arguments used by various methods}
}
\value{
A labelled vector of class "haven_labelled_spss".
}
\description{
Convert declared labelled objects to haven labelled objects
}
\details{
This is a function that reverses the process of \code{as.declared()}, making
a round trip between \code{declared} and \code{haven_labelled_spss} classes.
}
\examples{

x <- declared(
    c(1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = -1
)

x

as.haven(x)
}
