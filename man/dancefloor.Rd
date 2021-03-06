% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dancefloor.R
\name{dancefloor}
\alias{dancefloor}
\title{Define a dancefloor}
\usage{
dancefloor(space = 100, days = 1, admission = 0)
}
\arguments{
\item{space}{Numeric, room size for the dancefloor in square meters.}

\item{days}{Integer number between 1 and 7, number of days the club is opened per week.}

\item{admission}{Numeric,
      highest ticket price for the dancefloor. If prices vary over the week, use the average of highest prices, respectively.}
}
\value{
An object of class \code{\link[GEMATariff]{GEMA.dancefloor-class}}.
}
\description{
Define a dancefloor
}
\examples{
myDancefloor <- dancefloor(space=200, days=3, admission=6)
}

