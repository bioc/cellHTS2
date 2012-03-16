\name{getAlphaNumeric}
\alias{getAlphaNumeric}

\title{Convert from plate coordinates to alphanumeric notation.}
\description{
Given an array of (x,y) well coordinates, this function returns the corresponding alphanumeric notation.
}

\usage{
getAlphaNumeric(horizontal, vertical)
}
\arguments{
 \item{horizontal}{Integer coordinate of horizontal well location.}
 \item{vertical}{Integer coordinate of vertical well location.}
}
\value{
 \code{getAlphaNumeric} returns a list containing \code{id}, the full alphanumeric id of the well(s), \code{id.alpha}, the alpha part of the id, and \code{id.num}, the numeric part of the id.
}

\author{Joseph Barry \email{joseph.barry@embl.de}}

\seealso{
 \code{\link{convertWellCoordinates}}
}

\examples{
 ## To obtain the alpha, numeric and alphanumeric information for a single well
 getAlphaNumeric(horizontal=1,vertical=1)

 ## To obtain only the alphanumeric ids of a tetrad in the corner of a 1536 well plate
 getAlphaNumeric(horizontal=c(31,31,32,32),vertical=c(47,48,47,48))$id
}
