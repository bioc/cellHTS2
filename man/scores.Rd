\name{scores}
\alias{scores}
\alias{scores<-}
\alias{scores,cellHTS-method}
\alias{scores<-,cellHTS,numeric-method}

\title{Access and replace scored data from a cellHTS object.}
\description{
This generic function accesses and replaces scored data stored in slot \code{score} of an object of 
\code{\linkS4class{cellHTS}} class.
}
\usage{
scores(object)
scores(object) <- value
}
\arguments{
  \item{object}{Object derived from class \code{\linkS4class{cellHTS}}.}
  \item{value}{Numeric vector of length number of wells x number of plates.}
}
\value{
  \code{scores} returns a numeric vector of length number of wells x number of plates. See classes for details
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
\code{\linkS4class{cellHTS}}
}

\keyword{manip}
