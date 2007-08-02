\name{state}
\alias{state}
\alias{state,cellHTS-method}

\title{Retrieve the state of a cellHTS object.}

\description{
  This generic function access the state of an object derived from the
  \code{\linkS4class{cellHTS}} class.
}

\usage{
state(object)
}
\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}}.}
}
\value{
  \code{state} returns a logical vector corresponding to the contents of slot \code{state} of \code{object}.
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{\code{\linkS4class{cellHTS}}
}

\keyword{manip}
