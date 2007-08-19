\name{description}
\alias{description}
\alias{description,cellHTS-method}

\title{Retrieve description information from a cellHTS object.}

\description{
  This generic function access the screen description information that is stored 
  in the \code{screenDesc} slot of an object derived from the
  \code{\linkS4class{cellHTS}} class.
}

\usage{
description(object)
}
\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}}.}
}
\value{
  \code{description} returns a character vector containing the description for the screen 
  associated with \code{object}.
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{\code{\linkS4class{cellHTS}}, \code{\link[cellHTS2:configure]{configure}}
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\keyword{manip}
