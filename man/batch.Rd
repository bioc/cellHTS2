\name{batch}
\alias{batch}
\alias{batch<-}
\alias{batch,cellHTS-method}
\alias{batch<-,cellHTS,array-method}


\title{Access and replace the batch slot of a cellHTS object}
\description{
This generic function accesses and replaces the data stored in slot \code{batch}
of an object of \code{\linkS4class{cellHTS}} class.
}
\usage{
batch(object)
batch(object) <- value
}
\arguments{
  \item{object}{Object derived from class \code{\linkS4class{cellHTS}}.}
  \item{value}{a 3D array of integer values giving the batch number for each plate, sample and channel. Its dimensions should be equal to dim(object)[1] (this corresponds to the total number of features: number of wells per plate x number of plates) x dim(object)[2] (this corresponds to the number of samples or replicates) x number of channels.}
}

\value{
  \code{Data} returns a 3D array containing the contents of slot \code{batch}. This array has dimensions number of features (product between the number of wells per plates and the number of plates) x number of samples (or replicates) x number of channels of the current \code{\linkS4class{cellHTS}} object.

See class \code{\linkS4class{cellHTS}} for details.
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
\code{\linkS4class{cellHTS}}, \code{\link{nbatch}}
}

\keyword{manip}
