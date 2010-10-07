\name{Data}
\alias{Data}
\alias{Data<-}
\alias{Data,cellHTS-method}
\alias{Data<-,cellHTS,array-method}

\title{Access and replace the assayData slot of a cellHTS object}
\description{
This generic function accesses and replaces the data stored in slot \code{assayData}
of an object of \code{\linkS4class{cellHTS}} class.
}
\usage{
Data(object)
Data(object) <- value
}
\arguments{
  \item{object}{Object derived from class \code{\linkS4class{cellHTS}}.}
  \item{value}{a 3D array of dimensions dim(object)[1] (this corresponds to the total number of features: number of wells per plate x number of plates) x dim(object)[2] (this corresponds to the number of samples or replicates) x number of channels.}
}
\value{
  \code{Data} returns a 3D array containing the contents of slot \code{assayData}. This array has dimensions number of features (product between the number of wells per plates and the number of plates) x number of samples (or replicates) x number of channels. 
Depending on the preprocessing status of the \code{cellHTS} object, this array corresponds to the raw data, or to normalized data or to scored data.

See class \code{\linkS4class{cellHTS}} for details.
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\author{Ligia Bras}

\seealso{
\code{\linkS4class{cellHTS}}
}

\keyword{manip}
