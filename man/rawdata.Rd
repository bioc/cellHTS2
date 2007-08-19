\name{rawdata}
\alias{rawdata}
\alias{rawdata<-}
\alias{rawdata,cellHTS-method}
\alias{rawdata<-,cellHTS,array-method}
\alias{normdata}
\alias{normdata<-}
\alias{normdata,cellHTS-method}
\alias{normdata<-,cellHTS,array-method}

\title{Access and replace the raw or normalized data from a cellHTS object.}
\description{
These generic functions access and replace raw or normalized data stored in slot \code{xraw} or \code{xnorm} 
of an object of \code{\linkS4class{cellHTS}} class.
}
\usage{
rawdata(object)
rawdata(object) <- value

normdata(object)
normdata(object) <- value

}
\arguments{
  \item{object}{Object derived from class \code{\linkS4class{cellHTS}}.}
  \item{value}{4D array of dimensions number of wells x number of plates x number or replicates x number of channels.}
}
\value{
  \code{rawdata} returns a 4D array with dimensions number of wells x number of plates x number of replicates x number of channels. 

  \code{normdata} returns a 4D array with dimensions number of wells x number of plates x number of replicates x number of channels.

See class \code{\linkS4class{cellHTS}} for details.
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
\code{\linkS4class{cellHTS}}
}

\keyword{manip}
