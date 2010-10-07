\name{batch}
\alias{batch}
\alias{batch<-}
\alias{batch,cellHTS-method}
\alias{batch<-,cellHTS,data.frame-method}


\title{

  Access and replace the batch information of a cellHTS object
  
}


\description{
  
This method accesses and replaces the batch data stored in the slot
\code{plateData} of a \code{\linkS4class{cellHTS}} object.

}

\usage{
batch(object)
batch(object) <- value
}
\arguments{
  
  \item{object}{object of class \code{\linkS4class{cellHTS}}.}
  
  \item{value}{a dataframe of integer values giving the batch number for
    each plate, and sample.}
}

\value{
  
  \code{batch} returns a dataframe containing the contents of slot
  \code{plateData$Batch}. This dataframe has dimensions number of plates
  x number of samples of the \code{\linkS4class{cellHTS}} object.

  See class \code{\linkS4class{cellHTS}} for details.
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\author{Ligia Bras}

\seealso{
\code{\linkS4class{cellHTS}}, \code{\link{nbatch}}
}

\keyword{manip}
