\name{writeTab}
\alias{writeTab}
\alias{writeTab,cellHTS-method}

\title{Write the data from a cellHTS object to a tab-delimited file}

\description{
  Write the data from a \code{\linkS4class{cellHTS}} object to a tab-delimited file.
}


\usage{
\S4method{writeTab}{cellHTS}(object, file=paste(name(object), "txt", sep="."))
}

\arguments{
  \item{object}{a \code{cellHTS} object.}
  \item{file}{the name of the output file.}
}

\details{
  
This function is a wrapper for function \code{\link{write.table}} to
write the contents of \code{assayData} slot of a
\code{\linkS4class{cellHTS}} object to a tab-delimited file. If the
object is already annotated, the probe information
(\code{fData(object)@GeneID}) is also added.

}

\value{
 The name of the file that was written.
}

\seealso{\code{\linkS4class{cellHTS}}}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia P. Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)     
    writeTab(KcViabSmall, file=tempfile())
}
\keyword{manip}
