\name{writeTab}
\alias{writeTab}
\alias{writeTab,cellHTS-method}

\title{Write the data from a cellHTS object to a tab-delimited file}

\description{
  Write the data from a \code{\linkS4class{cellHTS}} object to a tab-delimited file.
}


\usage{
\S4method{writeTab}{cellHTS}(x, file=paste(x@name, "txt", sep="."), \dots)
}

\arguments{
  \item{x}{a \code{cellHTS} object.}
  \item{file}{the name of the output file.}
  \item{\dots}{ignored.}
}

\details{
This function is a wrapper for function \code{\link{write.table}} to write the data from a \code{\linkS4class{cellHTS}} object to a tab-delimited file.
}

\value{
 The name of the file that was written.
}

\seealso{\code{\linkS4class{cellHTS}}}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)     
    writeTab(KcViabSmall, file=tempfile())
}
\keyword{manip}
