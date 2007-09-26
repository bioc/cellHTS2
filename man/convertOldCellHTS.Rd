\name{convertOldCellHTS}
\alias{convertOldCellHTS}
\title{Convert an old S3 class cellHTS object to the new S4 class cellHTS object}
\description{
  Convert an old S3 class \code{cellHTS} object obtained using \code{cellHTS} package 
to new S4 class \code{\linkS4class{cellHTS}} object(s).}
\usage{
convertOldCellHTS(oldObject)
}

\arguments{
  \item{oldObject}{a S3 class \code{cellHTS} object obtained using the package \code{cellHTS}.}
}

\details{
This function converts an old S3 \code{cellHTS} class object called \code{oldObjt} into S4 class \code{\linkS4class{cellHTS}} object(s). 
}

\seealso{

}

\value{
 The function returns a list containing at least one \code{cellHTS} objects named "raw" which corresponds 
to a unnormalized \code{\linkS4class{cellHTS}} object (i.e. comprising the raw data stored in \code{oldObject}). 
Depending on the state of \code{oldObject} (normalized and scored), the other components of this list can be:
\code{"normalized"}: a S4 class \code{cellHTS} object containing normalized data, and 
\code{"scored"}: a S4 class \code{cellHTS} object containing scored data. 
}

\author{Ligia P. Bras \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\examples{
    data(oldKcViabSmall)
    out <-  convertOldCellHTS(oldKcViabSmall)
    x <- out[["raw"]]
    x
}

\keyword{manip}
