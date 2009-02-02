\name{convertOldCellHTS}
\alias{convertOldCellHTS}
\title{Convert an old S3 class cellHTS object to the new S4 class cellHTS object}
\description{
Convert an old S3  \code{cellHTS} object (from the \code{cellHTS} package) 
into one or several S4 \code{\linkS4class{cellHTS}} objects (from the
\code{cellHTS2} package).}

\usage{convertOldCellHTS(oldObject)}

\arguments{
\item{oldObject}{an S3 class \code{cellHTS} object obtained using the package \code{cellHTS}.}
}

\value{
  The function returns a list containing one or more \code{cellHTS}
  objects. The element \code{raw} contains the
  unnormalized data from \code{oldObject}.
  Depending on the state of \code{oldObject}
  (on whether it is normalized and scored), the other components of this list can be:
  \code{normalized}, an S4 \code{cellHTS} object containing the normalized data, and 
  \code{scored}, an S4 class \code{cellHTS} object containing the scored data. 
}

\author{Ligia P. Bras \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\seealso{

\code{\link{updateCellHTS}}
  
}


\examples{
    data("oldKcViabSmall")
    out <-  convertOldCellHTS(oldKcViabSmall)
    names(out)
    out[["raw"]]
}
