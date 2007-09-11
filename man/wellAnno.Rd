\name{wellAnno}
\alias{wellAnno}
\alias{wellAnno,cellHTS-method}
\alias{geneAnno}
\alias{geneAnno,cellHTS-method}

\title{Access the annotation from a cellHTS object.}
\description{
These generic functions access the annotation data stored in the \code{featureData} slot of an object of 
class \code{\linkS4class{cellHTS}}.
}
\usage{
wellAnno(object)
geneAnno(object)
}
\arguments{
  \item{object}{Object derived from class \code{\linkS4class{cellHTS}}.}
}
\value{
  \code{wellAnno} returns a \code{\link{factor}} of length equal to the total number of features (number of plates x number of wells per plate) indicating the contents of the wells. Corresponds to \code{fData(object)[,"controlStatus"]}. 

  \code{geneAnno} returns a vector of the same length as the number of features in \code{object} (number of plates x number of wells per plate) containing the gene IDs used in the screen. This corresponds to the contents of 
\code{fData(object)[,"GeneID"]}. 

See \code{\linkS4class{cellHTS}} class for details.
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\seealso{
\code{\linkS4class{cellHTS}}
}

\keyword{manip}
