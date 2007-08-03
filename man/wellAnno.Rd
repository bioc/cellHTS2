\name{wellAnno}
\alias{wellAnno}
\alias{wellAnno,cellHTS-method}
\alias{geneAnno}
\alias{geneAnno,cellHTS-method}

\title{Access the annotation from a cellHTS object.}
\description{
These generic functions access the annotation data stored in slot \code{wellAnno} or slot \code{geneAnno} of an object of 
clas \code{\linkS4class{cellHTS}}.
}
\usage{
wellAnno(object)
geneAnno(object)
}
\arguments{
  \item{object}{Object derived from class \code{\linkS4class{cellHTS}}.}
}
\value{
  \code{wellAnno} returns a \code{\link{factor}} of length equal to number of plates x number of wells per plate indicating the contents of the wells. Corresponds to \code{object@wellAnno}. 

  \code{geneAnno} returns a \code{\link{data.frame}} containing gene annotation for the screen and 
corresponds to the contents of \code{object@geneAnno}. It contains at least three columns, and column names \code{Plate}, \code{Well} and \code{GeneID}, where the contents of \code{Plate} are integer. The number of rows is equal to the product between the number of wells in each plate and the number of plates.

See \code{\linkS4class{cellHTS}} class for details.
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
\code{\linkS4class{cellHTS}}
}

\keyword{manip}
