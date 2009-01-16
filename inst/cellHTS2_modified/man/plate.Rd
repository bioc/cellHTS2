\name{plate}
\alias{plate}
\alias{plate,cellHTS-method}
\alias{pdim}
\alias{pdim,cellHTS-method}
\alias{position}
\alias{position,cellHTS-method}
\alias{well}
\alias{well,cellHTS-method}

\title{Retrieve information related with the format of a RNAi experiment}

\description{
  These generic functions retrieves information related with the format of RNAi experiment conducted in multi-plate format and stored in an object of \code{\linkS4class{cellHTS}} class.
}

\usage{
pdim(object)
plate(object)
well(object)
position(object)
}

\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}}.}
}
\value{
  \code{pdim} returns a vector of length 2 containing integer values that correspond to the number of rows
    and columns in a plate. This corresponds to the plate    	
format used in the screen (for example, 96-well  or 384-well plates).

\code{plate} returns a vector of integers with the same length as the product between the number of wells per plate and the number of plates. This gives the plate number for each well in the assay. Corresponds to \code{fData(object)[,"plate"]}.

\code{well} returns a character vector containing the wells identifiers (for example "A01", "H06"). Its length equals the product between the number of wells per plate and the number of plates. This vector corresponds to \code{fData(object)[,"well"]}.

\code{position} gives a numeric vector containing every well positions (1, 2, 3, ...). Its length is equal to the product between the number of wells per plate and the number of plates.
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{\code{\linkS4class{cellHTS}}
}

\keyword{manip}
