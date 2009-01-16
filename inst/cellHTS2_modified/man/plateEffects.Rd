\name{plateEffects}
\alias{plateEffects}
\alias{plateEffects,cellHTS-method}

\title{Access plate effects stored in a cellHTS object.}
\description{
This generic function accesses plate effects stored in slot \code{rowcol.effects} and \code{overall.effects} 
of a \code{\linkS4class{cellHTS}} instance.
}
\usage{
plateEffects(object)
}
\arguments{
  \item{object}{Object derived from class \code{\linkS4class{cellHTS}} that has been normalized by a spatial normalization method. See details.}
}
\value{
  \code{plateEffects} returns a list with two elements: \code{rowcol} and \code{overall}.
 
\code{plateEffects[["rowcol"]]} corresponds to the contents of slot \code{rowcol.effects} of \code{object}. This is a 3D array with the same dimensions as \code{Data(object)}: \code{nr of Features x nr of Samples x nr of Channels} of the current \code{\linkS4class{cellHTS}} object.
  
\code{plateEffects[["overall"]]} contains the data stored in slot \code{overall.effects} of \code{object}. This is a 3D array with dimensions \code{nr Plates x nr Samples x nr Channels} of \code{object}. Slot \code{overall.effects} is only estimated when B score method is used to normalize the data. 

Note that these 2 slots accessed by this function were stored after preprocessing the data either using Bscore method or local regression fit (see \code{\link{normalizePlates}} for details), by setting the option \code{save.model=TRUE}.
}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
\code{\linkS4class{cellHTS}}
\code{\link[cellHTS2:normalizePlates]{normalizePlates}}
\code{\link[cellHTS2:Bscore]{Bscore}}
\code{\link[cellHTS2:spatialNormalization]{spatialNormalization}}
}

\keyword{manip}
