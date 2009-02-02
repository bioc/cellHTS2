\name{updateCellHTS}

\alias{updateCellHTS}


\title{ Update old serialized cellHTS objects. }

\description{
  During the development of the cellHTS2 package, the definition of the
  \code{cellHTS} object hasd changes. This function can be used to
  update old serialized \code{cellHTS} objects.
}

\usage{

updateCellHTS(object)
}


\arguments{
  \item{object}{ The \code{cellHTS} object to update. }
}


\value{
 An updated \code{cellHTS} object. 
}

\seealso{

\code{\link{convertOldCellHTS}}
  
}


\author{ Florian Hahne }

\examples{

data(KcViabSmall)
updateCellHTS(KcViabSmall)

}
