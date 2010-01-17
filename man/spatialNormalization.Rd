\name{spatialNormalization}
\alias{spatialNormalization}
\concept{Spatial normalization}

\concept{normalization}

\title{Spatial normalization}

\description{
  Adjust spatial plate effects. This function works on the data stored
  in the slot \code{assayData} of a \code{\linkS4class{cellHTS}} object by fitting a
  bivariate function within each plate using local regression
  (\code{\link[locfit:locfit]{robust local fit}}) with second degree
  polynomials. Only wells containing \code{"sample"} are considered
  for the parameter fitting, but adjusted data for all wells are returned.
}

\usage{

spatialNormalization(object, save.model=FALSE, ...)

}
\arguments{
  
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already
    been configured.}
  
  \item{...}{Parameters that get passed on to the
    \code{\link[locfit:lp]{lp}} function of \code{locfit}.
    Most relevant are \code{nn} and \code{h}.}

  \item{save.model}{a logical value specifying whether the values of the
    fitted adjustment functions should be returned in the slot
    \code{rowcol.effects} of the returned object. }
  
}

\details{
This function is typically not called directly, but rather indirectly from
\code{\link[cellHTS2:normalizePlates]{normalizePlates}} function.
The normalization is performed separately for each replicate and
channel.}

\value{
  An object of class \code{\linkS4class{cellHTS}} with normalized data
  stored in slot \code{assayData}.  Furthermore, if
  \code{save.model=TRUE}, it will contain a slot \code{rowcol.effects},
  a 3D array with the same dimension as \code{Data(object)}.

  Moreover, the processing status of the \code{\linkS4class{cellHTS}}
  object is updated in the slot \code{state} to
  \code{object@state[["normalized"]]=TRUE}.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
  \code{\link[stats:medpolish]{medpolish}},
  \code{\link[locfit:locfit]{locfit}},
  \code{\link[cellHTS2:plotSpatialEffects]{plotSpatialEffects}},
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}},
  \code{\link[cellHTS2:plateEffects]{plateEffects}}
}

\examples{
    data(KcViabSmall)
    x <- KcViabSmall
    xs <- spatialNormalization(x, save.model = TRUE, h=3)

    ## Calling spatialNormalization function from "normalizePlates":
    xopt <- normalizePlates(x, varianceAdjust="none", save.model = TRUE)
    all(xs@rowcol.effects == xopt@rowcol.effects, na.rm=TRUE)

}
\keyword{manip}
