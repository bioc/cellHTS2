\name{spatialNormalization}
\alias{spatialNormalization}
\alias{spatialNormalization,cellHTS-method}
\concept{Spatial normalization}

\concept{normalization}

\title{Spatial normalization}
\description{
  Correction of spatial plate effects of data stored in slot \code{xraw} of a \code{\linkS4class{cellHTS}} object by fitting a polynomial surface within each plate using local regression (\code{\link[stats:loess]{loes}} or \code{\link[locfit:locfit]{robust local fit}}). Uses a second degree polynomial (local quadratic fit). Only wells containing "samples" are considered to fit the models.
}
\usage{
spatialNormalization(object, model="locfit", smoothPar=0.6, save.model=FALSE)
}
\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already been configured.}
  \item{model}{character. Fit the polynomial surface using robust "locfit" or "loess". The default is "locfit".}
  \item{smoothPar}{numeric. The default is \code{smoothPar=0.6}. The parameter which controls the degree of smoothing 
  (corresponds to 'span' argument of \code{\link[stats:loess]{loess}}, 
   or to the parameter 'nn' of \code{\link[locfit:lp]{lp}} of locfit).}
  \item{save.model}{a logical value specifying whether the per-plate spatial effects should be stored in slot 
\code{rowcol.effects}. See details.}
}

\details{
For convenience, this function should be called indireclty from \code{\link[cellHTS2:normalizePlates]{normalizePlates}} function.

The normalization is performed separately for each replicate and channel, in a per-plate fashion using the chosen local regression model. The polynomical surfaces are fitted using the wells containing "sample".

If \code{save.model=TRUE}, the models row and column offsets are stored in the slot 
\code{rowcol.effects} of \code{object}.
}

\value{
An object of class \code{\linkS4class{cellHTS}} with normalized data stored in slot \code{xnorm}.
data. Furthermore, if \code{save.model=TRUE}, the slot \code{rowcol.effects} is also stored in \code{object}.
This slot is a 4D array with the same dimension as \code{object@xraw}.

Moreover, the processing status of the \code{\linkS4class{cellHTS}} object is updated
in the slot \code{state} to \code{object@state["normalized"]=TRUE}.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
  \code{\link[stats:medpolish]{medpolish}},
  \code{\link[stats:loess]{loess}},
  \code{\link[locfit:locfit]{locfit}},
  \code{\link[cellHTS2:plotSpatialEffects]{plotSpatialEffects}},
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}}
}

\references{
}

\examples{
    data(KcViabSmall)
    x <- KcViabSmall
    x <- spatialNormalization(x, model="loess", save.model = TRUE)
    ## Calling spatialNormalization function from "normalizePlates":
    xopt <- normalizePlates(x, method="loess", varianceAdjust="byExperiment", save.model = TRUE)
    all(x@rowcol.effects==xopt@rowcol.effects, na.rm=TRUE)

}
\keyword{manip}
