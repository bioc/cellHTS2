\name{plotSpatialEffects}
\alias{plotSpatialEffects}
\concept{B score method}
\concept{Spatial normalization}
\concept{spatial effects}
\title{Plate plot with the row and column offsets estimated by the B score method}
\description{
  The function plots the per-plate row and column effects estimated by the B score method or by the spatial normalization.
}
\usage{
plotSpatialEffects(object, channel = 1, plates)

}
\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already been normalized using the B score method  or other spatial normalization (see details).}
  \item{channel}{a numeric value giving the channel of \code{object} to plot.}
  \item{plates}{a vector of integers giving the plate numbers to plot. If missing, the function considers all of the plates.}
}

\details{
The function plots the \code{\link[prada:plotPlate]{plate plots}} displaying the row and column offsets (stored in slot \code{rowcol.effects} of the \code{\linkS4class{cellHTS}} object) within the plates in \code{plates}, and for channel \code{channel}, as determined by the \code{\link[cellHTS2:Bscore]{B score method}} or \code{\link[cellHTS2:spatialNormalization]{spatial normalization}}.
Before plotting the spatial offsets, the values within the chosen channel (\code{channel}) are transformed in order to be confined in the range \eqn{[-1, 1]}, as follows:

\deqn{y^t = \frac{(y - min(y))}{max(y)-min(y)}}{%
yt = (y - min(y))/(max(y) - min(y))}

Here, \eqn{y^t}{yt} are the transformed values, and \eqn{y} the estimated spatial effects.
The maximum and the minimum values are calculated using all the values in 
\code{object@rowcol.effects[,,,channel]}.
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\seealso{
  \code{\link[prada:plotPlate]{plotPlate}},
  \code{\link[cellhts2:Bscore]{Bscore}},
  \code{\link[cellhts2:spatialNormalization]{spatialNormalization}},
  \code{\link[cellhts2:normalizePlates]{normalizePlates}},
%  \code{\link[cellhts2:summarizeChannels]{summarizeChannels}}
}

\examples{
    data(KcViabSmall)
    x <- normalizePlates(KcViabSmall, scale="multiplicative", log=TRUE, method="Bscore", save.model = TRUE)
    ## see plate plots with the row and column estimated offsets for plates 1 and 3:
    plotSpatialEffects(x, plates=c(1,3))
}
\keyword{manip}
