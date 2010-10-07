\name{Bscore}
\alias{Bscore}

\title{B score normalization}
\description{
  Correction of plate and spatial effects of data stored in slot
  \code{assayData} of a \code{\linkS4class{cellHTS}} object using
  the B score method (without variance adjustment of the residuals).
  Using this method, a two-way median polish is fitted, on a per-plate
  basis, to account for row and column effects.
}
\usage{
Bscore(object, save.model = FALSE)
}
\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already been configured. See details.}
  \item{save.model}{a logical value specifying whether the per-plate models should be stored in slots \code{rowcol.effects} and \code{overal.effects}. See details.}
}

\details{
  This function is usually not called directly by the user,
  but from within the
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}} function.
  The normalization is performed in a per-plate fashion using the
  B score method, for each replicate and channel. 
  In the B score method, the residual \eqn{r_{ijp}} of the measurement
  for row \emph{i} and column \emph{j} on the \emph{p}-th plate is
  obtained by fitting a \link[stats:medpolish]{two-way median polish},
  in order to account for both row and column effects within the plate:

  \deqn{
    r_{ijp} = y_{ijp} - \hat{y}_{ijp} =
    y_{ijp} - (\hat{\mu}_p + \hat{R}_{ip} + \hat{C}_{jp})}{%
    r_{ijp} = y_{ijp} - yest_{ijp} =
    y_{ijp} - (mu_p + R_{ip} + C_{jp})}

  \eqn{y_{ijp}} is the measurement value in row \emph{i} and
  column \emph{j} of plate \emph{p} 
  (taken from slot \code{assayData} - only sample wells are considered),
  and \eqn{\hat{y}_{ipj}}{yest_{ijp}} is the corresponding fitted value. 
  This is defined as the sum between the estimated average
  of the plate (\eqn{\hat{\mu}_p}{mu_p}), the estimated systematic
  offset for row \emph{i} (\eqn{\hat{R}_{ip}}{R_{ip}}),
  and the systematic offset for column \emph{j} (\eqn{\hat{C}_{jp}}{C_{jp}}).

\emph{NOTE:}
In the original B score method, as presented by Malo et al.,
a further step is performed: for each plate \emph{p},
each of the obtained residual values \eqn{r_{ijp}}'s are divided by
the median absolute deviation of the residuals in plate \emph{p}
(\eqn{MAD_p}{MAD_p}), resulting in:

\deqn{\frac{r_{ijp}}{MAD_p}}{r_{ijp}/MAD_p}

The intention of such a further adjustment is to compensate
for plate-to-plate variability in dynamic range.
In the \code{Bscore} function, this step is not automatically
performed,
but can be done if B score normalization is called using the
function \code{\link[cellHTS2:normalizePlates]{normalizePlates}}
with arguments \code{method="Bscore"} and
\code{varianceAdjust="byPlate"}. See the latter function for more details. 

If \code{save.model=TRUE}, the models row and column offsets and
overall offsets are stored in the slots 
\code{rowcol.effects} and \code{overall.effects} of \code{object}.
}

\value{
An object of class \code{\linkS4class{cellHTS}} with B-score normalized data stored in slot \code{assayData}. 

Furthermore, if \code{save.model=TRUE}, the row and column effects and the overall effects are stored in slots \code{rowcol.effects} and \code{overall.effects} , respectively. 
The latter slots are arrays with the same dimension as \code{Data(object)}, except the 
\code{overall.effects} slot, which has dimensions \code{nr Plates x nr Samples x nr Channels}.

After calling this function, the processing status of the \code{\linkS4class{cellHTS}} object is updated
in the slot \code{state} to \code{object@state["normalized"]=TRUE}.
}

\author{Ligia Bras}

\seealso{
  \code{\link[stats:medpolish]{medpolish}},
  \code{\link[stats:loess]{loess}},
  \code{\link[locfit:locfit.robust]{locfit.robust}},
  \code{\link[cellHTS2:plotSpatialEffects]{plotSpatialEffects}},
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}}
  \code{\link[cellHTS2:plateEffects]{plateEffects}}
}

\references{

Brideau, C., Gunter, B., Pikounis, B. and Liaw, A. (2003) Improved statistical methods for hit selection in high-throughput screening, \emph{J. Biomol. Screen} \bold{8}, 634--647.

Malo, N., Hanley, J.A., Cerquozzi, S., Pelletier, J. and Nadon, R. (2006) Statistical practice in high-throughput screening data analysis, \emph{Nature Biotechn} \bold{24}(2), 167--175. 

Boutros, M., Br\'as, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\examples{
data("KcViabSmall")
xb <- Bscore(KcViabSmall, save.model = TRUE)
## Calling Bscore function from "normalizePlates" and adding the per-plate variance adjustment step:
xopt <- normalizePlates(KcViabSmall, method="Bscore", varianceAdjust="byPlate", save.model = TRUE)
## Access the slots overall.effects and rowcol.effects
ef1 = plateEffects(xb)
ef2 = plateEffects(xopt)


## double-check 
stopifnot(
  all(xb@rowcol.effects==xopt@rowcol.effects, na.rm=TRUE),
  all(xb@overall.effects==xopt@overall.effects, na.rm=TRUE),
  identical(ef1, ef2)
)

}
\keyword{manip}
