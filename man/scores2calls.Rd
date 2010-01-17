\name{scores2calls}
\alias{scores2calls}

\title{Sigmoidal transformation of the score values stored in a cellHTS object obtaining the call values for each probe.}
\description{
Apply a sigmoidal transformation with parameters z0 and lambda to the summarized scored values stored in a \code{\linkS4class{cellHTS}} object. 
The obtained results are called \emph{calls} and are stored in slot \code{assayData}, overridding its current content.

Currently this function is implemented only for single-color data.
}
\usage{
scores2calls(x, z0, lambda)
}
\arguments{
  \item{x}{an object of class \code{\linkS4class{cellHTS}} containing replicate data that have already been scored and summarized (see details).}
  \item{z0}{a numeric value giving the centre of the sigmoidal transformation. See details.}
  \item{lambda}{a numeric value (>0) that corresponds to the parameter \code{lambda} of the sigmoidal transformation. This value should be \code{>0}, but usually it makes more sense to use a value \code{>=1}. See details.} 
}
\details{

This function applies a sigmoidal transformation with parameters z0 and lambda to the single per-probe score values stored 
in a \code{cellHTS} object. The obtained results are called \emph{calls}. The transformation is given by:

\deqn{1 / (1 + exp(-lambda * (z- z0)))}

where \code{z} are the score values, \code{z0} is the centre of the sigmoidal transformation, and the \code{lambda} is a parameter that controls the smoothness of the transformation. The higher is \code{lambda}, more steeper is the transition from lower to higher values. \code{lambda} should be \code{> 0}, but usually it makes more sense to use a value \code{>=1}.

This transformation maps the score values to the interval \code{[0,1]}, and is intended to expand the scale of scores with intermediate values and shrink the ones showing extreme values, therefore making the difference between intermediate phenotypes larger.
}

\value{
  The \code{cellHTS} object with the call values stored in slot \code{assayData}. This is an object of class \code{assayData} corresponding to a single matrix of dimensions \code{Features x 1}.
}
\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}},
  \code{\link[cellHTS2:scoreReplicates]{scoreReplicates}},
  \code{\link[cellHTS2:summarizeReplicates]{summarizeReplicates}},
  \code{\link[cellHTS2:imageScreen]{imageScreen}}.
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- normalizePlates(KcViabSmall, scale="multiplicative", method="median", varianceAdjust="none")
    x <- scoreReplicates(x, sign="-", method="zscore")
    x <- summarizeReplicates(x, summary="min")
    xc <- scores2calls(x, z0=1.5, lambda=2) 
    plot(Data(x), Data(xc), col="blue", xlab="z-scores", ylab="calls", main=expression(1/(1+e^{-lambda *(z-z[0])})))
    if(require(splots)) {
     sp = split(Data(xc), plate(xc))
     plotScreen(sp, zrange=c(0,1), fill=c("white", "red"), na.fill="yellow",
               main="Calls", ncol=3L)
    }
}
\keyword{manip}
