\name{getMeasureRepAgreement}
\alias{getMeasureRepAgreement}
\title{Measures of agreement between plate replicates from a cellHTS object}
\description{
  Calculate the agreement between plate replicates using raw data or normalized data stored in a 
  \code{\linkS4class{cellHTS}} object. 
  This function calculates the repeatability standard deviation between replicate plates and the correlation coefficient between replicates. If there are more than 2 replicates, the minimum and maximum correlation between replicates is given. These measures are calculated only for \code{sample} wells.
}

\usage{
getMeasureRepAgreement(x, corr.method = "spearman")
}

\arguments{
  \item{x}{a configured \code{\linkS4class{cellHTS}} object. See details.}
  \item{corr.method}{a character string indicating which correlation coefficient should be computed. Can be either "pearson", "kendall" or "spearman" (default). The correlation is calculated by calling the function 
\code{\link[stats:cor]{cor}}.}
}

\details{
Given an already configured \code{\linkS4class{cellHTS}} object (\code{state(x)[["configured"]]=TRUE}), this function calculates the repeatability standard deviation between replicate plates and the correlation coefficient between plate replicates using only the \code{sample} wells. If there are more than 2 replicates, the minimum and maximum correlation value between pairs of replicates are given.

These measures are calculated using the data values stored in slot \code{assayData} of the \code{x}.

For a given plate \eqn{p}, the repeatability standard deviation is determined as the square root of the average of 
the squared standard deviations (\eqn{sr}) calculated for each sample well \eqn{k} by considering the measurement of all of the replicates:

% repeatability standard deviation:

\deqn{RepStDev_{p} = \sqrt{\frac{\sum{sr^2}}{n_{k} } } }{%
RepStDev_{p} = \sqrt{\frac{\sum{sr^2}}{n_{k} } } } 

where \eqn{n_{k}} is the total number of sample probes for plate \eqn{p}. 
}

\seealso{
  \code{\link[cellHTS2:configure]{configure}}, 
  \code{\link[cellHTS2:writeReport]{writeReport}}
}

\value{
  The function generates a list with elements: 

"repStDev": matrix with the calculated repeatability standard deviation between plate replicates. It has dimensions \code{nrPlates x nrChannels};

"corrCoef" (if the number of replicates equals 2): matrix with the correlation coefficients between plate replicates. It has dimensions: \code{nrPlates x nrChannels};

"corrCoef.min" (if the number of replicates is greater than 2): matrix with the minimum value of the correlation coefficients between plate replicates. It has dimensions \code{nrPlates x nrChannels};

"corrCoef.max" (if the number of replicates is greater than 2): matrix with the maximum value of the correlation coefficients between plate replicates. It has dimensions \code{nrPlates x nrChannels}.

}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    repAgree <- getMeasureRepAgreement(KcViabSmall)
    x <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="none")
    repAgree <- getMeasureRepAgreement(x)
}

\keyword{manip}
