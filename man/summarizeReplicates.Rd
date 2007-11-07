\name{summarizeReplicates}
\alias{summarizeReplicates}


\title{Summarize between scored replicate values given in a cellHTS object to obtain a single value for each probe}
\description{
This function summarizes the replicate values stored in slot \code{assayData} of a \code{\linkS4class{cellHTS}} object 
and calculates a single score for each probe. Data are stored in slot \code{assayData} overridding its current content.

Currently this function is implemented only for single-color data.
}
\usage{
summarizeReplicates(object, summary ="min")
}
\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}} that has already been normalized and scored (see details).}
  \item{summary}{a character string indicating how to summarize between replicate measurements. 
    One of "min" (default), "mean", "median", "max", "rms", "closestToZero", or "FurthestFromZero" can be used (see details).}
}
\details{
 A single value per probe is calculated by summarizing between scored replicates stored in the slot \code{assayData} of \code{object}. The summary is performed as follows:
\itemize{
\item If \code{summary="mean"}, the average of replicate values is considered; 
\item If \code{summary="median"}, the median of replicate values is considered; 
\item If \code{summary="max"}, the maximum of replicate intensities is taken; 
\item If \code{summary="min"}, the minimum is considered, instead; 
\item If \code{summary="rms"}, the square root of the mean squared value of the replicates (root mean square) is taken as a summary function;
\item If \code{summary="closestToZero"}, the value closest to zero is taken as a summary (useful when both sides of the distribution of z-score values are of interest);
\item If \code{summary="furthestFromZero"}, the value furthest from zero is taken as a summary (useful when both sides of the distribution of z-score values are of interest).
}
}

\value{
  The \code{cellHTS} object with the summarized scored values stored in slot \code{assayData}. This is an object of class \code{assayData} corresponding to a single matrix of dimensions Features x 1.
  Moreover, the processing status of the \code{cellHTS} object is updated
  in the slot \code{state} to \code{object@state[["scored"]]= TRUE}.}

\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}},
  \code{\link[cellHTS2:scoreReplicates]{scoreReplicates}},
  \code{\link[cellHTS2:imageScreen]{imageScreen}}.
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    # normalize
    x <- normalizePlates(KcViabSmall, scale="multiplicative", method="median", varianceAdjust="none")
    # score the replicates
    x <- scoreReplicates(x, sign="-", method="zscore")
    # summarize the replicates (conservative approach: take the minimum value between replicates)
    x <- summarizeReplicates(x, summary="min")
}
\keyword{manip}
