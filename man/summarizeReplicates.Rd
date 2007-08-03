\name{summarizeReplicates}
\alias{summarizeReplicates}


\title{Summarize between normalized replicate values given in a cellHTS object obtaining a single z-score for each probe}
\description{
This function Summarizes the normalized replicate values stored in slot \code{xnorm} of a \code{\linkS4class{cellHTS}} object, and calculates a single \emph{z}-score value for each probe.

Currently this function is implemented only for single-color data.
}
\usage{
summarizeReplicates(object, zscore="+", summary ="min")
}
\arguments{
  \item{object}{a object of class \code{\linkS4class{cellHTS}} that has already been normalized (see details).}
  \item{zscore}{a character string, either "+" (default) or "-", whi  \code{\link[cellHTS:summarizeChannels]{summarizeChannels}}ch will be used to set the sign to calculate the \emph{z}-scores before summarization. See details.}
  \item{summary}{a character string indicating how to summarize between replicate measurements. 
    One of "min" (default), "mean", "max", "rms", "closestToZero", or "FurthestFromZero" can be used (see details).}
}
\details{
 Given the normalized values given in the slot \code{xnorm} of \code{object}, a single \emph{z}-score is calculated for each probe.

 Before replicates summarization, a robust \emph{z}-score is calculated for each plate and each well by subtracting the overall median and dividing by the overall \code{\link[stats:mad]{mad}}. These are estimated for each replicate by considering the distribution of intensities (over all plates) in the wells whose content is annotated as \code{sample}. 
 The allowed values for \code{zscore} ("+" or "-") are used to set the sign of 
 the calculated \emph{z}-scores. For example, with a \code{zscore="-"} a strong decrease in the signal will be represented 
 by a positive \emph{z}-score, whereas setting \code{zscore="+"}, 
 such a phenotype will be represented by a negative \emph{z}-score.  
 This option can be set to calculate the results to the commonly used convention.
 
 Finally, a single \emph{z}-score per probe is calculated by summarizing between scored replicates:
\itemize{
\item If \code{summary="mean"}, the average of replicate values is considered; 
\item If \code{summary="max"}, then the maximum of replicate intensities is taken; 
\item If \code{summary="min"}, the minimum is considered, instead (conservative); 
\item If \code{summary="rms"}, the square root of the mean squared value of the replicates (root mean square) is taken as a summary function;
\item If \code{summary="closestToZero"}, the value closest to zero is taken as a summary (useful when both sides of the distribution of z-score values are of interest);
\item If \code{summary="furthestFromZero"}, the value furthest from zero is taken as a summary (useful when both sides of the distribution of z-score values are of interest).
}
}

\value{
  The \code{cellHTS} object with the scored values stored in slot \code{score}. 
  \code{object@score} is a numeric vector containing the z-factor for each well in
  every plate. The length of this vector is therefore equal to the product
  between the number of wells per plate and the number of plates.
  Moreover, the processing status of the \code{cellHTS} object is updated
  in the slot \code{state} to \code{state["scored"]= TRUE}.}

\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}},
  \code{\link[cellHTS2:scores]{scores}}
}


\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- normalizePlates(KcViabSmall, scale="additive", method="median", varianceAdjust="byExperiment")
    x <- summarizeReplicates(x, zscore="-", summary="min")
}
\keyword{manip}
