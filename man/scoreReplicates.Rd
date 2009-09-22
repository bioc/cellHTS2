\name{scoreReplicates}
\alias{scoreReplicates}


\title{Scores normalized replicate values given in a cellHTS object}
\description{
This function scores the normalized replicate values stored in slot \code{assayData} of a \code{\linkS4class{cellHTS}} object.
Current available options are to take the \emph{z}-score value or the per-replicate normalized percent inhibition (NPI). Data are stored in slot \code{assayData} overridding its current content.
}
\usage{
scoreReplicates(object, sign="+", method="zscore", \dots)
}
\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}} that has already been normalized.}
  \item{sign}{a character string, either "+" (default) or "-", which corresponds to multiplying the data by \code{+1} or \code{-1}, respectively, after applying the scoring method specified by argument \code{method}. See details.}
  \item{method}{a character string indicating which method to use to score the replicate measurements. Available options are "none", "zscore" (default), "NPI". See details.}
  \item{\dots}{additional parameters required by some of the methods chosen in \code{method}.}
}
\details{

 This function scores the normalized values given in the slot \code{assayData} of \code{object}.
 Current availabe scoring methods are:
 \itemize{
        \item \code{method="none"}, no scoring is applied.

	\item \code{method="zscore"} (robust \emph{z}-scores), for each replicate, this is calculated by subtracting the overall median from each measurement and dividing the result by the overall \code{\link[stats:mad]{mad}}. These are estimated for each replicate by considering the distribution of intensities (over all plates) in the wells whose content is annotated as \code{sample}.

	\item \code{method="NPI"} (normalized percent inhibition applied in a per-replicate basis, i.e. using the overall mean of positive and negative controls across all plates of a given replicate), for each replicate, this method consists of subtracting each measurement from the average of the intensities on the positive controls (taken across all plates), and this result is then divided by the difference between the averages of the measurements on the positive and the negative controls (taken across all plates). If this method is chosen, one may need to provide further arguments to \code{scoreReplicates}, namely, arguments \code{posControls} and \code{negControls}. These arguments should be vectors of regular expression patterns specifying the name of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file. The length of these vectors should match the current number of channels in \code{object} (i.e. \code{dim(Data(object))[3]}). By default, if \code{posControls} or \code{negControls} are not given, \emph{pos} and \emph{neg} will be taken as the name for the wells containing positive or negative controls. The content of \code{posControls} and \code{negControls} is passed to \code{\link[base:grep]{regexpr}} for pattern matching within the well annotation given in the \code{featureData} slot of \code{object} (which can be accessed via \code{wellAnno(object)}) (see examples for \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}}). 
}

 After replicate scoring using the chosen method, the value given in \code{sign} ("+" or "-") is used to set the sign of the calculated scores. 
For example, with a \code{sign="-"}, a strong decrease in the signal will be represented 
 by a positive score, whereas setting \code{sign="+"}, such a phenotype will be represented by a negative score. This option can be set to calculate the results to the commonly used convention.
}

\value{

  A \code{cellHTS} object with its slot \code{assayData} replaced with the scored values (same dimension). 

  \emph{Important:} Note that the processing state "scored" of the \code{cellHTS} object is only updated to \code{TRUE}
  after summarizing the replicates, which is the next preprocessing step (see \code{\link[cellHTS2:summarizeReplicates]{summarizeReplicates}}).
}

\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}},
  \code{\link[cellHTS2:summarizeReplicates]{summarizeReplicates}}.
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- normalizePlates(KcViabSmall, scale="multiplicative", method="median", varianceAdjust="none")
    x <- scoreReplicates(x, sign="-", method="zscore")
    x <- summarizeReplicates(x, summary="min") # conservative approach
}
\keyword{manip}
