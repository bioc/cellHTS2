\name{getDynamicRange}
\alias{getDynamicRange}
\title{Per-plate dynamic range of a cellHTS object}
\description{
  Calculates per-plate dynamic range of raw data or normalized data stored in a \code{\linkS4class{cellHTS}} object.}
\usage{

getDynamicRange(x, 
verbose=interactive(), 
definition, 
posControls, 
negControls)
}

\arguments{
  \item{x}{a configured \code{\linkS4class{cellHTS}} object. See details.}
  \item{verbose}{a logical, if \code{TRUE} the function reports some of its intermediate progress. The default is the state of \code{\link[base:interactive]{interactive()}}.}
  \item{definition}{a character string with possible values "ratio" or "difference". See details.}
  \item{posControls}{(optional) a list or vector of regular expressions specifying the name of the positive controls. See details.}
  \item{negControls}{(optional) a vector of regular expressions specifying the name of the negative controls. See details.}
}

\details{

\code{x} should be an already configured \code{\linkS4class{cellHTS}} object (\code{state(x)["configured"]=TRUE}), so that the information about the well annotation of the plates is available.

If \code{x} is normalized (\code{state(x)["normalized"]=TRUE}), the per-plate dynamic ranges are calculated for the normalized data (content of slot \code{xnorm}), otherwise the raw data are considered (slot \code{xraw}).

If \code{definition="difference"}, the dynamic range is calculated as the absolute difference between the arithmetic average on positive and negative controls.

If \code{definition="ratio"}, the dynamic range is calculated as the ratio between the geometric mean on positive and negative controls.

\emph{NOTE:} the argument \code{definition} should only be set to \code{"ratio"} if data are in positive scale!

If \code{definition} is missing it is determined from the scale of the data. By default, if data are in positive scale, definition is set to \code{"ratio"}, otherwise, it is set to \code{"difference}.

\code{posControls} and \code{negControls} should be given as a vector of regular expression patterns specifying the name of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file 
(and stored in \code{x@wellAnno}). The length of these vectors should be equal to the number of reporters used in the screen (\code{dim(x@xraw)[4]} or to \code{dim(x@xnorm)[4]}, in case \code{x} contains multi-channel data that has been normalized by combining the values from two or more channels).
  By default, if \code{posControls} is not given, \code{pos} will be taken as the name for the wells containing positive controls. Similarly, if \code{negControls} is missing, by default \code{neg} will be considered as the name used to annotated the negative controls. 
  The content of \code{posControls} and \code{negControls} will be
  passed to \code{\link[base:regexpr]{regexpr}} for pattern matching
  within the well annotation given in \code{x@wellAnno} (see
  examples). If no controls are available for a given channel, use
  \code{""} or \code{NA} for that channel. For example,
  \code{posControls = c("", "(?i)^diap$")} means that channel 1 has no
  positive controls, while \code{diap} is the positive control for channel 2.

  The arguments \code{posControls} and \code{negControls} are particularly useful in multi-channel data since the controls might be reporter-specific, or after normalizing multi-channel data.

If there are different positive controls, the dynamic range is calculated between each of the positive controls and the negative controls.

In the case of a two-way assay, where two types of "positive" controls are used in the screen ("activators" and "inhibitors"), \code{posControls} should be defined as a list with two components (called \code{act} and \code{inh}), each of which should be vectors of regular expressions of the same length as the current number of reporters (as explained above). The dynamic range is calculated between each type of positive control (\code{activators} or \code{inhibitors}) and the negative controls.

}
\seealso{
  \code{\link[cellhts2:configure]{configure}}, 
  \code{\link[cellhts2:writeReport]{writeReport}}
}

\value{
  The function generates a list with the per-plate dynamic ranges in each channel and each replicate. The average dynamic range between replicates is also given.
  Each element of this list is an array of dimensions \code{nrPlates x (nrReplicates + 1) x nrChannels}, and is named by the positive controls. In the case of a two-way assay, these elements are called \code{activators} and \code{inhibitors}, while for a one-way assay, the elements have the same name of the positive controls. See examples section.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\examples{
    data(KcViabSmall)
    ## pCtrls <- c("pos") 
    ## nCtrls <- c("neg") 
    ## or for safety reasons (not a problem for the current well annotation, however) 
    pCtrls <- c("^pos$") 
    nCtrls <- c("^neg$")
    dr <- getDynamicRange(KcViabSmall, definition="ratio", posControls=pCtrls, negControls=nCtrls)
    ## same as: 
    ## getDynamicRange(KcViabSmall)
    
    x <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="byExperiment")
    dr <- getDynamicRange(x, definition="ratio")
    
}

\keyword{manip}
