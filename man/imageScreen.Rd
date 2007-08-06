\name{imageScreen}
\alias{imageScreen}
\title{Experiment-wide quality control plot of a cellHTS object}
\description{Experiment-wide quality control plot of a scored \code{cellHTS} object.}
\usage{
imageScreen(object, ar = 3/5, zrange, map = FALSE, anno)
}

\arguments{
\item{object}{a \code{\linkS4class{cellHTS}} object that has already been scored (i.e. containing the slot \code{score}).}
\item{ar}{the desired aspect ration for the image plot (i.e. number of columns per number of rows)} 
\item{zrange}{the range of values to be mapped into the color
      scale. If missing, zrange will be set to the range of \code{object@score}.}
\item{map}{a logical value that determines whether an image map should be created using tooltips to indicate the annotation at each position. It only makes sense to set it to \code{TRUE} when the function is called from \code{writeReport} function, so the default is \code{FALSE}.}
\item{anno}{optional input giving the annotation information for the mapping. It should be a vector of the same size as
\code{object@score}. See details.}
}

\details{
This function creates an image plot that gives an overview of the whole set of score values stored in slot \code{score} 
of \code{object}. 
When the annotation mapping is performed, by default, \code{anno} is set to: 
\enumerate{
\item The content of \code{object@geneAnno$GeneSymbol} (or \code{object@geneAnno$GeneID}, if the former is not available), if \code{object} is annotated;
\item The position within the plate, if \code{object} is not annotated yet.
}
}

\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}},
  \code{\link[cellHTS2:summarizeReplicates]{summarizeReplicates}},
  \code{\link[cellHTS2:scores]{scores}}
%  \code{\link[cellHTS2:writeReport]{writeReport}}
}

\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall) 
    x <- KcViabSmall   
    x <- normalizePlates(x, scale="additive", log=FALSE, method="median", varianceAdjust="byExperiment")
    x <- summarizeReplicates(x, zscore="-", summary="min") 
    imageScreen(x, zrange=c(-5,5))
}

\keyword{manip}
