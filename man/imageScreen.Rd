\name{imageScreen}
\alias{imageScreen}
\title{Experiment-wide quality control plot of a cellHTS object}
\description{Experiment-wide quality control plot of a scored \code{cellHTS} object.}
\usage{
imageScreen(object, ar=3/5, zrange=NULL, map=FALSE, anno=NULL,
            col=list(posNeg=rev(brewer.pal(11,
                 "RdBu"))[c(1:5, rep(6, 3), 7:11)], pos=brewer.pal(9,
                 "Greys")), nbImageBins=256, nbLegendBins=7)
}

\arguments{
  
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already
    been scored (i.e. \code{state(object)['scored']=TRUE}).}

  \item{ar}{the desired aspect ration for the image plot (i.e. number of
    columns per number of rows)}

  \item{zrange}{the range of values to be mapped into the color
    scale. If missing, \code{zrange} will be set to the range of the
    score values stored in slot \code{assayData} of \code{object}.}
  
  \item{map}{a logical value that determines whether an image map should
    be created using tooltips to indicate the annotation at each
    position. It only makes sense to set it to \code{TRUE} when the
    function is called from \code{writeReport} function, so the default
    is \code{FALSE}.}

  \item{anno}{optional input giving the annotation information for the
    mapping. It should be a vector of the same size as the total number
    of featured in \code{object}. See details.}

  \item{col}{a list giving the colors for the plot. The first element
    \code{posNeg} is used if \code{zrange[1]<0} and \code{zrange[2]>0},
    the second if all values are either positive or negative.}

  \item{nbImageBins}{The number of color bins used in the
    map. Default is 256.}
  
  \item{nbLegendBins}{The number of color bins shown in the
    legend. Default is 7.}
}

\details{
This function creates an image plot that gives an overview of the whole set of score values stored in slot \code{assayData} of a scored \code{cellHTS} object.
When the annotation mapping is performed, by default, \code{anno} is set to: 
\enumerate{
\item (if \code{object} is annotated) The content of column named \code{GeneSymbol} or named \code{GeneID} (if the former is not available) of the \code{featureData} slot of \code{object};
\item The position within the plate, if \code{object} is not annotated.
 }%enumerate
}

\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}},
  \code{\link[cellHTS2:scoreReplicates]{scoreReplicates}},
  \code{\link[cellHTS2:summarizeReplicates]{summarizeReplicates}},
  \code{\link[cellHTS2:Data]{Data}}
  \code{\link[cellHTS2:writeReport]{writeReport}}
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall) 
    x <- KcViabSmall   
    x <- normalizePlates(x, scale="multiplicative", log=FALSE, method="median", varianceAdjust="none")
    x <- scoreReplicates(x, sign="-", method="zscore") 
    x <- summarizeReplicates(x, summary="min") 
    imageScreen(x, zrange=c(-5,5))
}

\keyword{manip}
