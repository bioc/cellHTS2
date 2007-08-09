\name{ROC}
\alias{ROC}
\alias{ROC,cellHTS-method}
%\alias{plot,ROC-method}
\alias{plot,ROC,missing-method}
\alias{lines,ROC-method}

\title{Creates an object of class "ROC" which can be plotted as a ROC curve}
\description{
  The function \code{ROC} construct an object of S4 class \code{\linkS4class{ROC}},
  which represents a receiver-operator-characteristic curve,
  from the data of the annotated positive and negative controls in a
  scored \code{\linkS4class{cellHTS}} object.
}
\usage{
\S4method{ROC}{cellHTS}(object, positives, negatives)
\S4method{plot}{ROC,missing}(x, col="darkblue", type="l", main = "ROC curve", \dots)
\S4method{lines}{ROC}(x, \dots)
}


\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already been scored (see details).}
  \item{positives}{a list or vector of regular expressions specifying the name of the positive controls. 
   See the details for the argument \code{posControls} of \code{writeReport} function.}
  \item{negatives}{a vector of regular expressions specifying the name of the negative controls. 
   See the details for the argument \code{negControls} of \code{writeReport} function.}
  \item{x}{a \code{\linkS4class{ROC}} object obtained using function \code{ROC}.}
  \item{col}{the graphical parameter for color; see \code{\link{par}} for details.}
  \item{type}{the graphical parameter giving the type of plot desired; see \code{\link{par}} for details.}
  \item{main}{the graphical parameter giving the desired title of plot; see \code{\link{par}} for details.}
  \item{\dots}{other graphical parameters as in \code{\link{par}} may be also passed as arguments.}
}
\details{
  The \code{\linkS4class{cellHTS}} object \code{object} must be already scored (\code{state(object)["scored"]=TRUE}),
  and selection proceeds from large to small values of this score.
  Furthermore, \code{object} is expected to contain positive and negative
  controls annotated in the slot \code{wellAnno} with the values of
  the arguments \code{positives} and \code{negatives}, respectively.
  If the assay is a two-way experiment, \code{positives} should be a list with components \code{act} 
  and \code{inh}, specifying the name of the activators, and inhibitors, respectively. In this case, the ROC curve is constructed based on the absolute values of \code{object@score}.}

\value{An S4 object of class \code{\linkS4class{ROC}}. There are methods \code{show},
  \code{plot} and \code{lines}. }

\author{Ligia P. Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    \dontrun{
    x <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="byExperiment")
    x <- summarizeReplicates(x, zscore="-", summary="mean")
    y <- ROC(x)
    plot(y)
    lines(y, col="green")
    show(y)
    }
}
\keyword{univar}
