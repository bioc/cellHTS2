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
  \item{object}{a \code{\linkS4class{cellHTS}} object which replicate data have already been scored and summarized (see details).}
  \item{positives}{a list or vector of regular expressions specifying the name of the positive control(s). 
   See the details for the argument \code{posControls} of \code{writeReport} function. The default is \code{"^pos$"}.}
  \item{negatives}{a vector of regular expressions specifying the name of the negative control(s). 
   See the details for the argument \code{negControls} of \code{writeReport} function. The default is \code{"^neg$"}.}
  \item{x}{a \code{\linkS4class{ROC}} object obtained using function \code{ROC}.}
  \item{col}{the graphical parameter for color; see \code{\link{par}} for details.}
  \item{type}{the graphical parameter giving the type of plot desired; see \code{\link{par}} for details.}
  \item{main}{the graphical parameter giving the desired title of plot; see \code{\link{par}} for details.}
  \item{\dots}{other graphical parameters as in \code{\link{par}} may be also passed as arguments.}
}
\details{
  The \code{\linkS4class{cellHTS}} object \code{object} must be already scored (\code{state(object)["scored"]=TRUE}),
  and selection proceeds from large to small values of this single per-probe score.
  Furthermore, \code{object} is expected to contain positive and negative
  controls annotated in the column \code{controlStatus} of the \code{featureData} slot - which can be accessed via \code{wellAnno(object)}. The arguments \code{positives} and \code{negatives} should be given as regular expression patterns specifying the name of the positive(s) and negative(s) controls, respectivey. 
  By default, if \code{positives} is not given, \emph{pos} will be taken as the name for the wells containing positive controls. Similarly, if \code{negatives} is missing, by default \emph{neg} will be considered as the name used to annotate the negative controls. 
  The content of \code{posControls} and \code{negControls} are passed to \code{\link[base:grep]{regexpr}} for pattern matching within the well annotation (see examples for \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}}). 
  If the assay is a two-way experiment, \code{positives} should be a list with components \code{act} 
  and \code{inh}, specifying the name of the activators, and inhibitors, respectively. In this case, the ROC curve is constructed based on the absolute values of \code{Data(object)}.}

\value{An S4 object of class \code{\linkS4class{ROC}}. There are methods \code{show},
  \code{plot} and \code{lines}. }

\author{Ligia P. Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="byExperiment")
    x <- scoreReplicates(x, sign="-", method="zscore")
    x <- summarizeReplicates(x, summary="mean")
    y <- ROC(x)
    plot(y)
    lines(y, col="green")
    show(y)
}
\keyword{univar}
