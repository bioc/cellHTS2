\name{ROC-class}
\docType{class}
\alias{ROC-class}

\alias{show,ROC-method}
%\alias{plot,ROC-method}
%\alias{plot,ROC,missing-method}
%\alias{lines,ROC-method}

\title{
  Class that contain data that can be plotted as a ROC curve.
}

\description{

Container for data that represent a receiver-operator-characteristic curve, and that were generated 
  from the data of the annotated positive and negative controls in a 
  scored \code{\linkS4class{cellHTS}} object.
}

\section{Creating Objects}{
  \code{new("ROC")}

  \code{ROC(object, positives, negatives)} with \code{object} being an
  \code{\linkS4class{cellHTS}} instance.
}


\section{Slots}{
 \describe{
 \item{\code{name}:}{a character of length 1 with the name of the experiment from which the ROC object derives.}
 \item{\code{assayType}:}{a character of length 1 with the type of screenning assay. Possible values are: "one-way assay" and "two-way assay".}
  \item{\code{TP}:}{a vector of integers of length 1000.}
  \item{\code{FP}:}{a vector of integers of length 1000.}
 \item{\code{posNames}:}{a character vector with the name of the positive controls.}
 \item{\code{negNames}:}{a character vector with the name of the negative controls.}
   }
}

\section{Methods}{
  \describe{
%    \item{\code{[}}{Subset}
    \item{\code{show}}{Print a summary of the object.}
   \item{\code{plot}}{Plot the ROC curve corresponding to the object.}
   \item{\code{lines}}{Line plot of the ROC object.}
  }
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\seealso{
  \code{\link[cellHTS2:ROC]{ROC}}
}

\examples{
    showClass("ROC")
    showMethods(class="ROC")
  
    \dontrun{
    data(KcViabSmall)
    x <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="none")
    x <- scoreReplicates(x, sign="-", method="zscore")
    x <- summarizeReplicates(x, summary="mean")
    y <- ROC(x)
    plot(y)
    lines(y, col="green")
    show(y)
    }
}

\keyword{classes}
