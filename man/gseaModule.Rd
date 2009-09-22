\name{gseaModule}
\alias{gseaModule}
\alias{length,GeneSet-method}

\title{
Constructor for an object of class gseaModule
}


\description{
gseaModule objects encapsulate all the information that is necessary to
add the results of a gene set enrichment analysis to a cellHTS
report. This feature is still experimental
}

\usage{
gseaModule(geneSets, statFuns, scores, annotation)
}

\arguments{
  
  \item{geneSets}{An object of class
    \code{\link[GSEABase:GeneSetCollection-class]{GeneSetCollection}}
    containing the information about gene sets for which to perform the
    analysis.}

  \item{statFuns}{A list of functions to compute per gene set
    statistics. These will be called by
    \code{\link[Category:applyByCategory]{applyByCategory}} and need to
    be able to handle two mandatory arguments: \code{x} are the scores
    for the respective category, and \code{y} are all scores of the
    whole assay. This allows for statistics like \code{t.test(x,y)}.}
  
    \item{scores}{An optional numeric vector of assay scores. This
      should be extended to also handle numeric matrices for multi-channel
      assays. }

    \item{annotation}{A \code{data.frame} with additional annotation for the
      repsective gene sets.}

}
\details{
  The resulting gseaModule object can be supplied as an additional
  argument to \code{\link{writeReport}}.
  
  This feature is still experimental.
}


\value{
An object of class gseaModule.
}

\author{
  Florian Hahne
}

