\name{configurationAsScreenPlot}
\alias{configurationAsScreenPlot}
\title{Screen plot of the plate configuration of a cellHTS object}
\description{Screen plot displaying the plate configuration of a \code{cellHTS} object.}
\usage{
configurationAsScreenPlot(x, verbose=interactive(), posControls,
negControls, legend=FALSE, main="")
}

\arguments{
  
  \item{x}{a configured \code{\linkS4class{cellHTS}} object
    (i.e. \code{state(x)['configured']} must be \code{TRUE}).}
  
  \item{verbose}{a logical value, if \code{TRUE}, the function reports
    some of its intermediate progress. The default is
    \code{\link[base:interactive]{interactive()}}.}
  
  \item{posControls}{a list or vector of regular expressions specifying
    the name of the positive controls. See details.}
  
  \item{negControls}{a vector of regular expressions specifying the name
    of the negative controls. See details.}

  \item{legend}{logical defining whether to include a legend.}

  \item{main}{character giving a figure caption.}
  
}

\details{
This function calls the function \code{\link[splots]{plotScreen}} to create a screen plot showing the plate configuration (as defined by the plate configuration file used to configure the \code{cellHTS} object -- see function \code{\link{configure}}) used for the RNAi experiment stored in \code{x}.

  \code{posControls} and \code{negControls} should be given as vectors of regular expression patterns specifying the name(s) of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file 
(and acccessed via \code{wellAnno(x)}). 

  By default, if \code{posControls} is not given, "pos" will be taken as the annotation name for the wells containing positive controls. Similarly, if \code{negControls} is missing, by default "neg" will be considered as the name used to annotate the negative controls. 
  The content of \code{posControls} and \code{negControls} will be
  passed to \code{\link[base:grep]{regexpr}} for pattern matching
  within the well annotation given in column \code{controlStatus} of the
  \code{featureData} slot of the \code{cellHTS} object.  
}

\value{

Invisibly, a vector with the color map used to display the well
annotation in the image plot. 

}

\seealso{
  \code{\link[splots]{plotScreen}},
   \code{\link[cellHTS2:writeReport]{writeReport}}
}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\examples{
    data("KcViab") 
    configurationAsScreenPlot(KcViab)
}


