\name{writeReport}
\alias{writeReport}
\title{Create a directory with HTML pages of linked tables and plots
  documenting the contents of a  cellHTS object}
\description{
  Creates a directory with HTML pages of linked tables and plots
  documenting the contents of a \code{\linkS4class{cellHTS}} object.}
\usage{
writeReport(x,
  outdir=file.path(getwd(), name(x)),
  force=FALSE,
  plotPlateArgs=FALSE,
  imageScreenArgs=NULL,
  progressReport = interactive(),
  posControls,
  negControls)
}

\arguments{
  \item{x}{a \code{\linkS4class{cellHTS}} object.}
  \item{outdir}{a character of length 1 with the name of a directory where
    to write the report HTML file and images. If the directory does not
    exist, it is created. If it exists and is not empty, then the
    behaviour depends on the value of \code{force}.}
  \item{force}{a logical value, determines the behaviour of the function
    if \code{outdir} exists and is not empty.
    If \code{force} is \code{TRUE}, the function overwrites (removes and recreates)
    \code{outdir}, otherwise it casts an error.}
  \item{plotPlateArgs}{either a list with parameters for the plate plots
    of the per plate quality report pages, or a logical scalar with values
    \code{FALSE} or \code{TRUE}.
    If \code{FALSE}, the plate plots are omitted, this option is
    here because the production of the plate plots takes a long time.
    See details.}
  \item{imageScreenArgs}{a list with parameters for the function
    \code{\link[cellhts2]{imageScreen}}. See details.}
  \item{progressReport}{a logical, should a progress report window be displayed?}
  \item{posControls}{a list or vector of regular expressions specifying the name of the positive controls. See details.}
  \item{negControls}{a vector of regular expressions specifying the name of the negative controls. See details.}
}

\details{
   The following elements are recognized for \code{plotPlateArgs} and
   passed on to \code{\link[prada]{plotPlate}}:
    \code{sdcol}, the color scheme for the standard deviation plate plot,
    \code{sdrange}, the sd range to which the colors are mapped,
    \code{xcol}, the color scheme for the intensity plate plot,
    \code{xrange}, the intensity range to which the colors are mapped.
   If an element is not specified, default values are used.

   The following elements are recognized for \code{imageScreenArgs} and
   passed on to \code{\link[cellhts2]{imageScreen}}:
   \code{ar}, aspect ratio,
   \code{zrange}, range,
   \code{map}, logical value indicating whether tooltips with the annotation should be added to the plot (default value is FALSE),
   \code{anno}, gene annotation for the image map.

  \code{posControls} and \code{negControls} should be given as a vector of regular expression patterns specifying the name of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file 
(and stored in \code{x@wellAnno}). The length of these vectors should be equal to the number of reporters used in the screen (\code{dim(x@xraw)[4]} or to \code{dim(x@xnorm)[4]}, in case \code{x} contains multi-channel data that has been normalized by combining the values from two or more channels).
  By default, if \code{posControls} is not given, "pos" will be taken as the name for the wells containing positive controls. Similarly, if \code{negControls} is missing, by default "neg" will be considered as the name used to annotated the negative controls. 
  The content of \code{posControls} and \code{negControls} will be
  passed to \code{\link[base:regexpr]{regexpr}} for pattern matching
  within the well annotation given in \code{x@wellAnno} (see
  examples). If no controls are available for a given channel, use
  \code{""} or \code{NA} for that channel. For example,
  \code{posControls = c("", "(?i)^diap$")} means that channel 1 has no
  positive controls, while "diap" is the positive control for channel 2.

  The arguments \code{posControls} and \code{negControls} are particularly useful in multi-channel data since the controls might be reporter-specific, or after normalizing multi-channel data.

In case of a two-way assay, where two types of "positive" controls are used in the screen ("activators" and "inhibitors"), \code{posControls} should be defined as a list with two components (called \code{act} and \code{inh}), each of which should be vectors of regular expressions of the same length as the current number of reporters (as explained above).  

By default, tooltips doing the mapping between the probe annotation and the plate wells are not added to the plate plots and to the overall screen plot. If the cellHTS object \code{x} is annotated, the probe annotation used the information contained whether in \code{x@geneAnno$GeneSymbol}, or \code{x@geneAnno$GeneID}, if the former is missing. Otherwise, the mapping simply uses the well identifiers.
}
\seealso{
  \code{\link[prada:plotPlate]{plotPlate}},
  \code{\link[cellhts2:imageScreen]{imageScreen}}
}

\value{
  The function is called for its side-effect.
  It returns a character with the full path and name of the report index
  file, this is an HTML file which can be read by a web browser.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    ## pCtrls <- c("pos") 
    ## nCtrls <- c("neg") 
    ## or for safety reasons (not a problem for the current well annotation, however) 
    ## pCtrls <- c("^pos$") 
    ## nCtrls <- c("^neg$")
    ## writeReport(KcViabSmall, posControls=pCtrls, negControls=nCtrls)
    ## same as 
    ## writeReport(KcViabSmall)
    \dontrun{
    x <- normalizePlates(KcViabSmall, scale="additive", log=FALSE, method="median", varianceAdjust="byExperiment")
    x <- summarizeReplicates(x, zscore="-", summary="min")
    writeReport(x, force=TRUE, plotPlateArgs = list(), imageScreenArgs=list(zrange=c(-4,4)))
    }
    ## to turn on the tooltips in the overall screen image plot:
    ## writeReport(x, force=TRUE, plotPlateArgs = list(), imageScreenArgs = list(zrange=c(-4,4), map=TRUE))
}

\keyword{manip}
