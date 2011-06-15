\name{writeReport}
\alias{writeReport}
\title{Create a directory with HTML pages of linked tables and plots
  documenting the contents of a cellHTS experiment}
\description{
  Creates a directory with HTML pages of linked tables and plots
  documenting the contents of the preprocessing of a \code{\linkS4class{cellHTS}} object.}
\usage{
writeReport(
  raw,
  normalized=NULL,
  scored=NULL,
  cellHTSlist=NULL,
  outdir,
  force=FALSE,
  map=FALSE,
  plotPlateArgs=NULL,
  imageScreenArgs=NULL,
  posControls,
  negControls,
  mainScriptFile=NA,
  gseaModule=NULL,
  settings=list())
}

\arguments{
  
  \item{raw}{the intitial raw \code{\linkS4class{cellHTS}} object. See
    details.}

   \item{normalized}{a normalized \code{\linkS4class{cellHTS}}
     object. See details.}

   \item{scored}{a scored \code{\linkS4class{cellHTS}} object. See
     details.}
  
  \item{cellHTSlist}{a list of \code{\linkS4class{cellHTS}} objects. See
    details. Note: this argument is deprecated. Please use the separate
    arguments \code{raw}, \code{normalized} and \code{scored} instead. }
  
  \item{outdir}{a character of length 1 with the name of a directory
    where to write the report HTML file and images. If the directory
    does not exist, it is created. If it exists and is not empty, then
    the behaviour depends on the value of \code{force}.  If
    \code{outdir} is missing, it is set to \code{file.path(getwd(),
    name(cellHTSlist[['xraw']]))}, i.e. a directory with the name of the
    cellHTS object(s) in the current working path.}
  
  \item{force}{a logical value, determines the behaviour of the function
    if \code{outdir} exists and is not empty.  If \code{force} is
    \code{TRUE}, the function overwrites (removes and recreates)
    \code{outdir}, otherwise it casts an error.}
  
  \item{map}{a logical value indicating whether tooltips with the
    annotation should be added to the plate plots and image
    screen. Default value is FALSE. NOTE: This argument is deprecated
    and will go away in the next release. Please see
    \code{\link{settings}} to learn how to control the output of
    \code{writeReport}. }

  \item{plotPlateArgs}{either a list with parameters for the plate plots
    of the per plate quality report pages, or a logical scalar with
    values \code{FALSE} or \code{TRUE}.  If \code{FALSE} or \code{NULL},
    the plate plots are omitted, this option is here because the
    production of the plate plots takes a long time.  See details. NOTE:
    This argument is deprecated and will go away in the next
    release. Please see \code{\link{settings}} to learn how to control
    the output of \code{writeReport}.}
  
  \item{imageScreenArgs}{a list with parameters for the function
    \code{\link[cellHTS2]{imageScreen}}. See details. NOTE: This argument is
    deprecated and will go away in the next release. Please see
    \code{\link{settings}} to learn how to control the output of
    \code{writeReport}.}
  
  \item{posControls}{a list or vector of regular expressions specifying
    the name of the positive controls. See details.}
  
  \item{negControls}{a vector of regular expressions specifying the name
    of the negative controls. See details.}

  \item{mainScriptFile}{ The path to the R script generating the current
    report. We strongly advice to store this script in the compendium
    for future reference.}

  \item{gseaModule}{Add the output of a gene set enrichment analysis to
    the report. This is totally experimental at this time.}

  \item{settings}{A named list of settings controlling the output of
    \code{writeReport}. Please see \code{\link{settings}} for details.}
  
}

\details{

The function has to be called with the mandatory argument \code{raw}
corresponding to an unnormalized \code{\linkS4class{cellHTS}} object
(i.e. \code{state(cellHTSlist[["raw"]])["normalized"]=FALSE}). Additional
optional arguments are:


\itemize{
  
  \item \code{"normalized"}: a \code{cellHTS} object containing
  normalized data
  (i.e. \code{state(cellHTSlist[["normalized"]])["normalized"]=TRUE} and
  \code{state(cellHTSlist[["normalized"]])["scored"]=FALSE}).

  \item \code{"scored"}: a \code{cellHTS} object containing data scored
  data (i.e. \code{state(cellHTSlist[["scored"]])["scored"]=TRUE}). If
  this component is available, then \code{cellHTSlist[["normalized"]]}
  should also be given.

}

All of the above arguments have to be \code{cellHTS} objects containing
data from the same experiment, but in different preprocessing stages.

The \code{cellHTS} argument is deprecated and should no be used anymore.



The following elements are recognized for \code{plotPlateArgs} and
passed on to \code{\link[prada]{plotPlate}}: \code{sdcol}, the color
scheme for the standard deviation plate plot, \code{sdrange}, the sd
range to which the colors are mapped, \code{xcol}, the color scheme for
the intensity plate plot, \code{xrange}, the intensity range to which
the colors are mapped.  If an element is not specified, default values
are used. Both \code{sdrange} and \code{xrange} can also be provided as
functions, which take the values to be plotted by \code{platePlot} as a
single argument and has to return a numeric vector of length 2. See its
documentation for details.

The following elements are recognized for \code{imageScreenArgs} and
passed on to \code{\link[cellHTS2]{imageScreen}}: \code{ar}, aspect
ratio, \code{zrange}, range, \code{anno}, gene annotation for the image
map (if \code{map=TRUE}).

From now on, all settings controlling the output of \code{writeReport}
should either be provided through the \code{settings} argument, or as
session-wide parameters set using \code{\link{setSettings}}. Please see
\code{\link{settings}} for details.

\code{posControls} and \code{negControls} should be given as a vector of
regular expression patterns specifying the name of the positive(s) and
negative(s) controls, respectivey, as provided in the plate
configuration file (and acccessed via \code{wellAnno(objects)}).

If the \code{cellHTS} object containing normalized data was provided as argument
\code{norm}, the length of \code{posControls} and
\code{negControls} should be equal to the number of channels in this
\code{cellHTS} object
(\code{dim(Data(cellHTSlist[["normalized"]]))[3]}). Otherwise, the
length of these vectors should be equal to the number of channels in the
unpreprocessed \code{cellHTS} object (i.e.,
\code{dim(Data(cellHTSlist[["raw"]]))[3]}).

By default, if \code{posControls} is not given, "pos" will be taken as
the name for the wells containing positive controls. Similarly, if
\code{negControls} is missing, by default "neg" will be considered as
the name used to annotate the negative controls.  The content of
\code{posControls} and \code{negControls} will be passed to
\code{\link[base:grep]{regexpr}} for pattern matching within the well
annotation given in column \code{controlStatus} of the
\code{featureData} slot of the \code{cellHTS} object. If no controls are
available for a given channel, use \code{""} or \code{NA} for that
channel. For example, \code{posControls = c("", "(?i)^diap$")} means
that channel 1 has no positive controls, while "diap" is the positive
control for channel 2.

The arguments \code{posControls} and \code{negControls} are particularly
useful in multi-channel data since the controls might be
reporter-specific, or after normalizing multi-channel data.

In the case of a two-way assay, where two types of "positive" controls
are used in the screen ("activators" and "inhibitors"),
\code{posControls} should be defined as a list with two components
(called \code{act} and \code{inh}), each of which should be vectors of
regular expressions of the same length as the current number of
reporters (as explained above).

By default, tooltips doing the mapping between the probe annotation and
the plate wells are not added to the plate plots and to the overall
screen plot. If any of the \code{cellHTS} objects in \code{cellHTSlist}
is annotated, the probe annotation uses the information contained
either in column \code{GeneSymbol} or column \code{GeneID} (if the
former is missing) of the \code{featureData} slot of the annotated
\code{cellHTS} object. Otherwise, the mapping simply uses the well
identifiers.  } \seealso{ \code{\link[prada:plotPlate]{plotPlate}},
\code{\link[cellHTS2:imageScreen]{imageScreen}} }

\value{

  The function is called for its side-effect.
  It returns a character with the full path and name of the report index
  file, this is an HTML file which can be read by a web browser.
  
}

\author{

  Florian Hahne \email{florian.hahne@novartis.com}, Ligia P. Bras
  \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk},
  Gabor Bakos

}

\references{
  
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi
screens, \emph{Genome Biology} \bold{7}, R66.

}


\examples{
    data(KcViabSmall)
    pCtrls <- c("pos") 
    nCtrls <- c("neg") 
\dontrun{
    ## or for safety reasons (not a problem for the current well annotation, however) 
     pCtrls <- c("^pos$") 
     nCtrls <- c("^neg$")
    writeReport(raw=KcViabSmall, posControls=pCtrls, negControls=nCtrls)
    ## same as 
    ## writeReport(raw=KcViabSmall)
    xn <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median")
    xsc <- scoreReplicates(xn, sign="-", method="zscore")
    xsc <- summarizeReplicates(xsc, summary="min")
    ## to turn on the tooltips in the plate plots and in the image screen plot:
    writeReport(raw=KcViabSmall, normalized=xn, scored=xsc, force=TRUE, map=TRUE, plotPlateArgs = TRUE, imageScreenArgs=list(zrange=c(-4,4)))
    }
}

\keyword{manip}
