\name{getTopTable}
\alias{getTopTable}


\title{Generate the hit list from a scored cellHTS object}
 
\description{
  
  Generate the hit list from a scored \code{\linkS4class{cellHTS}}
  object and write it to a tab-delimited file.
  
}


\usage{

getTopTable(cellHTSlist, file="topTable.txt", verbose=interactive())

}

\arguments{
  
  \item{cellHTSlist}{a list of \code{\linkS4class{cellHTS}} objects. See
    details.}
  
  \item{file}{the name of the output file. Default is "topTable.txt".}
  
  \item{verbose}{a logical value, if \code{TRUE}, the function reports
  its progress. Defaults to the state of
  \code{\link[base:interactive]{interactive()}}.}

}

\details{
  
  Argument \code{cellHTSlist} should be a list containing at least one
  component named "scored" which corresponds to a scored
  \code{\linkS4class{cellHTS}} object. Other possible components of
  \code{cellHTSlist} can be:
  
  \describe{
    
    \item{\code{"raw"}:}{ a \code{cellHTS} object containing unpreprocessed
    data. I.e. \code{state(cellHTSlist[["raw"]])["normalized"]=FALSE}.}
  
    \item{\code{"normalized"}:}{a \code{cellHTS} object containing
    normalized
    data. I.e. \code{state(cellHTSlist[["normalized"]])["normalized"]=TRUE}
    and \code{state(cellHTSlist[["normalized"]])["scored"]=FALSE}.}
  
  }
  
  All of the components of \code{cellHTSlist} should be \code{cellHTS}
  objects containing data from the same experiment, but in different
  preprocessing stages.

  This function generates a \code{data.frame} that is written to
  \code{file}.  This data.frame and the output file contain the list of
  scored probes ordered by decreasing score values. They have one row
  for each well and plate, and contain the following columns (depending
  on the components of \code{cellHTSlist}):

  \describe{

    \item{\code{plate}:}{ plate identifier for each well.}

    \item{\code{position}:}{ gives the position of the well in the plate
    (ranges from 1 to the total number of wells in the plate).}

    \item{\code{well}:}{ gives the alphanumeric identifier for the wells.}
    
    \item{\code{score}:}{ content of slot \code{assayData} of the scored
    \code{\linkS4class{cellHTS}} object given in
    \code{cellHTSlist[["scored"]]}.}

    \item{code{wellAnno}:}{ell annotation as given by the plate
    configuration file.}

    \item{code{finalWellAnno}:}{gives the final well annotation for the
    scored values.  It combines the information given in the plate
    configuration file with the values in \code{assayData} slot of the
    scored \code{cellHTS} object, in order to have into account the
    wells that have been flagged either by the screen log file, or
    manually by the user during the analysis. These flagged wells appear
    with the annotation \emph{flagged}.}

    \item{\code{raw_ri_chj}:}{ (if \code{cellHTSlist[["raw"]]} is given)
    contains the raw intensities for replicate \code{i} in channel
    \code{j} (content of slot \code{assayData} of the \code{cellHTS}
    object given in \code{cellHTSlist[["raw"]]}).}

    \item{\code{median_chj}:}{ (if \code{cellHTSlist[["raw"]]} is given)
    corresponds to the median of raw measurements across replicates in
    channel \code{j}.}

    \item{\code{diff_chj}:}{ (if \code{cellHTSlist[["raw"]]} is given and
    if there are two replicates or samples) gives the difference between
    replicate (sample) raw measurements in channel \code{j}.}

    \item{\code{average_chj}:}{ (if \code{cellHTSlist[["raw"]]} is given
    and if there are more than 2 replicates or samples) corresponds to
    the average between replicate raw intensities for channel \code{j}.}

    \item{\code{raw/PlateMedian_ri_chj}:}{ (if \code{cellHTSlist[["raw"]]}
    is given) this column gives the ratio between each raw measurement
    and the median intensity in each plate for replicate (or sample)
    \code{i} in channel \code{j}. The plate median is determined for the
    raw intensities using exclusively the wells annotated as
    \code{sample}.}

    \item{\code{normalized_ri_chj}:}{ (if
    \code{cellHTSlist[["normalized"]]} is given) gives the normalized
    intensities for replicate (sample) \code{i} in channel
    \code{j}. This corresponds to the content of slot \code{assayData}
    of the \code{cellHTS} object given in
    \code{cellHTSlist[["normalized"]]}.}

  } 
  
  Additionally, if the \code{cellHTS} object given in
  \code{cellHTSlist[["scored"]]} is already annotated, the output
  \code{topTable} also contains the gene annotation stored in slot
  \code{featureData}.

}

\value{
  
Generates the file with the hit list and outputs a
\code{\link{data.frame}} with the same contents.

}

\seealso{\code{\linkS4class{cellHTS}}}

\references{
  
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi
screens, \emph{Genome Biology} \bold{7}, R66.

}


\author{Ligia P. Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    xn <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="none") 
    xsc <- scoreReplicates(xn, sign="-", method="zscore")
    xsc <- summarizeReplicates(xsc, summary="mean")
    out <- getTopTable(cellHTSlist=list("raw"=KcViabSmall, "normalized"=xn, "scored"=xsc), file=tempfile())
}
\keyword{manip}
