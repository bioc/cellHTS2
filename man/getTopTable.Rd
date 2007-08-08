\name{getTopTable}
\alias{getTopTable}
%\alias{getTopTable,cellHTS-method}

\title{Generate the hit list from a scored cellHTS object}
 
\description{
  Generate the hit list from a scored {\linkS4class{cellHTS}} object and write it to a tab-delimited file.
}


\usage{
getTopTable(x, file="topTable.txt", verbose=interactive())
}

\arguments{
  \item{x}{a scored \code{\linkS4class{cellHTS}} object.}
  \item{file}{the name of the output file. Default is "topTable.txt"}
  \item{verbose}{a logical value, if \code{TRUE}, the function reports its progress. Defaults to the state of \code{\link[base:interactive]{interactive()}}.}
}

\details{
Given a scored \code{\linkS4class{cellHTS}} object, \code{x}, this function generates a \code{data.frame} that is written to \code{file}.
This data.frame and the output file contain the list of scored probes 
ordered by decreasing score values (\code{x@score}). They have one row for each well and plate, 
and contain the following columns:

\itemize{

\item \code{plate}

\item \code{position}: Gives the position of the well in the plate (runs from 1 to the total 
number of wells in the plate).

\item \code{score}: Content of \code{x@score}.

\item \code{wellAnno}: Well annotation as given by the plate configuration file.

\item \code{normalized_ri_chj}: Gives giving the normalized intensities for replicate \code{i} in channel \code{j}. 
 This corresponds to the content of \code{x@xnorm}.

\item \code{xrawAnno_ri_chj}: Gives the final well annotation for replicate \code{i} in channel \code{j}. 
    It combines the information given in the plate configuration file with the values in \code{x@xraw}, in order to have 
into account the wells that have been flagged either by the screen log file, or manually by the user 
during the analysis. These flagged wells appear with the annotation \emph{flagged}.

\item \code{raw_ri_chj}: Contains the raw intensities for replicate \code{i} in channel \code{j} (content of \code{x@xraw}).

\item \code{median_chj}: Corresponds to the median of raw measurements across replicates in channel \code{j}.

\item \code{diff_chj}: Gives the difference between replicate raw measurements in channel \code{j} (only available when the number of replicates is equal to two).

\item \code{average_chj}: Corresponds to the average between replicate raw intensities for channel \code{j}
(only given if the number of replicates is higher than two).

\item \code{raw/PlateMedian_ri_chj}: Gives the ratio between each raw measurement and the median intensity in each plate for replicate \code{i} in channel \code{j}. The plate median is determined for the raw intensities using exclusively the wells annotated as \code{sample}.
} 

Additionally, if \code{x} is already annotated, the topTable also contains the data given in 
the original gene anotation file that was stored in \code{x@geneAnno}.
}

\value{
Generates the file with the hit list and outputs a \code{\link{data.frame}} with the same contents.
}

\seealso{\code{\linkS4class{cellHTS}}}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    KcViabSmall <- normalizePlates(KcViabSmall, scale="additive", log=FALSE, method="median", varianceAdjust="byExperiment") 
    KcViabSmall <- summarizeReplicates(KcViabSmall, zscore="-", summary="mean")
    out <- getTopTable(KcViabSmall, file=tempfile())
}
\keyword{manip}
