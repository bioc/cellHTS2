\name{cellHTS-class}
\docType{class}
\alias{cellHTS-class}

\alias{show,cellHTS-method}

\title{
  Class to contain cell-based high-throughput RNAi data
}

\description{

  Container for cell-based high-throughput RNA interference assays performed in plate format.
}

\section{Creating Objects}{
  \code{new("cellHTS")}
}


\section{Slots}{
 \describe{
  \item{\code{name}:}{a character of length 1 with the experiment name.}
  \item{\code{pdim}:}{a numeric vector of length 2 containg the number of rows
    and columns in a plate, and named "nrow" and "ncol". The product of these two numbers is the
    first dimension of \code{xraw}.}
  \item{\code{batch}:}{an integer vector with the batch number (1, 2, ...)
    for each plate. Its length corresponds to the second dimension of 
    of \code{xraw}.}
  \item{\code{xraw}:}{a 4D array of size plateSize x number of plates x
    number of replicates x number of channels, containing imported
    measurement data.}
  \item{\code{xnorm}:}{a 4D array containing the normalized data.}
  \item{\code{plateList}:}{a \code{\link{data.frame}} containing what was read from input measurement data files
    plus a column \code{status} of type character containing the string "OK" if the data import
    appeared to have gone well, and the respective error or warning
    message otherwise.}
  \item{\code{intensityFiles}:}{a list, where each component contains a
    copy of the imported input data files. Its length corresponds to the
    number of rows of \code{plateList}.}
  \item{\code{state}:}{a logical vector of length 4 representing the processing status of
    the object. Should have the names "configured", "normalized", "scored" and "annotated".}
  \item{\code{plateConf}:}{a \code{\link{data.frame}} containing what was read from the \emph{configuration file} for the experiment. It contains at least three columns, and column names \code{Batch}, \code{Well} and \code{Content}. The contents of \code{Batch} are expected to be integer. The number of rows is equal to the product between the number of wells in each plate and the number of batches.}
  \item{\code{screenLog}:}{a \code{\link{data.frame}} containing what was read from the \emph{screen log file} for the experiment, in case it exists. Contains at least three columns, and column names \code{Filename}, \code{Well},
and \code{Flag}.}
  \item{\code{screenDesc}:}{a character containing what was read from the \emph{description file} of the experiment.}
  \item{\code{wellAnno}:}{a \code{\link{factor}} of length equal to number of plates x number of wells per plate, with possible levels: \emph{empty}, \emph{other}, \emph{neg}, \emph{sample}, and \emph{pos}, indicative of the contents of the wells. Other levels may be employed for the positive and negative controls, besides \emph{pos} and \emph{neg}.}
  \item{\code{geneAnno}:}{a \code{\link{data.frame}} containing gene annotation for the screen. It contains at least three columns, and column names \code{Plate}, \code{Well} and \code{GeneID}, where the contents of \code{Plate} are integer. The number of rows is equal to the product between the number of wells in each plate and the number of plates.}
 \item{\code{score}:}{a numeric vector containing the scored values for each well in
  every plate. Its length is to the product
  between the plateSize and the number of plates.}
 \item{\code{rowcol.effects}:}{a 4D array of size plateSize x number of plates x
    number of replicates x number of channels containing estimated row and column offsets.}
 \item{\code{overall.effects}:}{a 4D array of size 1 x number of plates x number of replicates x number of channels containing estimated plate overall offsets.}
  }
}

\section{Methods}{
  \describe{
%    \item{\code{[}}{Subset}
%    \item{\code{dim}}{Access the dimensions of data matrix.}
%    \item{\code{nrow}}{Get number of rows of data matrix.}
%    \item{\code{ncol}}{Get number of columns of data matrix.}
    \item{\code{show}}{Print a summary of the object.}
    \item{\code{state}}{Access the \code{state} slot of a \code{\linkS4class{cellHTS}} instance.}
    \item{\code{scores(object), scores(object) <- value}:}{
       \code{signature(object = "cellHTS", value = "numeric")}: Access
       and replace the \code{score} slot of a \code{\linkS4class{cellHTS}} instance. 
       \code{scores} returns a numeric vector of length Number of wells per plate x Number of plates.}
    \item{\code{rawdata(object), rawdata(object) <- value}:}{
       \code{signature(object = "cellHTS", value = "array")}: Access
       and replace the \code{xraw} slot of a \code{\linkS4class{cellHTS}} instance. 
       \code{rawdata} returns a 4D array of dimension Number of wells per plate x Number of plates x Number of replicates x Number of channels.}
    \item{\code{normdata(object), normdata(object) <- value}:}{
       \code{signature(object = "cellHTS", value = "array")}: Access
       and replace the \code{xnorm} slot of a \code{\linkS4class{cellHTS}} instance. 
       \code{normdata} returns a 4D array of dimension Number of wells per plate x Number of plates x Number of replicates x Number of channels.}
    \item{\code{annotate}}{Annotate the \code{\linkS4class{cellHTS}} object using the \emph{screen annotation file}.}
    \item{\code{configure}}{Configure the \code{\linkS4class{cellHTS}} object using the \emph{screen configuration file}, the \emph{screen log file} and the \emph{screen description file}.} 
    \item{\code{writeTab}}{Write the data from a \code{\linkS4class{cellHTS}} object to a tab-delimited file.}
%\item{\code{normalizePlates}}{Plate-by-plate correction of raw data from a \code{\linkS4class{cellHTS}} object.}
\item{\code{ROC}}{Construct an object of S4 class \code{\linkS4class{ROC}},
  which represents a receiver-operator-characteristic curve,
  from the data of the annotated positive and negative controls in a
  scored \code{\linkS4class{cellHTS}} object.}
  }
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\seealso{
  \code{\link[cellHTS2:readPlateData]{reaPlateData}}
  \code{\link[cellHTS2:annotate]{annotate}}
  \code{\link[cellHTS2:annotate]{configure}}
  \code{\link[cellHTS2:writeTab]{writeTab}}
  \code{\link[cellHTS2:description]{description}}
  \code{\link[cellHTS2:scores]{scores}}
  \code{\link[cellHTS2:state]{state}}
  \code{\link[cellHTS2:rawdata]{rawdata}}
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}}
  \code{\link[cellHTS2:ROC]{ROC}}

}

\examples{
    showClass("cellHTS")
    showMethods(class="cellHTS")
  
    data("KcViabSmall")
    show(KcViabSmall)
    obj <- new("cellHTS")
}

\keyword{classes}
