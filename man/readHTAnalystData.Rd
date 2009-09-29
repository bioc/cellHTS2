\name{readHTAnalystData}
\alias{readHTAnalystData}
\title{Read a set of plate results obtained from a HTanalyser plate reader}
\description{
  Reads input files (specified by the argument \code{filenames}) containing replicate data for the same set of assay plates and obtained using an HTanalyser.  
  The number of replicates corresponds to the number of given files (\code{length(filenames)}), while the total number of plates should be given by the argument \code{nrPlates}. 
}
\usage{
readHTAnalystData(filenames, path=dirname(filenames), name, nrPlates, verbose=interactive())
}
\arguments{
  \item{filenames}{vector of characters giving the name(s) of the input file(s) obtained in a HTanalyser plate reader and containing the set of measurements for each set of replicate plates (see details).}
  \item{path}{can be either a character vector with the same length as \code{filenames} or a character of length 1 indicating the path in which to find the input file(s) \code{filenames}. By default, 
    it can extract the path from \code{filenames}.}
  \item{name}{a character of length 1 with the experiment name.}
  \item{nrPlates}{an integer value indicating the number of plates in the input file(s).}
  \item{verbose}{a logical value, if TRUE, the function reports some of its intermediate progress. Defaults to the state of \code{\link[base:interactive]{interactive()}}.}
}

\details{
This function reads input files obtained in a HTanalyser plate reader for a multi-plate format screening experiment. Data for the same set of replicate measurements of all plates are expected to come in a single input file.
So, the argument \code{filenames} should be a character vector specifying the name of the input files for each replicate.
Each of these files is expected to contain data for a total of \code{nrPlates} assay plates. It contains meta-experimental data together with plate measurements in a matrix-like format. The same type of format is expected for each of the \code{nrPlates} contained in each input file indicated in \code{filenames}.
The input files should be suitable to be used as input for \code{\link[base:readLines]{readLines}}.

The following metadata fields are expected to be repeated along the input file(s) for each assay plate:
\itemize{
 \item Microplate format, indicates the plate format used in the screen. Should be the same across plates.
 \item Barcode, this field is used to uniquely identify each assay plate.
 \item Method ID, indicates the method used to perform the assay. Should be the same for every plate.
 \item Data, indicates the type of data and should be the same for every plate.
 \item Units, indicates the units of the readings. Should be the same for every plate.
 \item Display format, indicating the numeric format used to display the data measurements. Should be the same along plates.
}

This function expects that the next line after the meta field "Display format" for a given plate contains the column numbers (\code{1:ncol}) which are then followed by the matrix of measurements in each well. Each entry of the data matrix corresponds to a position in the assay plate with coordinates \code{(row, col)}, except the first column, which gives the row ID, as upper letters (A, B, ...).

}

\value{
  An object of class \code{\linkS4class{cellHTS}}, which extends the
  class \code{\link[Biobase:class.NChannelSet]{NChannelSet}}.  
After calling this function, the content of the following slots is as follows:
	\item{assayData}{an object of class
  \code{\link[Biobase:class.AssayData]{AssayData}} containing the
  imported measurement data. Each matrix represents a single channel,
  and each sample (replicate) corresponds to a column. Thus, the total
  number of rows in each matrix corresponds to the product between the
  number of wells per plate and the number of assay plates.} 

  \item{phenoData}{the argument \code{name} is stored in its column
  \code{assay}.} 
  \item{featureData}{the information about the plate and well
  identifiers for each plate measurement are stored in columns
  \code{plate} and \code{well} of this slot.} 
  \item{intensityFiles}{a list, where each component contains a
    copy of the measurement data of a given plate, replicate and
  channel. Its length corresponds to the 
    number of rows of \code{plateList}.}
  \item{plateList}{a data.frame containing the columns "Filename",
  "Plate", "Replicate", "Channel" and "status". Each row of this slot
  makes the correspondence between a given component (name given in
  column "Filename") in the list stored in slot \code{intensityFiles}
  (i.e. plate) and its respective plate, replicate and channel
  number. Thus, this data.frame contains as many rows as the product
  between the total number of plates, replicates and channels. The last
  column named \code{status} is of type character and contains the
  string "OK" indicated the success status of the data import.} 
}

\seealso{
To read a collection of plate result files with measurements of a single
plate and replicate, see \code{\link{readPlateList}}. 
}

\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Bras \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi
screens, \emph{Genome Biology} \bold{7}, R66. 
}

\examples{
    datadir <- system.file("KcCellTiter", package = "cellHTS2")
    x <- readHTAnalystData(filenames = c("KcCellTiter0607.txt", "KcCellTiter0707.txt"), name="KcData", path=datadir, nrPlates=3L)
    x <- configure(x, confFile = "Plateconf.txt", logFile="Screenlog.txt", descripFile = "Description.txt",
           path = datadir)
}
\keyword{manip}
