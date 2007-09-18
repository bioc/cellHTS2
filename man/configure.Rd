\name{configure}
\docType{methods}
\alias{configure}
\alias{configure,cellHTS-method}
\title{Configures the plates and plate result files}
\description{
  Annotate the plates and the plate result files of a given \code{\linkS4class{cellHTS}} object.  
}
\usage{
\S4method{configure}{cellHTS}(object, descripFile, confFile, logFile, path)
}
\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object.}
  \item{descripFile}{the name of the screen description file (see details).
    This argument is just passed on to the \code{\link{readLines}}
    function, so any of the valid argument
    types for \code{\link{readLines}} are valid here, too.}
  \item{confFile}{the name of the configuration file (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too. 
    Must contain one row for each well and each batch.}
  \item{logFile}{optional; the name of the screen log file (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too.}
  \item{path}{optional; a character of length one indicating the path in
    which to find the configuration files. Useful when the files are locate in the same directory, 
    but should be omitted otherwise.}
 }

\details{
  \item{confFile}{This file is expected to be a tab-delimited file with two first header rows giving the total number of wells and plates in the screen. The next rows should be in the form of a data.frame with at least three columns named \code{Plate}, \code{Well} and \code{Content}. Columns \code{Plate} and \code{Well} are allowed to contain regular expressions.
For more details about the plate configuration file, read the accompanying vignette.
}
  \item{logFile}{If given as an argument, it is expected to be a tab-delimited file with
at least three columns, and column names \code{Plate}, \code{Well},
and \code{Flag}. If there are multiple samples (replicates or conditions), a column called \code{Sample} should also be given. If there are multiple channels, a column called \code{Channel} must be given. Further columns are allowed.
}

  \item{descripFile}{This file is the screen description file with
general information about the screen.}
  
Data from wells that are annotated as \emph{empty} are ignored and are set to NA in \code{object} in slot \code{xraw}.  
}

\value{
 An S4 object of class \code{\linkS4class{cellHTS}}, which extends the class \code{\link[Biobase:class.NChannelSet]{NChannelSet}}. The followings slots of \code{object} are updated:
 \item{plateConf}{a \code{\link{data.frame}} containing what was read from input file \code{confFile} (except the first two header rows.}
 \item{screenLog}{a data.frame containing what was read from input file \code{logFile}.}
 \item{screenDesc}{object of class \code{character} containing what was read from input file \code{descripFile}.}
 \item{state}{the processing status of the \code{\linkS4class{cellHTS}} object is updated in to \code{state["configured"]=TRUE}.}
 \item{featureData}{the column \code{controlStatus} is updated having into account the well annotation given by the plate configuration file.}
\item{experimentData}{an object of class \code{\link[Biobase]{MIAME}} containing descriptions of the
      experiment, constructed from the screen description file.}
}

\seealso{
  \code{\link[cellHTS2:readPlateList]{readPlateList}}
}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Bras \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\examples{

    datadir <- system.file("KcViabSmall", package = "cellHTS2")
    x <- readPlateList("Platelist.txt", name="KcViabSmall", path=datadir)
    x <- configure(x, "Description.txt", "Plateconf.txt", "Screenlog.txt", path=datadir)
}

\keyword{methods}
