\name{configure}
\docType{methods}
\alias{configure}
\alias{configure,cellHTS-method}
\title{Configures the plates and plate result files}
\description{
  Annotate the plates and the plate result files of a given \code{\linkS4class{cellHTS}} object.  
}
\usage{
\S4method{configure}{cellHTS}(object, confFile, logFile, descripFile, path, \dots)
}
\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object.}
  \item{confFile}{the name of the configuration file (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too. 
    Must contain one row for each well and each batch.}
  \item{logFile}{optional; the name of the screen log file (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument
    types for \code{\link{read.table}} are valid here, too.}
  \item{descripFile}{the name of the screen description file (see details).
    This argument is just passed on to the \code{\link{readLines}}
    function, so any of the valid argument
    types for \code{\link{readLines}} are valid here, too.}
  \item{path}{optional; a character of length one indicating the path in
    which to find the configuration files. Useful when the files are locate in the same directory, 
    but should be omitted otherwise.}
  \item{\dots}{additional parameters - ignored.}
}

\details{
  \item{confFile}{This file is expected to be a tab-delimited file with
at least three columns, and column names \code{Batch}, \code{Well} and \code{Content}. 
The contents of \code{Batch} are expected to be integer.}
  \item{logFile}{If given as an argument, it is expected to be a tab-delimited file with
at least three columns, and column names \code{Filename}, \code{Well},
and \code{Flag}. Further columns are allowed.}
  \item{descripFile}{This file is the screen description file with
general information about the screen.}
  
Data from wells that are annotated as \emph{empty} are ignored and are set to NA in \code{object} in slot \code{xraw}.  
}

\value{
 An S4 object of class \code{\linkS4class{cellHTS}}, which extends the argument \code{object} by the following elements:
 \item{plateConf}{a \code{\link{data.frame}} containing what was read from input file \code{confFile}. 
 The number of rows is equal to the product between the number of wells in each plate and the number of batches.}
 \item{screenLog}{a data.frame containing what was read from input file \code{logFile}.}
 \item{screenDesc}{object of class \code{character} containing what was read from input file \code{descripFile}.}
Moreover, the processing status of the \code{\linkS4class{cellHTS}} object is updated in the slot \code{state} 
to \code{state["configured"]=TRUE}.
 \item{wellAnno}{object of class \code{factor} of length number of plates x number of wells per plate, with possible levels: \emph{empty}, \emph{other}, \emph{neg}, \emph{sample}, and \emph{pos}, indicative of the contents of the wells. Other levels may be employed for the positive and negative controls, besides \emph{pos} and \emph{neg}.}

There are methods \code{show}, \code{configure}, \code{annotate}, \code{writeTab} and \code{scores}.
}

\seealso{
  \code{\link[cellHTS2:readPlateData]{readPlateData}}
}

\author{Wolfgang Huber \email{huber@ebi.ac.uk}, Ligia Braz \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\examples{

    datadir <- system.file("KcViabSmall", package = "cellHTS2")
    x <- readPlateData("Platelist.txt", "KcViabSmall", path=datadir)
    x <- configure(x, "Plateconf.txt", "Screenlog.txt", "Description.txt", path=datadir)
}

\keyword{methods}
