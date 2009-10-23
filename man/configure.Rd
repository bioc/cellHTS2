\name{configure}
\docType{methods}
\alias{configure}
\alias{configure,cellHTS-method}
\title{Configures the plates and plate result files}
\description{
  Annotate the plates and the plate result files of a \code{\linkS4class{cellHTS}} object.  
}
\usage{
\S4method{configure}{cellHTS}(object, descripFile, confFile, logFile,
path, descFunArgs=NULL, confFunArgs=NULL, logFunArgs=NULL)
}
\arguments{
  
  \item{object}{a \code{\linkS4class{cellHTS}} object.}
  
  \item{descripFile}{the name of the screen description file (see
    details).  This argument is just passed on to the
    \code{\link{readLines}} function, so any of the valid argument types
    for \code{\link{readLines}} are valid here, too. Alternatively this
    can be a function. See details.}
  
  \item{confFile}{the name of the configuration file (see details).
    This argument is just passed on to the \code{\link{read.table}}
    function, so any of the valid argument types for
    \code{\link{read.table}} are valid here, too.  Must contain one row
    for each well and each batch. Alternatively this can be a
    function. See details.}
  
  \item{logFile}{optional; the name of the screen log file (see
    details).  This argument is just passed on to the
    \code{\link{read.table}} function, so any of the valid argument
    types for \code{\link{read.table}} are valid here,
    too. Alternatively this can be a function. See details.}
  
  \item{path}{optional; a character of length one indicating the path in
    which to find the configuration files. It can be useful when the
    files are located in the same directory, and may be omitted
    otherwise.}

  \item{descFunArgs, confFunArgs, logFunArgs}{optional; lists of
    additional arguments that can be passed on if one or more of
    \code{descripFile}, \code{confFile} or \code{logFile} are functions
    rather than file names. See details.}
  
 }

\details{
  The configuration has three components:
 
  \emph{confFile}: This file specifies where the controls are.  This
  file is expected to be a tab-delimited file with two first header rows
  giving the total number of wells and plates in the screen. The next
  rows should be in the form of a spreadsheet table with at least three
  columns named \code{Plate}, \code{Well} and \code{Content}. Columns
  \code{Plate} and \code{Well} are allowed to contain regular
  expressions.  Data from wells that are configured as \emph{empty} will
  be ignored and are set to \code{NA} in the data slot \code{xraw}.  For
  an example, and for more details, please read the accompanying
  vignette.

  \emph{logFile}: This optional file allows to flag certain measurements
  as invalid. It is expected to be a tab-delimited file with at least
  three columns, and column names \code{Plate}, \code{Well}, and
  \code{Flag}. If there are multiple samples (replicates or conditions),
  a column called \code{Sample} should also be given. If there are
  multiple channels, a column called \code{Channel} must be
  given. Further columns are allowed.
 
  \emph{descripFile}: The screen description file contains general
  information about the screen.

  Alternatively, any of the three arguments can also be a user-defined
  function returning data frames similar to those produced by
  \code{read.table} from the respective files. If \code{confFile} is a
  function, it has to return a list, where the first list item is an
  integer vector of length 2 giving the total number of plates and
  wells, and the second list item is the \code{data.frame} of the actual
  plate configuration. Additional parameters can be passed on to these
  functions via the \code{descFunArgs}, \code{confFunArgs} and
  \code{logFunArgs} arguments. This design allows for instance to
  import the necessary information directly from a data base rather than
  using flat files.


}

\value{An S4 object of class \code{\linkS4class{cellHTS}},
which is obtained by copying \code{object} and updating
the following slots:
%\itemize{
 \item{plateConf}{a data frame containing what was read from input file \code{confFile} (except the first two header rows).}
 \item{screenLog}{a data frame containing what was read from input file \code{logFile}.}
 \item{screenDesc}{object of class \code{character} containing what was read from input file \code{descripFile}.}
 \item{state}{the processing status of the \code{\linkS4class{cellHTS}} object is updated in to \code{state["configured"]=TRUE}.}
 \item{featureData}{the column \code{controlStatus} is updated taking into account the well annotation given by the plate configuration file.}
 \item{experimentData}{an object of class
   \code{\link[Biobase:class.MIAME]{MIAME}} containing descriptions of
   the 
   experiment, constructed from the screen description file.}
%}
}

\seealso{
  \code{\link[cellHTS2:readPlateList]{readPlateList}}
  \code{\link[cellHTS2:templateDescriptionFile]{templateDescriptionFile}}
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

