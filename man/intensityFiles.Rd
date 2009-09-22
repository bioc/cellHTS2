\name{intensityFiles}
\alias{intensityFiles}
\alias{intensityFiles,cellHTS-method}
\alias{plateList}
\alias{plateList,cellHTS-method}
\alias{plateConf}
\alias{plateConf,cellHTS-method}
\alias{screenLog}
\alias{screenLog,cellHTS-method}
\alias{screenDesc}
\alias{screenDesc,cellHTS-method}

\title{Retrieve the contents of the input files used to generate a given cellHTS object.}

\description{
  These generic functions access different slots of an object derived from the
  \code{\linkS4class{cellHTS}} class, which contain the original content of the input files used to create the object.
}

\usage{
intensityFiles(object)
plateList(object)
plateConf(object)
screenLog(object)
screenDesc(object)
}

\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}}.}
}

\value{
  \code{intensityFiles} returns a list, where each component contains a
    copy of the imported input data files.

   \code{plateList} returns a data.frame containing what was read from the plate list file, 
    plus a column \code{status} of type character that contains
    the string "OK" if the data import
    appeared to have gone well, and the respective error or warning
    message otherwise. See \code{\link{readPlateList}} for more details.

   \code{plateConf} returns a data.frame that contains what was read from the plate configuration input file (except the first two header rows) during the screen configuration step. For more details see \code{\link{configure}}.

   \code{screenLog} returns a data.frame containing what was read from the screen log input file during the screen configuration step. See \code{\link{configure}} for more details.

 \code{screenDesc} returns an object of class \code{character} that contains what was read from the screen description input file during the configuration of the \code{\linkS4class{cellHTS}} object. See \code{\link{configure}} for more details.

}

\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\seealso{
\code{\linkS4class{cellHTS}}, 
\code{\link[cellHTS2]{readPlateList}}, 
\code{\link[cellHTS2]{configure}}, 
\code{\link[cellHTS2:cellHTS-class]{name}}
}

\keyword{manip}
