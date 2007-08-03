\name{getReplicatesMatrix}
\alias{getReplicatesMatrix}

\title{Create a matrix with replicate data in columns}
\description{
  Given an array of raw or normalized intensities (\code{rawdata(object)} or
  \code{normdata(object)}) from an object of class \code{\linkS4class{cellHTS}}, this function creates 
  a matrix with the data from the chosen channel.
  }
\usage{
getReplicatesMatrix(y, channel=1, na.rm=FALSE)
}
\arguments{
  \item{y}{an array with four dimensions, such as the slot \code{xraw} or
    \code{xnorm} of a \code{\linkS4class{cellHTS}} class object.}
  \item{channel}{a numeric value corresponding to the selected channel
    of \code{y}. By default, the first channel (that is, \code{y[,,,1]} is considered).}
  \item{na.rm}{Logical, indicated if the missing values should be omitted.}
  }

\details{

 Given as input an array \code{y} (e.g., the slot \code{xraw}, or \code{xnorm} of a
 \code{\linkS4class{cellHTS}} object) with dimensions \code{nr wells x nr plates x
 nr replicates x nr channels}, this function creates a matrix with the data for
 the chosen \code{channel}. Each replicate corresponds to a column of
 the output matrix.
 If \code{na.rm} is set to \code{TRUE}, only the positions with
 available values for all the replicates are given in the
 output matrix.}

\value{
  A matrix with the same number of columns as the number of replicates (\code{dim(y)[4]}).
  If \code{na.rm=FALSE} (the default), the number of rows of the output matrix
 is identical to the product between the first two
 dimensions of \code{y} (\code{nr wells x nr plates}). If \code{na.rm=TRUE}, only the
   rows with no missing entries in all of the replicates (columns) are
   given.}

\seealso{
  \code{\link[cellHTS2:rawdata]{rawdata}},
  \code{\link[cellHTS2:rawdata]{normdata}},
}


\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    y <- getReplicatesMatrix(rawdata(KcViabSmall))
}
\keyword{manip}
