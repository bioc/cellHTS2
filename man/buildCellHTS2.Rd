\name{buildCellHTS2}
\alias{buildCellHTS2}
\title{Build a cellHTS2 object from a data frame containing measurements}

\description{
  Builds a cellHTS2 object from a data frame.
}

\usage{
buildCellHTS2(xd, measurementNames)
}

\arguments{
  \item{xd}{a data frame containing the columns \code{plate},
    \code{replicate} and \code{well}, and the measurement columns.
    The \code{plate} and \code{replicate} columns must contain integer
    values, starting from 1. The \code{well} column must contain well
    names formed by one to two capitals letter followed by two digits,
    e.g. \code{A12} or \code{AB01}. Letter ordering is as follows:
    A, B, C,..., Z, AA, AB, AC,..., AZ, BA, BB, ..., ZZ.}
  
  \item{measurementNames}{an optional character vector containing
    the measurement names. If missing, the names of the measurement
    columns in \code{xd} are used. If \code{NULL}, the measurements
    are not named.}
}

\details{
  The function uses \code{readPlateList} to build a \code{cellHTS2}
  object.
}

\value{
  An object of class \code{\linkS4class{cellHTS}}, which extends the
  class \code{\link[Biobase:class.NChannelSet]{NChannelSet}}.
}

\seealso{
  \code{\link{readPlateList}}.
}

\author{
  Gregoire Pau \email{gregoire.pau@embl.de}
}

\references{
  Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi
  screens, \emph{Genome Biology} \bold{7}, R66.
}


\examples{
wells = sprintf("\%s\%02d", rep(LETTERS[1:8], each=12), 1:12)
xd = expand.grid(plate=1:3, replicate=1:2, well=wells)
xd$cell.number = rnorm(nrow(xd))
xd$cell.size = rnorm(nrow(xd))
x = buildCellHTS2(xd)
}
