\name{convertWellCoordinates}
\alias{convertWellCoordinates}
\title{Converts different well identifiers}
\description{
  Converts between different ways of specifying well coordinates. 
  For example, "B02"  <-> c("B", "02") <-> 26.
}

\usage{
convertWellCoordinates(x, pdim)
}
\arguments{
  \item{x}{either a character vector with alphanumeric well identifiers (e.g. 'B03') or a vector (or matrix) of the type 
c('B', '03') ) or a vector of integers with position identifiers for a well within a plate (e.g. 27).}
\item{pdim}{a vector of length 2 with names 'nrow' and 'ncol' giving the number of rows
    and columns in a plate (integer values). E.g. 'c(nrow=16L, ncol=24L)' for 384-well plates.}
}


\details{

This function can be used to convert between different ways of specifying well coordinates within a plate.
For example, wells can be identified by an alphanumeric character (e.g. "B02" or c("B", "02")) or by an integer value (e.g. 26). See Examples.
}


\value{
A list with elements:
\code{letnum}, with the alphanumeric well identifiers;
\code{let.num}, with the alphanumeric well identifiers giving as a pair c(LETTER, 2-digits);
\code{num}, with the position of the well within a plate.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk} and W. Huber \email{huber@ebi.ac.uk}}

\examples{
    pd <- c("nrow"=8L, "ncol"=12L) # 96-well plate
    w <- sample(1:prod(pd), 3)
    wpos <- convertWellCoordinates(w, pd)
    wpos
}
\keyword{manip}
