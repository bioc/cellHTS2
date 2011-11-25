\name{convertWellCoordinates}
\alias{convertWellCoordinates}
\title{Converts different well identifiers}
\description{
  For example, "B02"  <-> c("B", "02") <-> 26, 
  "AB32" <-> c("AB", "32") <-> 1328.
}

\usage{
convertWellCoordinates(x, pdim, type="384")
}
\arguments{
  \item{x}{either: a character vector with alphanumeric well identifiers
    (e.g. \code{B03} or \code{AB32}); or an \code{nx2} character matrix whose first column
    contains letters and whose second column contains numbers; or an integer
    vector with position identifiers for wells within a plate (e.g. 27).}
  \item{pdim}{a vector of length 2 with names \code{nrow} and
    \code{ncol} giving the number of rows
    and columns in a plate. E.g. 'c(nrow=32, ncol=48)' for 1536-well 
    plates.}
  \item{type}{an alternative way of specifying \code{pdim}. Supported
    are the values "24" for \code{c(nrow=4, ncol=6)},
    "96" for \code{c(nrow= 8, ncol=12)},
    "384" for \code{c(nrow=16, ncol=24)} and 
    "1536" for \code{c(nrow=32, ncol=48)}.}
}


\value{
A list with elements:
\code{letnum}, with the alphanumeric well identifiers;
\code{let.num}, with the alphanumeric well identifiers giving as a pair c(letter(s), 2-digits);
\code{num}, with the integer position of the well within a plate.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk} and Wolfgang Huber \email{huber@ebi.ac.uk}}

\examples{
    pd <- c("nrow"=8L, "ncol"=12L) # 96-well plate
    w <- sample(prod(pd), 3L)
    wpos <- convertWellCoordinates(w, pd)
    wpos
}
