\name{oneRowPerId}
\alias{oneRowPerId}
\title{Rearrange dataframe entries such that there is exactly one row
  per ID.}
\description{
Rearrange dataframe entries such that there is exactly one row
per ID. The IDs are taken from the argument \code{ids} and are matched
against the first column of \code{x}. If an ID is missing in \code{x[,1]},
a row with \code{NA} values is inserted. If an ID occurs multiple times
in \code{x[,1]}, rows are collapsed into characters of comma-separated values.
}
\usage{
oneRowPerId(x, ids)
}
\arguments{
  \item{x}{dataframe.}
  \item{ids}{character vector.}
}

\value{A dataframe whose rows correspond 1:1 to \code{ids}.}

\author{W. Huber \email{huber@ebi.ac.uk}, Ligia Pedroso Bras \email{ligia@ebi.ac.uk}}

\examples{
    x = data.frame(ids=I(c("a", "a", "c")), val=11:13)
    oneRowPerId(x, letters[1:3])
}
\keyword{manip}
