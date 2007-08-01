\name{write.tabdel}
\alias{write.tabdel}
\title{Wrapper to function 'write.table' used to write data to a tab-delimited file}
\description{
  Wrapper for the function \code{\link[base:write.table]{write.table}}
  to write data to a tab-delimited file.
}
\usage{
write.tabdel(\dots)
}
\arguments{
 \item{...}{arguments that get passed on to the function
 \code{\link[base:write.table]{write.table}}.}
}

\details{A trivial function, which we have included for convenience.}
\value{The name of the file that was written.}
\seealso{\code{\link[base:write.table]{write.table}}}
\author{Ligia Braz \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- KcViabSmall
    ## determine the ratio between each well and the plate median
    y <- array(as.numeric(NA), dim=dim(x@xraw))
    nrWell <- dim(x@xraw)[1]
    for(p in 1:(dim(x@xraw)[2])) {
      samples <- (x@wellAnno[(1:nrWell)+nrWell*(p-1)]=="sample")
      y[, p, , ] <- apply(x@xraw[, p, , , drop=FALSE], 3:4, function(w) w/median(w[samples], na.rm=TRUE))
    }
    y <- signif(y, 4)
    out <- matrix(y, nrow=prod(dim(y)[1:2]), ncol=dim(y)[3:4])
    out <- cbind(x@geneAnno, x@wellAnno, out)
    colnames(out) <- c(names(x@geneAnno), "wellAnno",
       sprintf("Well/Median_r\%d_ch\%d", rep(1:dim(y)[3], dim(y)[4]), rep(1:dim(y)[4], each=dim(y)[3])))
    write.tabdel(out, file = tempfile())
}
\keyword{manip}
