\name{write.tabdel}
\alias{write.tabdel}
\title{Wrapper to function 'write.table' used to write data to a
  tab-delimited file} 
\description{
  Wrapper for the function \code{\link[base:write]{write.table}}
  to write data to a tab-delimited file.
}
\usage{
write.tabdel(\dots)
}
\arguments{
 \item{...}{arguments that get passed on to the function
 \code{\link[base:write]{write.table}}.}
}

\details{A trivial function, which we have included for convenience.}
\value{The name of the file that was written.}
\seealso{\code{\link[base:write]{write.table}}}
\author{Ligia Bras \email{ligia@ebi.ac.uk}}

\examples{
    data(KcViabSmall)
    x <- KcViabSmall
    ## determine the ratio between each well and the plate median
    xraw <- Data(x)
    y <- array(as.numeric(NA), dim=dim(xraw))
    nrWell <- prod(pdim(x))

    for(p in 1:max(plate(x))) {
      ind <- (1:nrWell)+nrWell*(p-1)
      samples <- (wellAnno(x)[ind]=="sample")
      y[ind, ,] <- apply(xraw[ind, , ,drop=FALSE], 2:3, function(w) w/median(w[samples], na.rm=TRUE))
    }
    y <- signif(y, 4)
    out <- y[,,1]
    out <- cbind(geneAnno(x), wellAnno(x), out)
    colnames(out) <- c("GeneID", "wellAnno",
       sprintf("Well/Median_r\%d_ch\%d", rep(1:dim(y)[2], dim(y)[3]), rep(1:dim(y)[3], each=dim(y)[2])))
    write.tabdel(out, file = tempfile())
}
\keyword{manip}
