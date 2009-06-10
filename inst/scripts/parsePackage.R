## Source the whole package for debugging purpose.
sourcePackage <- function(path="~/Rpacks/cellHTS2/R/")
{
    library(prada)
    library(geneplotter)
    for(i in dir(path, full=TRUE))
        source(i, local=TRUE)
    invisible()
}
