## The workhorse function for the 'Screen Results module': a downloadable ASCII table of
## the screening results and a sortable HTML table. 
writeHtml.screenDescription <- function(cellHTSList, module, overallState, ...)
{
    if(overallState["configured"])
    {
        xr <- cellHTSList$raw
        fname <- module@url
        writeLines(screenDesc(xr), fname)
    }
    return(invisible(NULL))
}
