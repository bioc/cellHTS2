## The workhorse function for the 'Plate Summaries' module: boxplots of raw and normalized
## data as well as controls plots.
writeHtml.experimentQC <- function(cellHTSList, module, con, allControls, allZfac)
{
    outdir <- dirname(module@url)
    xn <- cellHTSList$normalized
    xr <- cellHTSList$raw
    plotTable <- QMexperiment(xr, xn, outdir, con, allControls, allZfac)		
    writeHTMLtable4plots(plotTable, con=con)
}
