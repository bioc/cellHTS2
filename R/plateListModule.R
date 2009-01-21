## The workhorse function for the 'Plate List' module: this is a matrix of quality metrics
## for the different plates and linked per plate quality reports. 
writeHtml.plateList <- function(cellHTSList, module, exptab, url, center, glossary, con, ...)
{
    writeHtml.header(con)
    writeHTMLtable(exptab, url=url, con=con, center=center, glossary=glossary)
    writeHtml.trailer(con)
    return(invisible(NULL))
}
