## The workhorse function for the 'Plate List' module: this is a matrix of quality metrics
## for the different plates and linked per plate quality reports. 
writeHtml.plateList <- function(cellHTSList, module, exptab, url, center, glossary, con, ...)
{
    writeHTMLtable(exptab, url=url, con=con, center=center, glossary=glossary)
    return(invisible(NULL))
}
