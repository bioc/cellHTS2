## The Workhorse function for the 'Screen Results' module. This writes the topTable outout
## into a downloadable ASCII file and also produces some nice sortable HTML output.
writeHtml.screenResults <- function(cellHTSList, file="topTable.txt", verbose=interactive(),
                                    overallState, ...)
{
     if(overallState["configured"]){
         out <- getTopTable(cellHTSList, file=file, verbose=verbose)
         writeHtml.header(con)
         hwrite("txt version", link=basename(file), page=con)
         hwrite(out, table.class="sortable", border=FALSE, page=con)
         writeHtml.trailer(con)
     }
     return(invisible(NULL))
}
