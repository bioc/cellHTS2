## The Workhorse function for the 'Screen Results' module. This writes the topTable outout
## into a downloadable ASCII file and also produces some nice sortable HTML output.
writeHtml.screenResults <- function(cellHTSList, file="topTable.txt", verbose=interactive(),
                                    overallState, con, ...)
{
     if(overallState["configured"]){
         out <- getTopTable(cellHTSList, file=file, verbose=verbose)
         rownames(out) <- NULL
         writeHtml.header(con)
         writeLines(sprintf(paste("<div class=\"download\"><a href=\"%s\" target=\"_new\"><img",
                                  "src=\"textfileIcon.jpg\"><br>txt version</a></div>"),
                            basename(file)), con)
         writeLines("<center>", con)
         hwrite(out, table.class="sortable", border=FALSE, page=con)
         writeLines("</center>", con)
         writeHtml.trailer(con)
     }
     return(invisible(NULL))
}
