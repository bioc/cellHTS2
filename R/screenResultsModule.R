## The Workhorse function for the 'Screen Results' module. This writes the topTable outout
## into a downloadable ASCII file and also produces some nice sortable HTML output.
writeHtml.screenResults <- function(cellHTSList, file="topTable.txt", verbose=interactive(),
                                    overallState, con, ...)
{
     if(overallState["scored"]){
         out <- getTopTable(cellHTSList, file=file, verbose=verbose)
         keepFP <- chtsGetSetting(c("screenResults", "keepFieldPattern"))
         keep <- grep(keepFP, colnames(out))
         sel <- !(is.na(out$score))
         out <- out[sel,keep]
         writeHtml.header(con)
         writeLines(sprintf(paste("<div class=\"download\"%s><a href=\"%s\" target=\"_new\"><img",
                                  "src=\"textfileIcon.jpg\"><br>txt version</a></div>"),
                            addTooltip("downloadTable"),
                            file.path("..", "in", basename(file))), con)
         htmlMaxItems <-  chtsGetSetting(c("screenResults", "htmlMaxItems"))
         if(length(unlist(out)) > htmlMaxItems)
         {
             writeLines("<div class=\"alert\">Result table too big to render.<br>
                         Please download txt version using the link to the left.</div>", con)
         }
         else
         {
             classes <- plateListClass(out, rep(1, nrow(out)))
             htmlLinks <- chtsGetSetting(c("screenResults", "htmlLinks"))
             if (!is.null(htmlLinks)) {
               z1 = paste(out$plate, out$well, sep='-')
               z2 = paste(htmlLinks$plate, htmlLinks$well, sep='-')
               htmlLinks = htmlLinks[match(z1, z2), -match(c('plate', 'well'), colnames(htmlLinks)), drop=FALSE]
               htmlLinks = as.list(htmlLinks)
               htmlLinks = lapply(htmlLinks, as.character)
             }
             hwrite(out, table.class="sortable", border=FALSE, center=TRUE, page=con, class=classes,
                    row.names=FALSE, col.link=htmlLinks)
         }
         writeHtml.trailer(con)
         return(NULL)
     }
     else
     {
         return(NA)
     }
 }
