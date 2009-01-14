## write HTML output for a single tab. The following mandatory arguments have to be set:
##   title: text used as a title for the tab
##   id: an integer skalar used as idenitfier for the tab in javascript
##   total: the total number of tabs on the page, this is also needed for the javascript
##   url: url to another HTML page which is supposed to be opened in the iFrame as result
##        of clicking the tab
##   con: a connection object
##   class: the css class of the tag, one in 'selected' or 'unselected'
writeHTML.tab <- function(title, id, total, url, class="unselected", con)
{
    writeLines(sprintf("
          <table class=\"tab\" id=\"tab%d\" onCLick=\"toggleTabs('tab%d',%d,'%s')\">
            <tr class=\"tab\">
              <td class=\"tab left %s\" id=\"tab%d_1\">
                &nbsp&nbsp
              </td>
              <td class=\"tab middle %s\" id=\"tab%d_2\">
                <div class=\"tab %s\" id=\"tab%d_3\">
                  %s
                </div>
              </td>
              <td class=\"tab right %s\" id=\"tab%d_4\">
                &nbsp&nbsp
              </td>
            </tr>
          </table>", id, id, total, url, class, id, class, id, class, id, title, class, id), con)
}


## write the overall HTML framework for a cellHTS report. This is mainly a bounding table, a set
## of tabs linking to further sub-pages and an iFrame which serves as canvas for these sub-pages.
## The following mandatory arguments have to be set
##   title: the experiment title
##   tabs: a data frame with all necessary values for the tabs. Each row will be supplied as
##         argument list to 'writeHTML.tab' via 'do.call'. The con and total arguments don't have to be
##         supplied again, they are taken from the function arguments
##   con: a connection object
writeHTML.mainpage <- function(title, tabs, con)
{
    writeLines(sprintf("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
  <head>
    <title>
      cellHTS2 Experiment Report
    </title>
    <LINK rel=\"stylesheet\" href=\"cellHTS.css\" type=\"text/css\">
    <SCRIPT type=\"text/javascript\" src=\"cellHTS.js\"></SCRIPT>
  </head>
  <body>
    <table class=\"border\">
      <tr class=\"border top\">
        <td class=\"border corner\">
          &nbsp&nbsp&nbsp&nbsp
        </td>
        <td class=\"border top\">
          <div class=\"header\">
	    Report for Experiment <span class=\"header\">%s</span>
          </div>
          <div class=\"timestamp\">
            generated %s
          </div>
          <div class=\"logo\">
	    cellHTS2
          </div>
        </td>
      </tr>
      <tr class=\"border middle\">
        <td class=\"border left\"></td>
        <td class=\"main\">", title, date()), con)
    for(i in seq_len(nrow(tabs))){
        alist <- as.list(tabs[i,])
        alist$con <- con
        alist$total <- nrow(tabs)
        do.call("writeHTML.tab", args=alist)
    }
    writeLines(sprintf("
          <div class=\"main\">
	    <iframe class=\"main\" src=\"%s\" name=\"main\" frameborder=\"0\"noresize>
	      <p>
	        Your browser does not support iFrames. This page will not work for you.
	      </p>
	    </iframe>
          </div>
        </td>
      </tr>
    </table>
  </body>
</html>", tabs[1,"url"]), con)
return(invisible(NULL))
}


tabs <- data.frame(title=c("Plate List", "Plate Configuration", "Plate Summaries", "Screen Summary",
                   "Screen Description", "Screen Results", "Doro's tab"),
                   id=1:7, class=c("selected", rep("unselected", 6)),
                   url=c(rep(c("http://www.ard.de", "http://www.google.de", "http://www.dkfz.de"),2), "http://www.google.de"))
con <- file("test.html", open="w")
writeHTML.mainpage(title="a first test", tabs=tabs, con=con)
close(con)







