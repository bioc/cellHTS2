## write HTML output for a single tab. The following mandatory arguments have to be set:
##   title: text used as a title for the tab
##   id: an integer skalar used as idenitfier for the tab in javascript
##   total: the total number of tabs on the page, this is also needed for the javascript
##   url: url to another HTML page which is supposed to be opened in the iFrame as result
##        of clicking the tab
##   con: a connection object
##   class: the css class of the tag, one in 'selected' or 'unselected'
writeHTML.tab <- function(title, id, total, url, class="unselected", image=FALSE, con)
{
    if(!image)
        writeLines(sprintf("
          <table class=\"tab\" id=\"tab%d\" onCLick=\"toggleTabs('%d',%d,'%s')\">",
                           id, id, total, url), con)
    else
        writeLines(sprintf("
          <table class=\"tab\" id=\"tab%d\" onCLick=\"toggleImages('%d',%d)\">",
                           id, id, total), con)
    writeLines(sprintf("
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
          </table>", class, id, class, id, class, id, title, class, id), con)
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
    if(is.null(tabs$class))
        tabs$class <- c("selected", rep("unselected", nrow(tabs)-1))
    for(i in seq_len(nrow(tabs))){
        alist <- as.list(tabs[i,])
        alist$con <- con
        alist$total <- nrow(tabs)
        if(is.null(alist$id))
            alist$id <- i
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


## A class to capture modules of a cellHTS report. We keep this very simple yet generic:
##   title: the name/title of the module. This will later be used as caption on the tab in the final report
##   url: the url to the html code created for the module by htmlFun. The tab will later link to this url
##   htmlFun: an arbitrary function creating all the necessary HTML (and possibly also images). It has to accept a
##            single mandatory argument, the list of raw, and -if available- normalized and scored cellHTS objects
## This should allow for simple extension of the report. In order to keep the code easier to read and understand,
## the computations and image generation in htmlFun should be kept separated from the rendering of HTML. Use the
## chtsImage class and its associated writeHTML method for all images to guarantee a similar look and feel.
setClass("chtsModule",
         representation=representation(title="character",
         url="character",
         htmlFun="function"))




## A class to hold information about images on cellHTS reports. The writeHTML method of the class will produce
## the necessary HTML output, guaranteeing for a common look and feel. None of the slots except for the thumbnail
## are mandatory, and the HTML will be adapted to what is present. There is a notion of image stacks, and those can
## be supplied by the usual R vectorization (i.e. a vector of characters), in which case the HTML will provides the
## selection though tabs. Slots are:
##   shortTitle: a vector of characters used for the tabs to drill down into image stacks. This will be ignored if
##               only a single image is present
##   title: a character scalar or vector of titles for the images
##   caption: a character scalar or vector of subtitles
##   thumbnail: a character scalar or vector of urls to the bitmap versions of the image(s)
##   fullImage: a character scalar or vector of urls to the vectorized versions of the image(s)
setClass("chtsImage",
         representation=representation(shortTitle="character",
         title="character",
         caption="character",
         thumbnail="character",
         fullImage="character"))
## constructor
chtsImage <- function(x)
{
    if(!is.data.frame(x))
        stop("'x' must be a data frame.")
    if(is.null(x$thumbnail))
        stop("You have to specifiy at least an image url to create a 'chtsImage' object.")
    if(nrow(x)>1 && is.null(x$shortTitle))
        x$shortTitle <- paste("Image", seq_len(nrow(x)))
    new("chtsImage", thumbnail=as.character(x$thumbnail), fullImage=as.character(x$fullImage),
        shortTitle=as.character(x$shortTitle), title=as.character(x$title),
        caption=as.character(x$caption))
}
                     

## write html code for chts objects
setGeneric("writeHtml",  function(x, ...)
         standardGeneric("writeHtml")
     )


## create HTML for chtsImage objects. If there are multiple images in the object, a tab navigation structure
## is created. If there is a link to a pdf version, this will also be created
setMethod("writeHtml",
          signature=signature("chtsImage"),
          definition=function(x, con)
      {
          st <- x@shortTitle
          tabs <- data.frame(title=st, id=seq_along(st))
          imgs <- data.frame(ID=seq_along(st), Title=I(x@title), Caption=I(x@caption), FullImage=I(x@fullImage),
                             Pdf=I(sapply(x@fullImage, function(y) ifelse(is.na(y), "", "pdf"))),
                             Thumbnail=x@thumbnail, Class=c("", rep("invisible", length(st)-1)))
          if(any(is.na(imgs)))
             imgs[is.na(imgs)] <- ""
          writeLines("
          <table class=\"image\">", con)
          if(nrow(tabs)>1){
              if(is.null(tabs$class))
                  tabs$class <- c("selected", rep("unselected", nrow(tabs)-1))
              writeLines("
            <tr class=\"image tabs\" align=\"center\">
              <td class=\"image tabs\" colspan=\"2\">", con)
              for(i in seq_len(nrow(tabs))){
                  alist <- as.list(tabs[i,])
                  alist$con <- con
                  alist$total <- nrow(tabs)
                  if(is.null(alist$id))
                      alist$id <- i
                  alist$image <- TRUE
                  do.call("writeHTML.tab", args=alist)
              }
              writeLines("    
              </td>
            </tr>", con)
          }
          for(i in seq_len(nrow(imgs))){
              writeLines(sprintf("   
            <tr class=\"image header %s\" id=\"img%d_1\">
              <td class=\"image header spacer\">
              </td>
              <td class=\"image header\">
                <span class=\"image header\">
	          %s
                </span>
              </td>
            </tr>  
            <tr class=\"image %s\" id=\"img%d_2\">
              <td class=\"image caption\">
                <span class=\"image caption\">
 	          %s
                </span>
              </td>
            <td class=\"image main\">
              <img class=\"image\" src=\"%s\">
            </td>
          </tr>
          <tr class=\"image pdf %s\" id=\"img%d_3\">
            <td class=\"image pdf\" colspan=\"2\">
              <span class=\"image pdf\" onClick=\"linkToPdf('%s');\">
                %s
              </span>
            </td>
          </tr>",  imgs[i, "Class"], imgs[i, "ID"], imgs[i, "Title"], imgs[i, "Class"],
                                 imgs[i, "ID"], imgs[i, "Caption"], imgs[i, "Thumbnail"],
                                 imgs[i, "Class"], imgs[i, "ID"], imgs[i, "FullImage"],  imgs[i, "Pdf"]), con)
          }
          writeLines("
        </table>", con)
          return(invisible(NULL))
      })


writeHtml.header <- function(con)
{
    writeLines("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
  <head>
    <title>
      cellHTS2 Experiment Report
    </title>
    <LINK rel=\"stylesheet\" href=\"cellHTS.css\" type=\"text/css\">
    <SCRIPT type=\"text/javascript\" src=\"cellHTS.js\"></SCRIPT>
  </head>
  <body>", con)
}

writeHtml.trailer <- function(con)
{
    writeLines("
 </body>
</html>", con)
}


tabs <- data.frame(title=c("Plate List", "Plate Configuration", "Plate Summaries", "Screen Summary",
                   "Screen Description", "Screen Results", "Doro's tab"),
                   url=c(rep(c("http://www.ard.de", "testImgs.html", "http://www.dkfz.de"),2), "http://www.google.de"))
con <- file("test.html", open="w")
writeHTML.mainpage(title="a first test", tabs=tabs, con=con)
close(con)


myImg <- chtsImage(data.frame(thumbnail=c("img2.png", "img1.png"),
                              title=c("Boxplot of raw and normalized values", "Controls after normalization"),
                              shortTitle=c("Boxplot", "Controls"),
                              fullImage=c("img2.pdf", "img1.pdf"),
                              caption=c("Replicate 1<br>Left:raw, right:normalized", NA)))



con <- file("testImgs.html", open="w")
writeHtml.header(con)
writeHtml(myImg, con)
writeHtml.trailer(con)
close(con)
