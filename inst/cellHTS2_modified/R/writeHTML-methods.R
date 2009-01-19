## Set up cellHTS HTML pages including all necessary javascript and css links
writeHtml.header <- function(con)
{
    writeLines("<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
  <head>
    <title>
      cellHTS2 Experiment Report
    </title>
    <link rel=\"stylesheet\" href=\"cellHTS.css\" type=\"text/css\">
    <script type=\"text/javascript\" src=\"cellHTS.js\"></script>
   </head>
  <body>
     <script type=\"text/javascript\" src=\"wz_tooltip.js\"></script>", con)
}



## Closing HTML code
writeHtml.trailer <- function(con)
{
    writeLines("
 </body>
</html>", con)
}


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
##         argument list to 'writeHTML.tab' via 'do.call'. The con, class, id and total arguments don't
##         have to be supplied again, they are taken from the function arguments or initialized
##   con: a connection object
writeHTML.mainpage <- function(title, tabs, con)
{
    writeHtml.header(con)
    writeLines(sprintf("
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



## write html code for chts objects
setGeneric("writeHtml",  function(x, ...)
         standardGeneric("writeHtml")
     )


## Create quasi-random guids. This is only based on the time stamp,
## not on MAC address or similar.
guid <- function()
    as.vector(format.hexmode(as.integer(Sys.time())/
                             runif(1)*proc.time()["elapsed"]))



## A class to capture modules of a cellHTS report. We keep this very simple yet generic:
##   title: the name/title of the module. This will later be used as caption on the tab in the final report
##   url: the url to the html code created for the module by htmlFun. The tab will later link to this url
##   htmlFun: an arbitrary function creating all the necessary HTML (and possibly also images). It has to accept three
##            mandatory argument 'cellHTSList', the list of raw, and -if available- normalized and scored cellHTS
##            objects, con, a file connection to write to, and the chtsModule object itself.
##            The return value of the function can be a list of additional
##            elements for the tabs data.frame which later serves as input to writeHTML.mainpage, i.e., everything
##            except 'url' and 'title', which is directly taken from the chtsModule object.
##   funArgs: a list of values for additional function arguments. htmlFun will be called via 'do.call' and this list
## This should allow for simple extension of the report. In order to keep the code easier to read and understand,
## the computations and image generation in htmlFun should be kept separated from the rendering of HTML. Use the
## chtsImage class and its associated writeHTML method for all images to guarantee a similar look and feel.
setClass("chtsModule",
         representation=representation(title="character",
         url="character",
         htmlFun="function",
         funArgs="list"))
## constructor
chtsModule <- function(cellHTSList, title="anonymous", url=file.path(outdir, guid()), htmlFun=function(...){},
                       funArgs=list(cellHTSList=cellHTSList), outdir=".")
{
    if(! "cellHTSList" %in% names(funArgs))
        funArgs$cellHTSList <- cellHTSList
    new("chtsModule", url=url, htmlFun=htmlFun, funArgs=funArgs, title=title)
}



## Call 'htmlFun' with 'funArgs' in a chtsModule object and generate all necessary HTML code
setMethod("writeHtml",
          signature=signature("chtsModule"),
          definition=function(x, con, cellHTSList)
      {
          if(missing(con))
          {
              if(!file.exists(dirname(x@url)))
                  dir.create(dirname(x@url), recursive=TRUE, showWarnings=FALSE)
              con <- file(x@url, open="w")
          }
          if(!is.null(con))
              on.exit(close(con))
          alist <- x@funArgs
          if(! "cellHTSList" %in% names(alist))
          {
              if(missing(cellHTSList))
                  stop("Argument 'cellHTSList' has to be supplied, either as part of the argument ",
                       "list or as a separate parameter.")
              alist$cellHTSList <- cellHTSList
              if(!all(is(alist@cellHTSList, "cellHTS")))
                  stop("The 'cellHTSList' has to be a list of cellHTS objects.")
          }
          alist$module <- x
          alist$con <- con
          tmp <- do.call(x@htmlFun, args=alist)
          res <- data.frame(title=if(!length(x@title)) NA else x@title, url=basename(x@url))
          if(!is.null(tmp) && is.list(tmp) %% names(tmp) %in% c("id", "total", "class"))
              res <- cbind(res, tmp)
          return(invisible(res))
      })



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
                     


## coerce chtsImage to data.frame
setAs(from="chtsImage", to="data.frame", def=function(from)
  {
      st <- if(!length(from@shortTitle)) "foo" else from@shortTitle
      ti <- if(!length(from@title)) NA else from@title
      ca <- if(!length(from@caption)) NA else from@caption
      fi <- if(!length(from@fullImage)) NA else from@fullImage
      df <- data.frame(ID=seq_along(st), Title=I(ti), Caption=I(ca), FullImage=I(fi),
                       Pdf=I(sapply(fi, function(y) ifelse(is.na(y), "", "pdf"))),
                       Thumbnail=from@thumbnail, Class=c("", rep("invisible", length(st)-1)))
       if(any(is.na(df)))
           df[is.na(df)] <- ""
      return(df)
  })

## Create HTML for chtsImage objects. If there are multiple images in the object, a tab navigation structure
## is created. If there is a link to a pdf version, this will also be created.
## The optional argument map will add an imageMap to the HTML code.
setMethod("writeHtml",
          signature=signature("chtsImage"),
          definition=function(x, con, map)
      {
          st <- x@shortTitle
          if(!length(st))
              st <- "foo"
          tabs <- data.frame(title=st, id=seq_along(st))
          imgs <- as(x, "data.frame")
          if(missing(map))
          {
              map <- vector(mode="list", length=nrow(imgs))
          }
          else
          {
              if(length(map) != nrow(imgs))
                  stop("The length of the imageMap list doesn't match the number of images.")
          }
          writeLines("
          <table class=\"image\" align=\"center\">", con)
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
              <img class=\"image\" src=\"%s\" %s>
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
                                 if(!is.null(map[[i]])) map[[i]] else "",
                                 imgs[i, "Class"], imgs[i, "ID"], imgs[i, "FullImage"],  imgs[i, "Pdf"]), con)
          }
          writeLines("
        </table>", con)
          return(invisible(NULL))
      })




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
