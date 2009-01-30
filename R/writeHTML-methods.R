## Set up cellHTS HTML pages including all necessary javascript and css links
writeHtml.header <- function(con, path=".")
{
    doc <- c("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\
\"http://www.w3.org/TR/html4/loose.dtd\">",
             "\"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">")
    out <-sprintf("%s
<html>
  <head>
    <title>
      cellHTS2 Experiment Report
    </title>
    <link rel=\"stylesheet\" href=\"%s/cellHTS.css\" type=\"text/css\">
    <script type=\"text/javascript\" src=\"%s/cellHTS.js\"></script>
    <script src=\"%s/sorttable.js\"></script>
   </head>
   <body onload=\"initialize();\">
     <script type=\"text/javascript\" src=\"%s/wz_tooltip.js\"></script>",doc[1], path,
                  path, path, path)
    if(!missing(con))
        writeLines(out, con)
    return(invisible(out))
}



## Closing HTML code
writeHtml.trailer <- function(con)
{
    out <-
"  </body>
</html>"
    if(!missing(con))
        writeLines(out, con)
    return(invisible(out)) 
}

 

## Add markup and javascript for a tooltip bubble
addTooltip <- function(word, title="Definition", fromGlossary=TRUE, link=FALSE)
{
    link <- if(link) " onClick=if(tt_Enabled) linkToFile('glossary.html');" else ""
    desc <- if(fromGlossary) getDefinition(word, createGlossary()) else word
    sprintf(" onmouseover=\"Tip('%s', WIDTH, 250, TITLE, '%s', OFFSETX, 1);\" onmouseout=\"UnTip();\"%s",
            desc, title, link)
}



## write HTML output for a single tab. The following mandatory arguments have to be set:
##   title: text used as a title for the tab
##   id: a character scalar used as identifier for the tab collection in javascript
##       (this identifies the collection of tabs, there may be multiple collections on one page)
##   script: a character containing the java script that is associated to the tab
##   url: url to another HTML page which is supposed to be opened in the iFrame as result
##        of clicking the tab
##   size: there are three different sizes of tabs: big, medium and small
##   con: a connection object
##   class: the css class of the tag, one in 'selected' or 'unselected'
writeHtml.tab <- function(title, url, id="mainTab",
                          script=sprintf("toggleTabById('%s', this, '%s')", id, url),
                          class, size, con)
{
    out <- sprintf("
	    <table class=\"%s %s %s\" onClick=\"%s\">
	      <tr>
		<td class=\"left\">
		  &nbsp&nbsp
		</td>
		<td class=\"middle\">
                  <span>
		     %s
                  </span>
		</td>
		<td class=\"right\">
		  &nbsp&nbsp		
		</td>
	      </tr>
	    </table>" , size, class, id, script, title)
    if(!missing(con))
        writeLines(out, con)
    return(invisible(out)) 
}


## Produce HTML output for a whole collection of tabs.
##   tabs: a data frame with all necessary values for the tabs. Each row will be supplied as
##         argument list to 'writeHtml.tab' via 'do.call'. The con, class, id, script and size
##         arguments don't have to be supplied again, they are taken from the function arguments
##         or are initialized as needed.
##   con: a connection object
writeHtml.tabCollection <- function(tabs, size=c("big", "medium", "small"), con)
{
    size <- match.arg(size)
    tabs$size <- size
    out <- sprintf("
    <div class=\"tab %s\">
      <table class=\"bar %s\">
	<tr>
	  <td class=\"topbar\">
	  </td>
	</tr>
	<tr>
	  <td class=\"bar\">
	  </td>
	</tr>
      </table>
      <table class=\"tabs\">
	<tr>
	  <td>", size, size)
    if(is.null(tabs$class))
        tabs$class <- c("selected", rep("unselected", nrow(tabs)-1))
    for(i in seq_len(nrow(tabs))){
        alist <- as.list(tabs[i,])
        alist$con <- NULL
        out <- c(out, do.call("writeHtml.tab", args=alist))
    }
    out <- c(out,"
           </td>
	</tr>
      </table>
    </div>")
    if(!missing(con))
        writeLines(out, con)
    return(invisible(out)) 
}



## write the overall HTML framework for a cellHTS report. This is mainly a bounding table, a set
## of tabs linking to further sub-pages and an iFrame which serves as canvas for these sub-pages.
## The following mandatory arguments have to be set
##   title: the experiment title
##   tabs: a data frame with all necessary values for the tabs. Each row will be supplied as
##         argument list to 'writeHtml.tab' via 'do.call'. The con, class, id, script and size
##         arguments don't have to be supplied again, they are taken from the function arguments
##         or are initialized as needed.
##   con: a connection object
writeHtml.mainpage <- function(title, tabs, con)
{
    writeHtml.header(con, path="html")
    writeLines(sprintf("
    <table class=\"border\">
      <tr class=\"border top\">
        <td class=\"border corner\">
          &nbsp&nbsp&nbsp&nbsp
          <div class=\"helpSwitch\">
	     help: 
	    <span onClick=\"toggleHelp(this);\" id=\"helpSwitch\"%s>
	    </span>
	  </div>
        </td>
        <td class=\"border top\">
          <div class=\"header\">
	    Report for Experiment <span class=\"header\">%s</span>
          </div>
	  <div class=\"timestamp\">
            generated %s
          </div>
          <div class=\"logo\">
	  </div>
        </td>
      </tr>
      <tr class=\"border middle\">
        <td class=\"border left\"></td>
        <td class=\"main\">", addTooltip("switchHelp", "Help"),
                       title, format(Sys.time(), "%a %b %d %H:%M %Y")), con)
    tabs <- tabs[!apply(tabs, 1, function(y) all(is.na(y))),]
    writeHtml.tabCollection(tabs, size="medium", con=con)
    writeLines(sprintf("
          <div class=\"main\">
	    <iframe class=\"main\" src=\"%s\" name=\"main\" frameborder=\"0\" noresize id=\"main\"
              scrolling=\"no\" marginwidth=\"0\ marginheight=\"0\">
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
##            elements for the tabs data.frame which later serves as input to writeHtml.mainpage, i.e., everything
##            except 'url' and 'title', which is directly taken from the chtsModule object. If the return value is
##            NA, the respective tab will be omitted. This is useful to handle conditional generation of particular
##            modules in the htmFun function rather than directly in 'writeReport'.
##   funArgs: a list of values for additional function arguments. htmlFun will be called via 'do.call' and this list
## This should allow for simple extension of the report. In order to keep the code easier to read and understand,
## the computations and image generation in htmlFun should be kept separated from the rendering of HTML. Use the
## chtsImage class and its associated writeHtml method for all images to guarantee a similar look and feel.
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



## Call 'htmlFun' with 'funArgs' in a chtsModule object and generate all necessary HTML code. Setting the
## 'con' argument to NULL results in not opening a file connection. This might be handy in case one wants
## to link to an already existing file, or to handle the file generation directly in the HTML function.
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
          if(!is.null(tmp) && is.na(tmp))
              return(NA)
          url <- file.path("html", basename(x@url))
          title <- if(!length(x@title)) NA else x@title
          res <- data.frame(title=title, url=url,
                            script=sprintf("toggleTabById('%s', this, '%s');\"%s",
                            "mainTab", url, addTooltip(title, "Help")))
          if(!is.null(tmp) && is.list(tmp) && names(tmp) %in% c("id", "total", "class"))
              res <- cbind(res, tmp)
          return(invisible(res))
      })



## A class to hold information about images on cellHTS reports. The writeHtml method of the class will produce
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
##   additionalCode: a character scalar or vector of arbitrary HTML code to be added to the bottom of the image
##   map: a character scalar or vector of valid HTML imageMap code for each image
##   jsclass: a character scalar which is used to identify the image in the javascripts. Additional
##          classes for the respective channel and replicate versions of the image are augmented
##          automatically.
setClass("chtsImage",
         representation=representation(shortTitle="character",
         title="character",
         caption="character",
         thumbnail="character",
         fullImage="character",
         additionalCode="character",
         map="character",
         jsclass="character"))

## constructor
chtsImage <- function(x)
{
    if(!is.data.frame(x))
        stop("'x' must be a data frame.")
    if(is.null(x$additionalCode))
        x$additionalCode <- ""
    if(is.null(x$map))
        x$map <- ""
    if(nrow(x)>1 && is.null(x$shortTitle))
        x$shortTitle <- paste("Image", seq_len(nrow(x)))
    new("chtsImage", thumbnail=as.character(x$thumbnail), fullImage=as.character(x$fullImage),
        shortTitle=as.character(x$shortTitle), title=as.character(x$title),
        additionalCode=as.character(x$additionalCode), map=as.character(x$map),
        caption=as.character(x$caption), jsclass=if(!is.null(x$jsclass)) x$jsclass else "default")
}
                     


## coerce chtsImage to data.frame
setAs(from="chtsImage", to="data.frame", def=function(from)
  {
      ltm <- max(sapply(slotNames(from), function(x) length(slot(from, x))))
      tm <- if(!length(from@thumbnail)) rep(NA, ltm)  else from@thumbnail
      ltm <- length(tm)
      st <- if(!length(from@shortTitle)) "foo" else from@shortTitle
      ti <- if(!length(from@title)) rep(NA, ltm)  else from@title
      ca <- if(!length(from@caption)) rep(NA, ltm) else from@caption
      fi <- if(!length(from@fullImage)) rep(NA, ltm) else from@fullImage
      map <- if(!length(from@map)) rep(NA, ltm) else from@map
      ac <- if(!length(from@additionalCode)) rep(NA, ltm) else from@additionalCode
      df <- data.frame(ID=seq_len(ltm), Title=I(ti), Caption=I(ca), FullImage=I(fi),
                       Pdf=I(sapply(fi, function(y) ifelse(is.na(y), "", "pdf"))),
                       Thumbnail=I(tm), Class=from@jsclass, AdditionalCode=ac, Map=map)
       if(any(is.na(df)))
           df[is.na(df)] <- ""
      return(df)
  })

## Create HTML for chtsImage objects. If there are multiple images in the object, a tab navigation structure
## is created. If there is a link to a pdf version, this will also be created.
## The optional argument map will add an imageMap to the HTML code.
setMethod("writeHtml",
          signature=signature("chtsImage"),
          definition=function(x, con, stack=FALSE, vertical=TRUE)
      {
          st <- x@shortTitle
          if(!length(st))
              st <- "foo"
          tabs <- data.frame(title=st, id=seq_along(st))
          imgs <- as(x, "data.frame")
          out <- if(!vertical) "<table align=\"center\"><tr><td>" else "" 
          for(i in seq_len(nrow(tabs))){
              out <- c(out, sprintf("
          <table class=\"image %s\" align=\"center\">", unique(imgs[i,"Class"])))
              if(nrow(tabs)>1 && !stack)
              {
                  if(is.null(tabs$class))
                      tabs$class <- c("selected", rep("unselected", nrow(tabs)-1))
                  out <- c(out, "
            <tr>
              <td class=\"tabs\">")
                  for(j in seq_len(nrow(tabs)))
                  {
                      alist <- as.list(tabs[j,])
                      alist$size <- "small"
                      if(is.null(alist$id))
                          alist$id <- j
                      alist$image <- TRUE
                      out <- c(out, do.call("writeHtml.tabCollection", args=alist))
                  }
                  out <- c(out, "    
              </td>
            </tr>")
              }
              out <- c(out, sprintf("
            <tr>
              <td class=\"header\">
                <div class=\"header\">
	          %s
                </div>
                <div class=\"caption\">
	          %s
                </div>
              </td>
            </tr>  
            <tr>
              <td class=\"main\">
                <img class=\"image\" src=\"%s\" %s>
                  %s
               </td>
            </tr>
            <tr>
              <td class=\"pdf\">
                <span class=\"pdf\" onClick=\"linkToPdf('%s');\"%s>
                   %s
                </span>
              </td>
            </tr>",  imgs[i, "Title"], imgs[i, "Caption"], imgs[i, "Thumbnail"], imgs[i, "Map"],
                                    imgs[i, "AdditionalCode"], imgs[i, "FullImage"],
                                    addTooltip("pdf", "Help"), imgs[i, "Pdf"]))
              out <- c(out, "
          </table>")
              if(!vertical)
                  out <- c(out, "</td><td>")
          }
          if(!vertical)
              out <- c(out, "</td></tr></table>")
          if(!missing(con))
              writeLines(out, con)
          return(invisible(out))
      })



## A class to hold chtsImage objects for multiple channels and replicates
## Each element of the first list is supposed to be one channel, and the
## elements in the subsequent list are the replicates. For each channel and replicate
## there may be exactly one chtsImage object, possibly with multiple sub-images
setClass("chtsImageStack",
         representation(stack="list", id="character", title="character", tooltips="character"),
         prototype(stack=list(list())))

## constructor
chtsImageStack <- function(stack=list(list()), id, title=as.character(NULL), tooltips=as.character(NULL))
{
    nrChan <- length(stack)
    nrRep <- unique(sapply(stack, length))
    if(length(nrRep)!=1)
        stop("Need the same number of replicates or images for each channel")
    vals <- sapply(unlist(stack), is, "chtsImage")
    if(!all(vals))
        stop("All elements of the outer lists must be 'chtsImage' objects")
    if(length(tooltips) && length(tooltips) != length(stack[[1]]))
        stop("'tooltips' must be a vector of the same length as number of replicates")
    new("chtsImageStack", stack=stack, id=id, title=title, tooltips=tooltips)
}


## Create HTML output. The organisation of the image stack is as follows:
##   For vertical=TRUE
##     each channel will be on a separate tab
##     each replicates will be on a separate tab
##     the individual images in the chtsImage objects are vertically stacked
##   For vertical=FALSE
##     each channel will be on a separate tab
##     each image from each chtsImage object will be on a separate tab
##     the replicates are horizontally stacked
setMethod("writeHtml",
          signature=signature("chtsImageStack"),
          definition=function(x, con, vertical=TRUE)
      {
          nrChan <- length(x@stack)
          nrRep <- unique(sapply(x@stack, length))
          class <- "imageStack"
          out <- sprintf("
            <table class=\"%s\" align=\"center\">
              %s
              <tr>
                <td class=\"tabs\">", class,
                         if(length(x@title)) sprintf("<tr><td class=\"header\">%s</td></tr>", x@title) else "")
          if(nrChan>1){
              chanTabs <- data.frame(title=paste("Channel", seq_len(nrChan)),
                                     id=paste(x@id, "Channel", sep=""),
                                     script=sprintf("toggleTabByChannel('%sChannel', this, %d)", x@id,
                                     seq_len(nrChan)))
              out <- c(out, writeHtml.tabCollection(chanTabs, size="medium"))
          }
          imgs <- !is.null(names(x@stack[[1]]))
          title <- if(!imgs) paste("Replicate", seq_len(nrRep)) else names(x@stack[[1]])
          if(nrRep>1){
              tt <- if(length(x@tooltips)) paste("\"", x@tooltips) else ""
              repTabs <- data.frame(title=title,
                                    id=paste(x@id, "Replicate", sep=""),
                                    script=sprintf("toggleTabByReplicate('%sReplicate', this, %d) %s", x@id,
                                    seq_len(nrRep), tt))
              out <- c(out, writeHtml.tabCollection(repTabs, size="small"))
          }
          out <- c(out, "
              </td>
            </tr>
            <tr>
              <td>")
          vert <- if(vertical) "" else "horizontal"
          for(i in seq_len(nrChan)){
              for(j in seq_len(nrRep)){
                  viz <- if(i==1 && j==1) "visible" else "invisible"             
                  img <- x@stack[[i]][[j]]
                  img@jsclass <- sprintf(" %s %s %s channel%d replicate%d", vert, viz, x@id, i, j)
                  out <- c(out, writeHtml(img, stack=TRUE, vertical=vertical))
              }
          }
          out <- c(out, "
              </tr>
            </td>  
          </table>")
          if(!missing(con))
              writeLines(out, con)
          return(invisible(out))     
      })
