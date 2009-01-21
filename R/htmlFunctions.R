## These functons are called in writeReport.R, QMbyPlate.R, and makePlot.R


## Function using hwriter to color a data.frame in a checkerboard way
## returns a matrix of colors
## dataframe : dataframe to be colored in the html table
dataframeColor = function(dataframe,basicColors=matrix(c("#e0e0f0","#f0f0ff","#d0d0f0","#e0e0ff"), ncol=2, byrow=TRUE)){     
	mcolor=matrix(basicColors[1+(1:ncol(dataframe))%%2,1+(1:nrow(dataframe))%%2],ncol=ncol(dataframe),nrow=nrow(dataframe), byrow=TRUE) 
	return(mcolor)
}


## writes a dataframe of plots in the html report
## x : dataframe
## con : output file
writeHTMLtable4plots = function(x, con) { 
	hwrite("",con, br=TRUE)
	hwrite(x,con, row.names=FALSE, col.names=TRUE, border = 0, center = TRUE, style='font-weight: bold; text-align : center', row.style='font-weight: bold; font-size: 18px; text-align : center', row.bgcolor="#d0d0f0",bgcolor=dataframeColor(x), br = TRUE) 
	hwrite("",con, br=TRUE)
}


## write a dataframe of numeric component
## x : dataframe
## url : links to the by plate results (for main html page)
## con : output file
writeHTMLtable = function(x, url=NA, con, center=FALSE, extra=NULL, glossary=NULL) {
	## checking url
	if(!missing(url)) {
		if(! (is.matrix(url) && is.character(url) && nrow(url)==nrow(x) && ncol(url)==ncol(x))){
			stop("'url' must be a character matrix of the same size as 'x'")
		}
	}
	## glossary
	if(!is.null(glossary)) {
		for(j in 1:ncol(x)){   
			if(!is.na(match( colnames(x)[j], glossary[,1])[1])) {
				word = colnames(x)[j]
				colnames(x)[j]=paste("<a href=\"glossary.html\" onmouseover=\"Tip('",glossary[,2][match(word, glossary[,1])[1]],"', WIDTH, 250, TITLE, 'Definition', OFFSETX, 1)\" onmouseout=\"UnTip()\">", word,"</a>")
			}      
		}
	}
	## writing data.frame 
	hwrite(x,con, row.names=FALSE, col.names=TRUE, link = ifelse(is.na(url),NA,url),border = 0, center = TRUE, bgcolor=dataframeColor(x), style = 'text-align : center',row.style='font-weight: bold; text-align : center',row.bgcolor="#d0d0f0", br = TRUE)
	hwrite("",con, br=TRUE)
}


## function used in makePlot to write plots in the html report
## outf : names of the plots (end with .png or .pdf)
## con : output file
writeImgRef=function(outf,con) {  
	hwrite("",con, br=TRUE)
	hwrite(hwriteImage(outf[2], image.border=2), con, link = outf[1], center = TRUE, br = TRUE)
	hwrite("",con, br=TRUE)
}


