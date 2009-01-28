## function used in makePlot to write plots in the html report
## outf : names of the plots (end with .png or .pdf)
## con : output file
writeImgRef <- function(outf,con)
{  
    hwrite("",con, br=TRUE)
    hwrite(hwriteImage(outf[2], image.border=2), con, link=outf[1], center=TRUE, br=TRUE)
    hwrite("",con, br=TRUE)
}


