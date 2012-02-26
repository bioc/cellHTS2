## output dataframe
dataframeOutput = function(x, header, caption, label, gotable=FALSE, prename="cellhts2") {

  head = c("\\begin{table}[tp]", "\\begin{center}")
  tail = c(sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", label),
    "\\end{center}", "\\end{table}")

if (!gotable) out = paste("\\begin{tabular}{", paste(rep("r", ncol(x)), collapse=""), "}", sep="") else out = paste("\\small\\begin{tabular}{", 
     paste(paste(rep("r", ncol(x)-1), collapse=""),"p{0.5\\textwidth}", collapse=""), "}", sep="")

  if(header)
    out = c(out, paste(paste("\\textbf{", colnames(x), "}", sep="", collapse="&"), "\\\\", sep=""))
  for(i in 1:nrow(x))
    out = c(out, paste(paste(x[i,], collapse="&"), "\\\\", sep=""))
  out = c(out, "\\end{tabular}")

  writeLines(c(head, out, tail), con=sprintf("%s-%s.tex", prename, label))
  writeLines(out, con=sprintf("%s-%s.txt", prename, label))
}

## output a file
tableOutput = function(fn, nm, header=TRUE, dropColumns, selRows=1:5, preName="cellhts2") {
  r = read.table(fn, sep="\t", header=header,  na.strings="", as.is=TRUE)
  x = r[c(selRows, 1), ]
  if(!missing(dropColumns))
   x = x[, -dropColumns]
  for(i in 1:ncol(x)) {
    x[[i]]=I(as.character(x[[i]]))
    x[[i]][length(x[[i]])]="..."
  }

  dataframeOutput(x, header=header,
    caption=sprintf("Selected lines from the example %s file \\texttt{%s}.", 
      nm, gsub("_", "\\\\_", basename(fn))),
    label = gsub(" ", "", nm), prename=preName)

}



## output a file when the file has header rows (e.g. the current format of the plate configuration file)
tableOutputWithHeaderRows = function(fn, nm, dropColumns, selRows=NULL, preName="cellhts2") {
  r = read.table(fn, fill=TRUE, header=FALSE, as.is=TRUE, na.strings="")#sep="\t", header=header,  na.string="", as.is=TRUE)
  x <- if(!is.null(selRows)) r[c(selRows, 1), ] else r
  if(!missing(dropColumns))
   x = x[, -dropColumns]

  for(i in 1:ncol(x)) {
    x[[i]]=I(as.character(x[[i]]))
    if(!is.null(selRows)) x[[i]][length(x[[i]])]="..."
  }
# replace empty entries in the 2 header rows by "":
nc <- which(rowSums(is.na(t(x[1:2,])))==2)
x[1:2,nc] <- "" 

  dataframeOutput(x, header=FALSE,
    caption=sprintf("Selected lines from the example %s file \\texttt{%s}.", 
      nm, gsub("_", "\\\\_", basename(fn))),
    label = gsub(" ", "", nm), prename=preName)
}
