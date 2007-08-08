

setGeneric("configure", def=function(object, confFile, logFile, descripFile, path, ...) standardGeneric("configure"))

setGeneric("annotate", def=function(object, geneIDFile, path=dirname(geneIDFile), ...) standardGeneric("annotate"))

setGeneric("writeTab", def=function(object, file=paste(object@name, "txt", sep="."), ...) standardGeneric("writeTab"))

setGeneric("scores", def=function(object) standardGeneric("scores"))

setGeneric("scores<-", def=function(object, value) standardGeneric("scores<-"))

setGeneric("description", def=function(object) standardGeneric("description"))

setGeneric("name", def=function(object) standardGeneric("name"))

setGeneric("name<-", def=function(object, value) standardGeneric("name<-"))

setGeneric("state", def=function(object) standardGeneric("state"))

setGeneric("rawdata", def=function(object) standardGeneric("rawdata"))

setGeneric("rawdata<-", def=function(object, value) standardGeneric("rawdata<-"))

setGeneric("normdata", def=function(object) standardGeneric("normdata"))

setGeneric("normdata<-", def=function(object, value) standardGeneric("normdata<-"))

setGeneric("plateEffects", def=function(object) standardGeneric("plateEffects"))

setGeneric("wellAnno", def=function(object) standardGeneric("wellAnno"))

setGeneric("geneAnno", def=function(object) standardGeneric("geneAnno"))

setGeneric("ROC", def=function(object, positives, negatives) standardGeneric("ROC"))


#setGeneric("normalizePlates", def=function(object, scale="additive", log = FALSE, method="median", varianceAdjust="byBatch", posControls, negControls,...) standardGeneric("normalizePlates"))

