

setGeneric("configure", def=function(object, confFile, logFile, descripFile, path, ...) standardGeneric("configure"))

setGeneric("annotate", def=function(object, geneIDFile, path=dirname(geneIDFile), ...) standardGeneric("annotate"))

setGeneric("writeTab", def=function(object, file=paste(object@name, "txt", sep="."), ...) standardGeneric("writeTab"))

setGeneric("scores", def=function(object) standardGeneric("scores"))

setGeneric("scores<-", def=function(object, value) standardGeneric("scores<-"))

setGeneric("description", def=function(object) standardGeneric("description"))

setGeneric("state", def=function(object) standardGeneric("state"))