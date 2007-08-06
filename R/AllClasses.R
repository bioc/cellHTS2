## Validity functions and class definitions.

##------------------------------------------------------------------------------
## Validity function
##------------------------------------------------------------------------------
## Dimension consistency
## length(pdim) = 2
## length(state) = 4
## xraw - dim 4
## xnorm - dim 4
## dim(xraw)[1] = prod(pdim)
## length(score) = prod(dim(xraw)[1:2]) or 0 (in case, the data are not scored yet)

equalOrZero = function(i, j) ((i==j)||(i==0))

#--------------------------------------------------------------------------------
checkMandatoryColumns = function(object, name, mandatory, numeric) {

  obj = slot(object, name)

  missingColumns = setdiff(mandatory, colnames(obj))
  if(length(missingColumns)>0)
    return(sprintf("Column%s %s %s missing from %s", ifelse(length(missingColumns)>1, "s ",""),
               paste(missingColumns, collapse=", "),
               ifelse(length(missingColumns)>1, " are"," is"),
               name))
  
  for(j in intersect(numeric, colnames(obj)))
    if(!is.numeric(obj[,j]) || any(is.na(obj[,j])))
      return(sprintf("Column %s in %s must be numeric and not contain missing values.",
                   j, name))
  }

#--------------------------------------------------------------------------------

validityCellHTS = function(object){
  if(length(object@name)!=1) return("'name' should be a character of length 1 giving the experiment name.")

  if(any(is.na(object@pdim))||length(object@pdim)!=2) return("'pdim' must be a numeric vector of length 2 and with no NA values.")

  if (length(object@state)!=4 || !all(is.logical(object@state)) || !all(names(object@state) %in% c("configured", "normalized", "scored", "annotated"))) return("'state' should be a logical vector of length 4.")

  if(any(is.na(object@batch))) return("'batch' must not contain NA values.")

  if(length(dim(object@xraw))!=4) return("'xraw' must be a 4D array.")

  if(length(dim(object@xnorm))!=4) return("'xnorm' must be a 4D array.")
 
  if(length(dim(object@rowcol.effects))!=4) return("'rowcol.effects' must be a 4D array.")

  if(length(dim(object@overall.effects))!=4) return("'overall.effects' must be a 4D array.")

  if(!equalOrZero(length(object@score), prod(dim(object@xraw)[1:2]))) return("'score' must be a vector of length 'prod(dim(xraw)[1:2])'.")

  if(prod(object@pdim)!=dim(object@xraw)[1]) return("'prod(pdim)' and 'dim(xraw)[1]' must match.")

  if(length(object@batch)!=dim(object@xraw)[2]) return("The length of 'batch' must match 'dim(xraw)[2]'.")

  if(!equalOrZero(length(object@wellAnno), prod(dim(object@xraw)[1:2]))) return("'length(wellAnno)' must be equal to 'prod(dim(xraw)[1:2])'.")

  if(any(dim(object@xraw)[1:3]!=dim(object@xnorm)[1:3]) && length(object@xnorm)!=0) return("Please check dimensions of 'xraw' and/or 'xnorm'.")
  
  if(any(dim(object@xraw)[1:3]!=dim(object@rowcol.effects)[1:3]) && length(object@rowcol.effects)!=0) return("Please check dimensions of 'rowcol.effects'.")

  if(any(dim(object@xraw)[2:3]!=dim(object@overall.effects)[2:3]) && length(object@overall.effects)!=0) return("Please check dimension of 'overall.effects'.")

  if(dim(object@overall.effects)[1]!=1) return("Please check dimension of 'overall.effects'.")

  if(length(object@intensityFiles)!=nrow(object@plateList)) return("'length(intensityFiles)' should match 'nrow(plateList)'.")

  if(object@state["configured"]) {
     if(length(object@plateConf)==0) return("If state['configured']==TRUE, 'plateConf' should not be empty!")
     checkMandatoryColumns(object, "plateConf", mandatory=c("Batch", "Well", "Content"),
               numeric=c("Batch"))

     if(nrow(object@plateConf)!=(max(object@batch)*dim(object@xraw)[1])) return("nrow(plateConf) must be equal to the product between the number of wells in each plate and the number of plates.")

     if(all(object@screenDesc=="")) return("If state['configured']==TRUE, 'screenDesc' should not be empty!")

    if(length(object@screenLog)==0 & !is.null(object@screenLog)) 
           return("If state['configured']==TRUE and a screen log file exists for this screen, 'screenLog' should not be empty!")

    if(!is.null(object@screenLog)) checkMandatoryColumns(object, "screenLog", mandatory=c("Filename", "Well", "Flag"),
                   numeric=character(0))

    if(length(object@wellAnno)==0) {
     return("If state['configured']==TRUE, 'wellAnno' should not be empty!") 
    }else{
      if(!("sample" %in% levels(object@wellAnno))) return("Expecting a level called 'sample' in 'wellAnno'!")
    }
 }


  if(object@state["scored"] && length(object@score)==0) return("If state['scored']==TRUE, 'xnorm' must not be empty!")

  if(object@state["annotated"]) {

   if(length(object@geneAnno)==0) return("If state['scored']==TRUE, 'geneAnno' must not be empty!")

  checkMandatoryColumns(object, "geneAnno", mandatory=c("Plate", "Well", "GeneID"),
               numeric=c("Plate"))
  }
  return(TRUE)
}


##-----------------------------------------------------------------------------

validityROC <- function(object) {

  if(!equalOrZero(length(object@TP), 1000) | !equalOrZero(length(object@FP), 1000)) return("'TP' and 'FP' should be vectors of integers with length 1000.")

  if(length(object@TP)!=length(object@FP)) return("'FP' and 'TP' should be vectors of integers with length 1000.")

  if(any(is.na(object@TP))) return("'TP' must not contain NA values.")

  if(any(is.na(object@FP))) return("'FP' must not contain NA values.")

  if(is.na(object@posNames)) return("'posNames' must not contain NA values.")
  if(is.na(object@negNames)) return("'negNames' must not contain NA values.")

  if(!(equalOrZero(length(object@assayType), 1))) return("'assayType' should be a character vector of length 1.")
  if(length(object@assayType)!=0) {
     if(!(object@assayType %in% c("two-way assay", "one-way assay"))) return("'assayType' should be one of the two options: 'one-way assay' or 'two-way assay'.") }

  return(TRUE)
}

##------------------------------------------------------------------------------
## Class cellHTS
##------------------------------------------------------------------------------
setClass("cellHTS",
  representation(
    name = "character", 
    pdim = "integer",
    batch = "integer",
    xraw = "array",
    xnorm = "array",
    plateList = "data.frame",
    intensityFiles = "list",
    state = "logical",
    plateConf = "data.frame",
    screenLog = "data.frame",
    screenDesc = "character",
    wellAnno = "factor",
    geneAnno = "data.frame",
    score = "numeric",
    rowcol.effects = "array",
    overall.effects = "array"
   ),

prototype = list(
    name = "", 
    pdim = c(nrow=integer(1), ncol=integer(1)),
    batch = integer(),
    xraw = array(dim=c(0,0,0,0)),
    xnorm = array(dim=c(0,0,0,0)),
    plateList = data.frame(),
    intensityFiles = list(),
    state = c("configured"=FALSE, "normalized"=FALSE, "scored"=FALSE, 
      "annotated" = FALSE),
    plateConf = data.frame(),
    screenLog = data.frame(),
    screenDesc = "",
    wellAnno = factor(integer()),
    geneAnno = data.frame(),
    score = numeric(),
    rowcol.effects = array(dim=c(0,0,0,0)),
    overall.effects = array(dim=c(1, 0,0,0))
    ),
  validity = validityCellHTS)



##------------------------------------------------------------------------------
## Class ROC
##------------------------------------------------------------------------------

setClass("ROC",
  representation(
     name = "character",  
     assayType = "character",
     TP = "integer",
     FP = "integer",
     #positives = positives,
     #negatives = negatives, 
     posNames = "character",
     negNames = "character"),
  prototype = list(
     name = "",
     assayType = character(),
     TP = integer(),
     FP = integer(),
     posNames = character(),
     negNames = character()
  ),
  validity = validityROC)








