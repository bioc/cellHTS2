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
checkMandatoryColumns = function(object, name, mandatory, numeric=NULL, factor=NULL, character=NULL) {

  obj <- slot(object, name)
  objcolnames <- varLabels(obj)


  missingColumns = setdiff(mandatory, objcolnames)
  if(length(missingColumns)>0 | !length(objcolnames))
    return(sprintf("Column%s %s %s missing from slot %s", ifelse(length(missingColumns)>1, "s ",""),
               paste(missingColumns, collapse=", "),
               ifelse(length(missingColumns)>1, " are"," is"),
               name))

  for(j in intersect(numeric, objcolnames))
    if(!is.numeric(obj[[j]]) || any(is.na(obj[[j]])))
      return(sprintf("Column %s in '%s' must be numeric and not contain missing values.",
                   j, name))

  for(j in intersect(factor, objcolnames))
    if(!is.factor(obj[[j]]) || any(is.na(obj[[j]])))
      return(sprintf("Column %s in '%s' must a vector of factors and not contain missing values.",
                   j, name))

  for(j in intersect(character, objcolnames))
    if(!is.character(obj[[j]]) || any(is.na(obj[[j]])))
      return(sprintf("Column '%s' in '%s' must be a vector of characters and not contain missing values.",
                   j, name))
  }

#--------------------------------------------------------------------------------

validityCellHTS = function(object){

    if (!is(object, "cellHTS"))
      return(paste("cannot validate object of class", class(object)))

     msg <-NULL
      if(length(assayData(object))) {
        msg <- checkMandatoryColumns(object, "phenoData", mandatory=c("replicate", "assay"),
               numeric=c("replicate"), character="assay")

        msg <- append(msg, checkMandatoryColumns(object, "featureData", mandatory=c("plate", "well", "controlStatus"),
               numeric=c("plate"), factor="controlStatus", character="well"))

        ## add test to see whether column 'well' has the alphanumeric format (e.g. "A02")??

        ch <- assayDataElementNames(object)


        if(length(object@batch)) {
           if(!is.integer(object@batch)) 
               msg = append(msg, "'batch' should be an array of integer values corresponding to the batch number for each plate, sample and channel!")
           if(any(dim(object@batch)!=c(dim(object), length(ch)))) msg=append(msg, sprintf("'batch' should have dimensions 'Features x Samples x Channels' (%s).", paste(c(dim(object), length(ch)), collapse=" x "))) 

        }
       }
    if(is.null(msg)) msg <- TRUE 
    return(msg)
  }


##-----------------------------------------------------------------------------



##------------------------------------------------------------------------------
## Class cellHTS (inherits from Biobase class 'NChannelSet'
##------------------------------------------------------------------------------

setClass("cellHTS",  contains = "NChannelSet",
 representation( 
    plateList = "data.frameOrNULL",
    intensityFiles = "list",
    state = "logical",
    plateConf = "data.frameOrNULL",
    screenLog = "data.frameOrNULL",
    screenDesc = "character",
    batch = "array",
    rowcol.effects = "array",
    overall.effects = "array"),

         prototype = prototype(
           new("VersionedBiobase",
               versions=c(classVersion("NChannelSet"), cellHTS="1.0.0")),
    plateList = data.frame(),
    intensityFiles = list(),
    state = c("configured"=FALSE, "normalized"=FALSE, "scored"=FALSE, 
      "annotated" = FALSE),
    plateConf = data.frame(),
    screenLog = data.frame(),
    screenDesc = "",
    batch = array(dim=c(0,0,0)), 
    #rowcol.effects = array(dim=c(0,0,0,0)),
    #overall.effects = array(dim=c(1,0,0,0)) ),
    rowcol.effects = array(dim=c(0,0,0)),
    overall.effects = array(dim=c(0,0,0))
    ),
  validity=validityCellHTS
) 







