## Validity functions and class definitions.
##------------------------------------------------------------------------------
## Validity functions
##------------------------------------------------------------------------------
## Pretty self-explainatory...
equalOrZero <- function(i, j) ((i==j)||(i==0))



## A helper function to check the columns of a data frame for their mode
checkMandatoryColumns <- function(object, name, mandatory, numeric=NULL, factor=NULL,
                                  character=NULL)
{
    obj <- slot(object, name)
    objcolnames <- varLabels(obj)
    missingColumns <- setdiff(mandatory, objcolnames)
    if(length(missingColumns)>0 | !length(objcolnames))
        return(sprintf("Column%s %s %s missing from slot %s",
                       ifelse(length(missingColumns)>1, "s ",""),
                       paste(missingColumns, collapse=", "),
                       ifelse(length(missingColumns)>1, " are"," is"),
                       name))

    for(j in intersect(numeric, objcolnames))
        if(!is.numeric(obj[[j]]) || any(is.na(obj[[j]])))
            return(sprintf("Column %s in '%s' must be numeric and not contain missing values.",
                           j, name))

    for(j in intersect(factor, objcolnames))
        if(!is.factor(obj[[j]]) || any(is.na(obj[[j]])))
            return(sprintf(paste("Column %s in '%s' must be a vector of factors and not",
                                 "contain missing values."), j, name))
    
    for(j in intersect(character, objcolnames))
        if(!is.character(obj[[j]]) || any(is.na(obj[[j]])))
            return(sprintf(paste("Column '%s' in '%s' must be a vector of characters",
                                 "and not contain missing values."), j, name))
}



## The main cellHTS object validity function
validityCellHTS <- function(object)
{
    if (!is(object, "cellHTS"))
        return(paste("cannot validate object of class", class(object)))
    msg <- NULL
    if(length(assayData(object))>0L)
    {
        msg <- checkMandatoryColumns(object, "phenoData", mandatory=c("replicate", "assay"),
                                     numeric=c("replicate"), character="assay")
        msg <- append(msg, checkMandatoryColumns(object, "featureData",
                                                 mandatory=c("plate", "well", "controlStatus"),
                                                 numeric=c("plate"), factor="controlStatus",
                                                 character="well"))
        ## add test to see whether column 'well' has the alphanumeric format (e.g. "A02")??
        ch <- assayDataElementNames(object)
        if(length(object@batch)>0L)
        {
            if(!is.integer(object@batch)) 
                msg <- append(msg, paste("'batch' must be an array of integer values",
                                         "corresponding to the batch number for each plate,",
                                         "sample and channel!"))
            if(any(dim(object@batch)!=c(dim(object), length(ch))))
                msg <- append(msg, sprintf(paste("'batch' should have dimensions",
                                                 "'Features x Samples x Channels' (%s)."),
                                           paste(c(dim(object), length(ch)), collapse=" x "))) 
        }
    }
    if(!((length(object@state)==4L)&&
         (identical(names(object@state),
                    c("configured", "normalized", "scored", "annotated")))))
        msg <- append(msg, paste("'state' must be of length 4 and have names",
                                 "'configured', 'normalized', 'scored', 'annotated'"))
    if(is.null(msg))
        msg <- TRUE 
    return(msg)
}



## Main validy function for class "ROC"
validityROC <- function(object)
{
    if(!equalOrZero(length(object@TP), 1000L) ||
       !equalOrZero(length(object@FP), 1000L) ||
       (length(object@TP)!=length(object@FP)))
        return("'TP' and 'FP' should be vectors of integers with length 1000.")
    if(any(is.na(object@TP)))
        return("'TP' must not contain NA values.")
    if(any(is.na(object@FP)))
        return("'FP' must not contain NA values.")
    if(is.na(object@posNames))
        return("'posNames' must not contain NA values.")
    if(is.na(object@negNames))
      return("'negNames' must not contain NA values.")
    if(!(equalOrZero(length(object@assayType), 1)))
        return("'assayType' should be a character vector of length 1.")
    if(length(object@assayType)!=0)
    {
        if(!(object@assayType %in% c("two-way assay", "one-way assay")))
            return(paste("'assayType' should be one of the two options:",
                         "'one-way assay' or 'two-way assay'."))
    }
    return(TRUE)
}



##------------------------------------------------------------------------------
## Class cellHTS (inherits from Biobase class 'NChannelSet'
##------------------------------------------------------------------------------
setClass("cellHTS",  contains="NChannelSet",
 representation(plateList="data.frameOrNULL",
                intensityFiles="list",
                state="logical",
                plateConf="data.frameOrNULL",
                screenLog="data.frameOrNULL",
                screenDesc="character",
                batch="array",
                rowcol.effects="array",
                overall.effects="array",
                plateData="list"),
         prototype=prototype(new("VersionedBiobase",
         versions=c(classVersion("NChannelSet"), cellHTS="1.0.0")),
         plateList=data.frame(),
         intensityFiles=list(),
         state=c("configured"=FALSE, "normalized"=FALSE, "scored"=FALSE, 
         "annotated"=FALSE),
         plateConf=data.frame(),
         screenLog=data.frame(),
         screenDesc="",
         batch=array(dim=c(0,0,0)), 
         rowcol.effects=array(dim=c(0,0,0)),
         overall.effects=array(dim=c(0,0,0)),
         plateData=list(batch=data.frame())
    ),
  validity=validityCellHTS
) 



##------------------------------------------------------------------------------
## Class ROC
##------------------------------------------------------------------------------
setClass("ROC",
         representation(name="character",  
                        assayType="character",
                        TP="integer",
                        FP="integer",
                        posNames="character",
                        negNames="character"),
         prototype=list(
         name="",
         assayType=character(),
         TP=integer(),
         FP=integer(),
         posNames=character(),
         negNames=character()
         ),
         validity=validityROC)




