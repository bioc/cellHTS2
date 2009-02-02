#------------------------------------------------------------
# methods related to the class 'ROC'
#------------------------------------------------------------

##----------------------------------------
## show
##----------------------------------------

setMethod("show",
          signature("ROC"),
  function(object)
      {
    if(any(!as.logical(c(length(object@FP), length(object@TP),
                         length(object@posNames), length(object@negNames),
                         length(object@assayType)))))
    {
        cat(sprintf("Incomplete %s object. \n", class(object)))
    }
    else
    {
        cat(class(object), sprintf("object derived from the '%s' cellHTS object called '%s'.\n",
                                   object@assayType, object@name))
        cat(sprintf("Positive control%s: %s. \nNegative control%s: %s\n",
                    ifelse(length(object@posNames)>1, "s", ""),
                    paste(sprintf("'%s'", object@posNames), collapse=", "),
                    ifelse(length(object@negNames)>1, "s", ""),
                    paste(sprintf("'%s'", object@negNames), collapse=", ")))
    }
})


##----------------------------------------
## plot
##----------------------------------------

setMethod("plot",
  signature(x="ROC", y="missing"),
  definition=function(x, col="darkblue", type="l", main="ROC curve", ...)
      {
          if((length(x@TP) + length(x@FP))!=0 &
             !all(as.logical(c(length(x@negNames), length(x@posNames),
                               length(x@assayType)))))
              stop("Please complete the 'ROC' object!")

          xinfo <- if (length(x@negNames) > 1) paste(x@negNames, collapse=", ") else x@negNames
          yinfo <- if (length(x@posNames) > 1) paste(x@posNames, collapse=", ") else x@posNames

          plot(x@FP, x@TP, xlab="", ylab="", col=col, type=type, ...)
          mtext(main, side = 3, line = 2, font=2)
          mtext(x@assayType, side = 3, line = 1)
          mtext("#FP", side = 1, line = 2)
          mtext(xinfo, side = 1, line=3, font=3)
          mtext("#TP", side = 2, line=3)
          mtext(yinfo, side = 2, line=2, font=3)
      })

##----------------------------------------
## lines
##----------------------------------------

setMethod("lines", signature(x="ROC"), 
          function(x, ...)
          lines(x@FP, x@TP, ...)
          )
