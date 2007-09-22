\name{normalizePlates}
\alias{normalizePlates}
\alias{plate effects}
%\alias{normalizePlates,cellHTS-method}

\concept{normalization}

\title{Per-plate data transformation, normalization and variance adjustment}
\description{
  Plate-by-plate normalization of the raw data stored in slot \code{assayData} of a \code{\linkS4class{cellHTS}} object.
  Normalization is performed separately for each plate, replicate and channel.
  \code{Log2} data transformation can be performed, and variance adjustment can be performed in different ways (per-plate, per-batch or per-experiment).
}
\usage{
normalizePlates(object,  scale="additive", log=FALSE, method="median", varianceAdjust="byExperiment", posControls, negControls,\ldots)
}
\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already been configured. See details.}
  \item{scale}{a character specifying the scale of the raw data: "additive" scale (default) or "multiplicative" scale.}
  \item{log}{logical. If \code{log=TRUE}, raw data are \code{\link{log2}} transformed. 
   If data are in additive scale (\code{scale="additive"}), \code{log} can only be set to \code{log=FALSE}. 
   The default is \code{log=FALSE}.}
  \item{method}{a character specifying the normalization method to use for performing the per-plate normalization. Allowed   values are \code{"median"} (default), \code{"mean"}, \code{"shorth"}, \code{"POC"}, \code{"NPI"}, \code{"negatives"}, 
  \code{\link{Bscore}}, \code{\link[cellHTS2:spatialNormalization]{loess}} and \code{\link[cellHTS2:spatialNormalization]{locfit}}. See details.}
  \item{varianceAdjust}{character vector of length one indicating the variance adjustment to perform. Allowed values are "byPlate", "byBatch" and "byExperiment" (default). See details.}
  \item{posControls}{a vector of regular expressions giving the name of the positive control(s). See details.}
  \item{negControls}{a vector of regular expressions giving the name of the negative control(s). See details.}
  \item{\ldots}{Further arguments that get passed on to the function implementing the normalization method chosen by \code{method}. Currently, this is only used for \code{\link{Bscore}} and \code{\link[cellHTS2:spatialNormalization]{loess}} and \code{\link[cellHTS2:spatialNormalization]{locfit}}.} 
}

\details{

Function \code{normalizePlates} uses the content of \code{assayData} slot of \code{object}.
For dual-channel data, the user should first correct for plate effects using \code{normalizePlates} function, then combine the two channels using function \code{\link{summarizeChannels}}, and finally, if necessary, normalize the summarized intensities calling \code{normalizePlates} again. 

In this function, the normalization is performed in a plate-by-plate fashion, following this workflow:
\enumerate{
   \item Log transformation of the data (optional, if data are in multiplicative scale);
   \item Per-plate normalization using the chosen method;
   \item Variance adjustment of the plate intensity corrected data.
}

The argument \code{scale} defines the scale of the data. If data are in multiplicative scale 
(\code{scale="multiplicative"}), data can be \code{\link{log2}} transformed by setting \code{log=TRUE}. This changes the scale of the data to "additive".

In the next step of preprocessing, intensities are corrected in a plate-by-plate basis using the chosen normalization method:

 \itemize{
    \item If \code{method="median"} (median scaling), plates effects are corrected  by dividing each measurement by the median value across wells annotated as \code{sample} in \code{wellAnno(object)}, for each plate and replicate.
    \item If \code{method="mean"} (mean scaling), the average in the \code{sample} wells is consider instead.
    \item If \code{method="shorth"} (scaling by the midpoint of the shorth), for each plate and replicate, the midpoint of the \code{\link[genefilter:shorth]{shorth}} of the distribution of values in the wells annotated 
           as \code{sample} is calculated. Then, every measurement is divided by this value.
    \item If \code{method="negatives"} (scaling by the negative controls), for each plate and replicate, each measurement is divided by the median of the measurements on the plate negative controls.
}

NOTE: Depending on the scale of the data prior to normalization, the above per-plate correction factors are subtracted from each plate measurement, instead.

Other available normalization methods are:
\itemize{
    \item \code{method="POC"} (percent of control): for each plate and replicate, each measurement is divided by the average of the measurements on the plate positive controls, and multiplied by 100.

    \item \code{method="NPI"} (normalized percent inhibition): each measurement is subtracted from the average of the intensities on the plate positive controls, and this result is divided by the difference between 
          the means of the measurements on the positive and the negative controls.
    \item \code{method="Bscore"} (B score): for each plate and replicate, the \code{\link[cellHTS2:Bscore]{B score method}} (based on a 2-way median polish) is applied to remove plate effects and row and column biases.
    \item \code{method="locfit"} (robust local fit regression): for each plate and replicate, spatial effects are removed by fitting a bivariate local regression (see \code{\link[cellHTS2:spatialNormalization]{spatial normalization function}}).
    \item \code{method="loess"} (loess regression): for each plate and replicate, spatial effects are removed by fitting a loess curve (see \code{\link[cellHTS2:spatialNormalization]{spatial normalization function}}).
}

In the final preprocessing step, variance of plate-corrected intensities can be adjusted as follows:

\itemize{
   \item \code{varianceAdjust="byPlate"}: per plate normalized intensities are divided by the per-plate median absolute deviations (MAD) in "sample" wells. This is done separately for each replicate and channel;
   \item \code{varianceAdjust="byBatch"}: using the content of slot \code{batch}, plates are split according to assay batches and the individual normalized intensities in each group of plates (batch) are divided by the per-batch of plates MAD values (calculated based on "sample" wells). This is done separately for each replicate and channel;
   \item \code{varianceAdjust="byExperiment"}: each normalized measurement is divided by the overall MAD of normalized values in wells containing "sample". This is done separately for each replicate and channel;
}

  The arguments \code{posControls} and \code{negControls} are required for applying the normalization methods based on the control measurements (that is, when \code{method="POC"}, or \code{method="NPI"}, or \code{method="negatives"}). 
  \code{posControls} and \code{negControls} should be given as a vector of regular expression patterns specifying the name of the positive(s) and negative(s) controls, respectivey, as provided in the plate configuration file (and accessed via \code{wellAnno(object)}). The length of these vectors should be equal to the current number of channels in \code{object} (i.e. to the \code{dim(Data(object))[3]}).
  By default, if \code{posControls} is not given, \emph{pos} will be taken as the name for the wells containing positive controls. Similarly, if \code{negControls} is missing, by default \emph{neg} will be considered as the name used to annotate the negative controls. 
  The content of \code{posControls} and \code{negControls} will be passed to \code{\link[base:regexpr]{regexpr}} for pattern matching within the well annotation given in the featureData slot of \code{object} (which can be accessed via \code{wellAnno(object)}) (see examples for \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}}). The arguments \code{posControls} and \code{negControls} are particularly useful in
multi-channel data since the controls might be reporter-specific, or
after normalizing multi-channel data.
}

\value{
An object of class \code{\linkS4class{cellHTS}} with the normalized data stored in slot \code{assayData} (its previous contents were overridden).
The processing status of the \code{object} is updated
in the slot \code{state} to \code{object@state[["normalized"]]=TRUE}.

Additional slots of \code{object} may be updated if
\code{method="Bscore"}, or \code{method="loess"} or \code{method="locfit"}. Please refer to the help page of
the \code{\link[cellHTS2:Bscore]{Bscore}} function and \code{\link[cellHTS2:spatialNormalization]{spatialNormalization}} function.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\seealso{
  \code{\link[cellHTS2:Bscore]{Bscore}}, 
  \code{\link[cellHTS2:spatialNormalization]{spatialNormalization}},
  \code{\link[cellHTS2:summarizeChannels]{summarizeChannels}}
}

\examples{
    data(KcViabSmall)
    # per-plate median scaling of intensities
    x1 <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="byExperiment")
    # per-plate median subtraction of log2 transformed intensities  
    x2 <- normalizePlates(KcViabSmall, scale="multiplicative", log=TRUE, method="median", varianceAdjust="byExperiment")
    \dontrun{
    x3 <- normalizePlates(KcViabSmall, scale="multiplicative", log=TRUE, method="Bscore", varianceAdjust="byExperiment", save.model=TRUE)
    }

    ## an example to illustrate the use of slot 'batch':
   \dontrun{
   try(xnorm <- normalizePlates(KcViabSmall, scale="multiplicative", method="median", varianceAdjust="byBatch"))
   
   # It doesn't work because we need to have slot 'batch'!
   # For example, we will suppose that a different lot of reagents was used for plate 1:
   pp <- plate(KcViabSmall)
   fData(KcViabSmall)$"reagent" <- "lot B"
   fData(KcViabSmall)$"reagent"[pp==1] <- "lot A"
   fvarMetadata(KcViabSmall)["reagent",] <- "Lot of reagent used"

   bb <- as.factor(fData(KcViabSmall)$"reagent")
   batch(KcViabSmall) <- array(as.integer(bb), dim=dim(Data(KcViabSmall)))
   ## check number of batches:
   nbatch(KcViabSmall)
   x1 <- normalizePlates(KcViabSmall, scale="multiplicative", log = FALSE, method="median", varianceAdjust="byBatch")
}
}
\keyword{manip}
