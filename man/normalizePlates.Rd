\name{normalizePlates}
\alias{normalizePlates}
\concept{normalization}

\title{Per-plate data transformation, normalization and variance adjustment}
\description{
  Plate-by-plate normalization of the raw data stored in slot \code{assayData} of a \code{\linkS4class{cellHTS}} object.
  Normalization is performed separately for each plate, replicate and channel.
  \code{Log2} data transformation can be performed and variance adjustment can be performed in different ways (none, per-plate, per-batch or per-experiment).
}
\usage{
normalizePlates(object,  scale="additive", log=FALSE, method="median", varianceAdjust="none", posControls, negControls,\ldots)
}
\arguments{
  \item{object}{a \code{\linkS4class{cellHTS}} object that has already been configured. See details.}
  \item{scale}{a character specifying the scale that the input data are
    considered to be on: "additive" scale (default) or
    "multiplicative". The interpretation of this terminology is that
    data on an additive scale will be normalised by subtraction of a correction
    offset, whereas data on a multiplicative scale are normalised by
    division through a correction factor.}
  \item{log}{logical. If \code{TRUE}, data will first be \code{\link{log2}} transformed. 
   If data are on an additive scale (i.e. if \code{scale} is
   \code{"additive"}), then \code{log} is only allowed to be \code{FALSE}. 
   The default is \code{log=FALSE}.}
   \item{method}{character specifying the normalization method to use
     for the per-plate normalization. Allowed values are \code{"median"}
     (the default), \code{"mean"}, \code{"shorth"}, \code{"POC"},
     \code{"NPI"}, \code{"negatives"}, \code{\link{Bscore}}
     and \code{\link[cellHTS2:spatialNormalization]{"locfit"}}. See details.}
  \item{varianceAdjust}{character specifying the
    variance adjustment to perform. 
    Allowed values are \code{"none"} (the default), code{"byPlate"},
    \code{"byBatch"} and \code{"byExperiment"}. See details.}
  \item{posControls}{a vector of regular expressions giving the name of the positive control(s). See details.}
  \item{negControls}{a vector of regular expressions giving the name of the negative control(s). See details.}
  \item{\ldots}{Further arguments that get passed on to the function
    implementing the normalization method 
    chosen by \code{method}. Currently, this is only used for
    \code{\link{Bscore}} and 
    \code{\link[cellHTS2:spatialNormalization]{locfit}}.} 
}

\details{
The function \code{normalizePlates} uses the content of the \code{assayData} slot of \code{object}.
For dual-channel data, a recommended workflow is (i) to correct for
plate effects using the \code{normalizePlates} function, (ii) combine
the two channels using the function \code{\link{summarizeChannels}}, and
(iii) finally, if necessary, normalize the summarized intensities
calling \code{normalizePlates} again. 

In this function, the normalization is performed in a plate-by-plate fashion, following this workflow:
\enumerate{
   \item Log transformation of the data (optional)
   \item Per-plate normalization 
   \item Variance adjustment of the plate intensity corrected data (optional)
}

The argument \code{scale} defines the scale of the data. If the data are on a multiplicative scale 
(\code{scale="multiplicative"}), the data can be \code{\link{log2}}
transformed by setting \code{log=TRUE}. This then changes the scale of the data to code{"additive"}.

In the next step of preprocessing, intensities are corrected in a
plate-by-plate basis using the chosen normalization method:
\itemize{
    \item If \code{method="median"}, plates effects are corrected by the
    median value across wells that are annotated as \code{sample} in
    \code{wellAnno(object)}, for each plate and replicate.
    \item If \code{method="mean"}, the average in the \code{sample}
    wells is used instead.
    \item If \code{method="shorth"}, the midpoint of the
    \code{\link[genefilter:shorth]{shorth}} 
    of the distribution of values in the wells annotated 
    as \code{sample} is used. 
    \item If \code{method="negatives"}, the median of the negative
    controls is used.
}
Depending on the scale of the data prior to normalization, the data are
divided by the above defined correction factors (scale:
\code{"multiplicative"}), or the value is subtracted (scale:
\code{"additive"}).

Further available normalization methods are:
\itemize{
  \item \code{method="POC"} (percent of control): for each plate and
  replicate, each measurement is divided by the average of the
  measurements on the plate positive controls, and multiplied by 100.

  \item \code{method="NPI"} (normalized percent inhibition): each
  measurement is subtracted from the average of the intensities on the
  plate positive controls, and this result is divided by the difference
  between the means of the measurements on the positive and the negative
  controls.
 
  \item \code{method="Bscore"}: for each plate and replicate, the
  \code{\link[cellHTS2:Bscore]{B-score method}}, which is based on a
  2-way median polish, is applied to remove row and column biases.
  
  \item \code{method="locfit"} (robust local fit regression): for each
  plate and replicate, spatial effects are removed by fitting a
  bivariate local polynomial regression
  (see \code{\link[cellHTS2:spatialNormalization]{spatialNormalization}}).
}

In the final preprocessing step, variance of plate-corrected intensities
can be adjusted as follows:

\itemize{
   \item \code{varianceAdjust="byPlate"}: per plate normalized intensities are divided by the per-plate median absolute deviations (MAD) in "sample" wells. This is done separately for each replicate and channel;
   \item \code{varianceAdjust="byBatch"}: using the content of slot \code{batch}, plates are split according to assay batches and the individual normalized intensities in each group of plates (batch) are divided by the per-batch of plates MAD values (calculated based on "sample" wells). This is done separately for each replicate and channel;
   \item \code{varianceAdjust="byExperiment"}: each normalized measurement is divided by the overall MAD of normalized values in wells containing "sample". This is done separately for each replicate and channel;
}

By default, no variance adjustment is performed
(\code{varianceAdjust="none"}).

The arguments \code{posControls} and \code{negControls} are required for
applying the normalization methods based on the control measurements
that is, when \code{method="POC"}, or \code{method="NPI"}, or
\code{method="negatives"}).  \code{posControls} and \code{negControls}
should be vectors of regular expression patterns specifying the name of
the positive(s) and negative(s) controls, respectivey, as provided in
the plate configuration file (and accessed via
\code{wellAnno(object)}). The length of these vectors should be equal to
the current number of channels in \code{object} (i.e. to the
\code{dim(Data(object))[3]}).  By default, if \code{posControls} is not
given, \emph{pos} will be taken as the name for the wells containing
positive controls. Similarly, if \code{negControls} is missing, by
default \emph{neg} will be considered as the name used to annotate the
negative controls.  The content of \code{posControls} and
\code{negControls} will be passed to \code{\link[base:grep]{regexpr}}
for pattern matching within the well annotation given in the featureData
slot of \code{object} (which can be accessed via
\code{wellAnno(object)}) (see examples for
\code{\link[cellHTS2:summarizeChannels]{summarizeChannels}}). The
arguments \code{posControls} and \code{negControls} are particularly
useful in multi-channel data since the controls might be
reporter-specific, or after normalizing multi-channel data.

See the Examples section for an example on how this function can be used
to apply a robust version of the Z score method, whereby, for each plate and replicate,  the
per-plate median (computed only from sample wells) is subtracted from the
measurements, and the result is divided by the per-plate MAD
(only from sample wells).
}

\value{
  An object of class \code{\linkS4class{cellHTS}} with the normalized data
  stored in slot \code{assayData} (its previous contents were overridden).
  The processing status of the \code{object} is updated
  in the slot \code{state} to \code{object@state[["normalized"]]=TRUE}.

  Additional slots of \code{object} may be updated if
  \code{method="Bscore"} or \code{method="locfit"} are used.
  Please refer to the help page of
  the \code{\link[cellHTS2:Bscore]{Bscore}} function and
  \code{\link[cellHTS2:spatialNormalization]{spatialNormalization}} functions.
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
    x1 <- normalizePlates(KcViabSmall, scale="multiplicative", log=FALSE, method="median", varianceAdjust="none")
    # per-plate median subtraction of log2 transformed intensities  
    x2 <- normalizePlates(KcViabSmall, scale="multiplicative", log=TRUE, method="median", varianceAdjust="none")
    \dontrun{
    x3 <- normalizePlates(KcViabSmall, scale="multiplicative", log=TRUE, method="Bscore", varianceAdjust="none", save.model=TRUE)
    }


    ## robust Z score method (plate intensities are subtracted by the per-plate median on sample wells and divided by the per-plate MAD on sample wells):
    xZ <- normalizePlates(KcViabSmall, scale="additive", log=FALSE, method="median", varianceAdjust="byPlate")

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
