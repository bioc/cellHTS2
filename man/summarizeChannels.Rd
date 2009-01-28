\name{summarizeChannels}
\alias{summarizeChannels}
\concept{multi-channel screens}
\concept{normalization}
\title{Summarization of dual-channel data}
\description{
  Combines plate intensities (raw or already corrected in a per-plate fashion) from multi-channel data stored in slot \code{assayData} of a
  \code{\linkS4class{cellHTS}} instance by applying the function defined in \code{fun}.
}
\usage{
summarizeChannels(object,
    fun = function(r1, r2, thresh=-Inf) ifelse(r1>thresh, r2/r1, as.numeric(NA)))
}

\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}} that has been configured. See details.}
  \item{fun}{a user-defined function for the multi channel summarization.
    \code{fun} takes as many numeric vectors as there are channels,
  names \code{r1, r2, etc.}, and
  returns a single numeric vector of
    the same length. The default is to take the ratio between the
    second and first channels, with a threshold on \code{r1} shown above
    in the \emph{Usage} section that should be set by the user.}
}

\details{
For each plate and replicate of a multi-color experiment, the function
defined in \code{fun} is applied to relate the intensity values in the
respective channels of the \code{cellHTS} object. The default is to 
take the ratio between the second and first channels, with a threshold on 
\code{r1} (see the \emph{Usage} section). This threshold should be adjusted by the 
user according to the data. For an example, see the \emph{Examples} section.
This function uses the content of slot \code{assayData} of \code{object} and can be applied either to raw data or after per-plate correction of the intensity values in each channel using function \code{\link[cellHTS2:normalizePlates]{normalizePlates}}. This choice depends on channel summarization method that one intends to apply (i.e., the function given by argument \code{fun}).
}

\value{
An object of class \code{\linkS4class{cellHTS}} with the summarized multi-channel intensities stored in slot \code{assayData}.
This is an object of class \code{assayData} containing one matrix with the summarized channel data (dimensions nrFeatures x nrSamples).
}

\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:scoreReplicates]{scoreReplicates}},
  \code{\link[cellHTS2:summarizeReplicates]{summarizeReplicates}}.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\examples{
    data(dualCh)
    x <- dualCh
    table(wellAnno(x))

    ## Define the controls for the different channels:
    negControls=vector("character", length=dim(Data(x))[3])

    ## channel 1 - gene A
    ## case-insensitive and match the empty string at the beginning and end of a line (to distinguish between "geneA" and "geneAB", for example, although this is not a problem for the well annotation in this example)

    negControls[1]= "(?i)^geneA$"  
    ## channel 2 - gene A and geneB
    negControls[2]= "(?i)^geneA$|^geneB$" 
    posControls = vector("character", length=dim(Data(x))[3])
    ## channel 1 - no controls
    ## channel 2 - geneC and geneD
    posControls[2]="(?i)^geneC$|^geneD$"
 \dontrun{
    writeReport(cellHTSlist=list("raw"=x), map=TRUE, plotPlateArgs=TRUE, posControls=posControls, negControls=negControls)
 }
    ## In this example, we first normalize each channel separately by 
    ## plate median scaling (no variance adjustment), since we need to make the measurements 
    ## comparable across plates for the next step of channel summarization:
        xn = normalizePlates(x, scale="multiplicative", log=FALSE, method="median", varianceAdjust="none") 
    ## Then, we define a low intensity threshold for the measurements in the constitutive channel R1, 
    ## which will be set to the 5% quantile of the overall plate median corrected intensities in R1, 
    ## and take the ratio R2/R1.
        xn = summarizeChannels(xn, fun = function(r1, r2, 
             thresh=quantile(r1, probs=0.05, na.rm=TRUE)) ifelse(r1>thresh, r2/r1, as.numeric(NA))) 
    ## After channel summarization, we take the log2 and apply plate median normalization, 
    ## and opt to not adjust the variance:
    xn = normalizePlates(xn, scale="multiplicative", log=TRUE, method="median", varianceAdjust="none") 
    ## Define the controls for the normalized and summarized intensities (only one channel):
    negControls = vector("character", length=dim(Data(xn))[3])
    ## For the single channel, the negative controls are geneA and geneB 
    negControls[1]= "(?i)^geneA$|^geneB$" 
    posControls = vector("character", length=dim(Data(xn))[3])
    ## For the single channel, the negative controls are geneC and geneD 
    posControls[1]="(?i)^geneC$|^geneD$"
 \dontrun{
    writeReport(cellHTSlist=list("raw"=x, "normalized"=xn), force=TRUE, map=TRUE, plotPlateArgs=list(xrange=c(-3,3)), 
         posControls=posControls, negControls=negControls)
 }

    ## Another option could be to just take the log2 of the ratio between R2 and R1 raw intensities:
    xn1 = summarizeChannels(x, fun = function(r1, r2) log2(r2/r1)) 

}

\keyword{manip}
