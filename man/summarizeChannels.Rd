\name{summarizeChannels}
\alias{summarizeChannels}
\concept{multi-channel screens}
\concept{normalization}
\title{Summarization of dual-channel data}
\description{
  Combines plate-corrected intensities from dual-channel data stored in slot \code{assayData} of a
  \code{\linkS4class{cellHTS}} instance by applying the function defined in \code{fun}.
}
\usage{
summarizeChannels(object,
    fun = function(r1, r2, thresh) ifelse(r1>thresh, r2/r1, as.numeric(NA)))
}

\arguments{
  \item{object}{an object of class \code{\linkS4class{cellHTS}} that has been configured 
                and corrected for plate effects. See details.}
  \item{fun}{a user-defined function for the two channel summarization.
    \code{fun} takes two numeric vectors and returns a numeric vector of
    the same length. The default is to take the ratio between the
    second and first channels, with a threshold on \code{r1} shown above
    in the \emph{Usage} section that should be set by the user.}
}

\details{
For each plate and replicate of a two-color experiment, the function
defined in \code{fun} is applied to relate the intensity values in the
two channels of the \code{cellHTS} object. The default is to 
take the ratio between the second and first channels, with a threshold on 
\code{r1} (see the \emph{Usage} section). This threshold should be ajusted by the 
user according to the data. For an example, see the \emph{Examples} section.
This function uses the content of slot \code{assayData} of \code{object}, and should be applied after per-plate correction of the intensity values in each channel using function \code{\link[cellHTS2:normalizePlates]{normalizePlates}}.
}

\value{
An object of class \code{\linkS4class{cellHTS}} with the summarized dual-channel intensities stored in slot \code{assayData}.
This is an object of class \code{assayData} containing one matrix with the summarized channel data (dimensions nrFeatures x nrSamples).
}

\seealso{
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}},
  \code{\link[cellHTS2:summarizeReplicates]{summarizeReplicates}}.
}

\author{Ligia Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}


\examples{

    datadir <- system.file("DualChannelScreen", package = "cellHTS2")
    x <- readPlateList("Platelist.txt", name="TwoColorData", path=datadir)
    x <- configure(x, "Description.txt", "Plateconf.txt", "Screenlog.txt", path=datadir)
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
    #writeReport(x, map=TRUE, plotPlateArgs=TRUE, posControls=posControls, negControls=negControls)
 }
    ## In this example, we first normalize each channel separately by 
    ## plate median scaling and variance adjustment.
        x = normalizePlates(x, scale="multiplicative", log=FALSE, method="median", varianceAdjust="byExperiment") 
    ## Then, we define a low intensity threshold for the measurements in the constitutive channel R1, 
    ## which will be set to the 5% quantile of the overall plate median corrected intensities in R1, 
    ## and take the ratio R2/R1.
    x = summarizeChannels(x, fun = function(r1, r2, 
             thresh=quantile(r1, probs=0.05, na.rm=TRUE)) ifelse(r1>thresh, r2/r1, as.numeric(NA))) 
    ## After channel summarization, we take the log2 and re-apply plate median scaling, 
    ## and readjust the variance of each replicate experiment.
    x = normalizePlates(x, scale="multiplicative", log=TRUE, method="median", varianceAdjust="byExperiment") 
    ## Define the controls for the normalized and summarized intensities (only one channel):
    negControls = vector("character", length=dim(Data(x))[3])
    ## For the single channel, the negative controls are geneA and geneB 
    negControls[1]= "(?i)^geneA$|^geneB$" 
    posControls = vector("character", length=dim(Data(x))[3])
    ## For the single channel, the negative controls are geneC and geneD 
    posControls[1]="(?i)^geneC$|^geneD$"
 \dontrun{
   # writeReport(x, force=TRUE, map=TRUE, plotPlateArgs=list(xrange=c(-3,3)), 
   #      posControls=posControls, negControls=negControls)
 }
}

\keyword{manip}
