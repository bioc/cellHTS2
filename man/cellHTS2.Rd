\name{cellHTS2}
\alias{cellHTS2}
\alias{cellHTS2-package}
\docType{package}
\title{cellHTS2 Package Overview}
\description{
   cellHTS2 Package Overview
}
\details{
  This package provides data structures and algorithms for
  cell-based high-throughput assays performed in plate
  format. Typical applications are RNA interference
  or small molecular compound screens.
  The most important data class is \code{\linkS4class{cellHTS}},
  which extends the \code{\link[Biobase:class.NChannelSet]{NChannelSet}} class.

Full help on methods and associated functions is available from within
class help pages.

Data sets: \code{\link{KcViab}}, \code{\link{KcViabSmall}}, 
\code{\link{oldKcViabSmall}}, \code{\link{dualCh}},
\code{\link{bdgpbiomart}}.

Introductory information is available from vignettes, type
\code{openVignette()}.

Class-specific methods: \code{\link{annotate}}, \code{\link{batch}},
\code{\link{batch<-}}, \code{\link{compare2cellHTS}}, \code{\link{configure}}, \code{\link{Data}},
\code{\link{Data<-}}, \code{\link{geneAnno}},
\code{\link{intensityFiles}}, \code{\link{name}}, \code{\link{name<-}},
\code{\link{nbatch}}, \code{\link{pdim}}, \code{\link{plate}},
\code{\link{plateConf}}, \code{\link{plateEffects}},
\code{\link{plateList}}, \code{\link{position}},
\code{\link{screenDesc}}, \code{\link{screenLog}}, \code{\link{state}},
\code{\link{well}}, \code{\link{wellAnno}}, \code{\link{writeTab}}, 
\code{\link{ROC}}.

Generic functions: \code{\link{show}}, \code{\link{initialize}},
\code{\link{validObject}}.

Other functions: \code{\link{oneRowPerId}}, \code{\link{write.tabdel}}, 
\code{\link{readPlateList}}, \code{\link{readHTAnalystData}}, \code{\link{normalizePlates}}, 
\code{\link{Bscore}}, \code{\link{spatialNormalization}}, \code{\link{plotSpatialEffects}}, 
\code{\link{summarizeChannels}}, \code{\link{scoreReplicates}}, \code{\link{summarizeReplicates}}, 
\code{\link{imageScreen}}, \code{\link{configurationAsScreenPlot}}, \code{\link{getEnVisionRawData}},
\code{\link{getEnVisionCrosstalkCorrectedData}},
\code{\link{getTopTable}}, \code{\link{getMeasureRepAgreement}}. 
\code{\link{getDynamicRange}}, \code{\link{getZfactor}},
\code{\link{writeReport}}, \code{\link{convertOldCellHTS}},
\code{\link{scores2calls}},
and \code{\link{templateDescriptionFile}}.


A full listing of documented topics is available in HTML view by
typing \code{help.start()} and selecting the \code{cellHTS2} package from the
\code{Packages} menu or via \code{library(help="cellHTS2")}.
}

\seealso{

  Below, we present a list of \sQuote{high level} functions in the
  celHTS2 package together with a brief description of what they do.

 
  \bold{Data import:}  

    \code{\link{readPlateList}} read a collection of plate reader data files.
 
    \code{\link{readHTAnalystData}} read input files from a HTanalyser plate reader containing data for a set of plate replicates.

     \code{\link{getEnVisionRawData}} this function can be used as an import function when calling \code{\link[cellHTS2:readPlateList]{readPlateList}} to read plate result files obtained from EnVision Plate Reader. 
     
     \code{\link{getEnVisionCrosstalkCorrectedData}} this function can be used as an import function when calling \code{\link[cellHTS2:readPlateList]{readPlateList}} to read plate result files obtained from EnVision Plate Reader. 

  \bold{Screen configuration and annotation:}

     \code{\link{annotate}} annotates the reagents (e.g. siRNAs,
     molecular compounds) of a \code{\linkS4class{cellHTS}} object.

     \code{\link{configure}} annotates the plates and the plate result
     files of a g\code{\linkS4class{cellHTS}} object.

  \bold{Accessors:}  

   \code{\link{batch}} accesses and replaces the \code{batch} slot of a cellHTS object.

   \code{\link{Data}} accesses and replaces the \code{assayData} slot of a cellHTS object. It returns a 3D array with  dimensions number of features (product between the number of wells per plates and the number of plates) x number of samples (or replicates) x number of channels. 

   \code{\link{geneAnno}} returns the reagent IDs used in the screen (i.e. the contents of \code{fData(object)[,"GeneID"]}.  

   \code{\link{intensityFiles}} returns a list, where each component contains a
    copy of the imported input data files.

   \code{\link{name}} obtains the name(s) of the assay, or multiple assays, stored in the \code{object}. This corresponds to the contents of column \code{assay} of the \code{phenoData} slot of the \code{cellHTS} object.

   \code{\link{pdim}} obtains the plate dimensions (number of rows and columns) for the data stored in \code{object}.

   \code{\link{plate}} plate identifier for each feature (well).

   \code{\link{plateConf}} returns a data.frame that contains what was read from the plate configuration input file (except the first two header rows) during the screen configuration step.

   \code{\link{plateList}}  returns a data.frame containing what was read from the plate list file, 
    plus a column \code{status} of type character that contains
    the string "OK" if the data import appeared to have gone well, and the respective error or warning
    message otherwise.

    \code{\link{plateEffects}} accesses the slots \code{rowcol.effects} and \code{overall.effects}.

    \code{\link{position}} gives the well number for each feature (well) within each plate.

    \code{\link{screenDesc}} returns an object of class \code{character} that contains what was read from the screen description input file during the configuration of the \code{\linkS4class{cellHTS}} object.

   \code{\link{screenLog}}  returns a data.frame containing what was read from the screen log input file during the screen configuration step.

   \code{\link{state}}   This generic function accesses the state of an object derived from the
  \code{\linkS4class{cellHTS}} class.

   \code{\link{well}} gives the alphanumeric identifier (e.g. A01, A02, ...) for each well and plate.

   \code{\link{wellAnno}} accesses the plate annotation stored in \code{fData(object)[,"controlStatus"]}. 



  \bold{Data preprocessing:}

  All the following methods work on the data stored in the
  slot \code{assayData} of a \code{\linkS4class{cellHTS}} object.
  
  \code{\link{normalizePlates}} per-plate data transformation, normalization and variance adjustment.
  
  \code{\link{Bscore}} correction of plate and spatial effects using the B score method (without variance adjustment of the residuals).

  \code{\link{spatialNormalization}} correction of spatial effects by fitting a polynomial surface within each plate using local regression (\code{\link[stats:loess]{loess}} or \code{\link[locfit:locfit]{robust local fit}}). Uses a second degree polynomial (local quadratic fit). Only wells containing "sample" are considered to fit the models.

  \code{\link{summarizeChannels}} combines intensities from a dual-channel assay by applying the function defined in \code{fun}.

  \code{\link{scoreReplicates}} transform per-replicate values into scores.
   
  \code{\link{summarizeReplicates}} summarizes between normalized and
  scored replicate values, obtaining a single value for each probe.

  \code{\link{scores2calls}} applies a sigmoidal transformation to the z-score values stored in a \code{cellHTS} object mapping them to the range [0,1].

   
  \bold{Miscellaneous:}

  \code{\link{compare2cellHTS}} compares two \code{\linkS4class{cellHTS}} objects to see whether they derive from the same initial \code{\linkS4class{cellHTS}} object.

  \code{\link{convertOldCellHTS}} converts an old S3 class \code{cellHTS} object obtained using \code{cellHTS} package 
to new S4 class \code{cellHTS} object(s) to use with package \code{cellHTS2}.


  \code{\link{convertWellCoordinates}} converts between different ways of specifying well coordinates within a plate.
For example, wells can be identified by an alphanumeric character (e.g. "B02" or c("B", "02")) or by an integer value (e.g. 26).

  \code{\link{ROC}} creates an object of class \code{\linkS4class{ROC}} from a scored \code{\linkS4class{cellHTS}} object which can be plotted as a ROC curve. 

  \code{\link{getTopTable}} generates the hit list from a scored \code{\linkS4class{cellHTS}} object and write it to a tab-delimited file.

  \code{\link{getMeasureRepAgreement}} calculates the agreement between plate replicates using raw data or normalized data stored in a \code{\linkS4class{cellHTS}} object.

  \code{\link{getDynamicRange}} calculates per-plate dynamic range of data stored in a \code{\linkS4class{cellHTS}} object.

  \code{\link{getZfactor}} calculates per-experiment Z'-factor of data stored in a \code{\linkS4class{cellHTS}} object.
  The Z'-factor is a measure that quantifies the separation between the distribution of positive and negative controls.

  \code{\link{plotSpatialEffects}} this function plots the per-plate row and column effects estimated by the B score method or by the spatial normalization.

  \code{\link{imageScreen}} creates an image plot that gives an overview of the whole set of score values stored 
   in a scored \code{cellHTS} object.

  \code{\link{writeReport}} creates a directory with HTML pages of linked tables and plots
  documenting the contents of the preprocessing of a \code{\linkS4class{cellHTS}} object.

  \code{\link{oneRowPerId}}  rearranges dataframe entries such that there is exactly one row
per ID.

   \code{\link{nbatch}} gives the total number of batches in a \code{\linkS4class{cellHTS}} object.

   \code{\link{templateDescriptionFile}} creates a template description file for an RNAi experiment with default entries compliant with MIAME class 
and with additional entries specific for a \code{\linkS4class{cellHTS}} object.

   \code{\link{writeTab}} this function is a wrapper for the function \code{\link{write.table}} to write the contents of \code{assayData} slot of a \code{\linkS4class{cellHTS}} object to a tab-delimited file. If the object is already annotated, the probe information (\code{fData(object)@GeneID}) is also added.

   \code{\link{write.tabdel}} a wrapper for the function \code{\link{write.table}} used to write data to a tab-delimited file.

   \code{\link{meanSdPlot}} method for \code{\link[vsn]{meanSdPlot}}
   (from the \code{vsn} package)
   to construct the standard deviation versus
   mean plot of data stored in a \code{cellHTS} object.
}

\author{Ligia P. Bras}

\references{
  Boutros, M., Bras, L.P. and Huber, W. (2006)
  Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\keyword{package}
