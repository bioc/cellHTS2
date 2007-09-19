\name{cellHTS2}
\alias{cellHTS2}
\alias{cellHTS2-package}
\docType{package}
\title{cellHTS2 Package Overview}
\description{
   cellHTS2 Package Overview
}
\details{
Important data class: \code{\linkS4class{cellHTS}}. 
This class is a container for cell-based high-throughput RNA
interference assays performed in plate format and it extends the
\code{\link[Biobase:class.NChannelSet]{NChannelSet}} class.

Full help on methods and associated functions is available from within
class help pages.

Data sets: \code{\link{KcViab}}, \code{\link{KcViabSmall}}.

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
\code{\link{well}}, \code{\link{wellAnno}}, \code{\link{writeTab}}.

Generic functions: \code{\link{show}}, \code{\link{initialize}},
\code{\link{validObject}}.

Other functions: \code{\link{oneRowPerId}}, \code{\link{write.tabdel}}, 
\code{\link{readPlateList}}, \code{\link{readHTAnalystData}}, \code{\link{normalizePlates}}, 
\code{\link{Bscore}}, \code{\link{spatialNormalization}}, %\code{\link{plotSpatialEffects}}, 
\code{\link{summarizeChannels}}, \code{\link{summarizeReplicates}}, 
%\code{\link{imageScreen}}, 
\code{\link{getTopTable}}, \code{\link{getMeasureRepAgreement}}. 
\code{\link{getDynamicRange}}, \code{\link{getZfactor}},
%\code{\link{writeReport}}, 
and \code{\link{ROC}}.

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

  \bold{Accessors:}  

 \code{\link{plate}} plate identifier for each feature (well).

  \code{\link{plateEffects}} accesses the slots \code{rowcol.effects} and \code{overall.effects}.


  \bold{Data preprocessing:}

  \code{\link{normalizePlates}} --- complete ----
  
   
  \bold{Miscellaneous:}

  \code{\link{compare2cellHTS}} compares 2 \code{\linkS4class{cellHTS}} objects to see whether they derive from the same initial \code{\linkS4class{cellHTS}} object.

  \code{\link{ROC}} creates an object of class \code{\linkS4class{ROC}} from a scored \code{\linkS4class{cellHTS}} object which can be plotted as a ROC curve. 

  \code{\link{getTopTable}} generates the hit list from a scored {\linkS4class{cellHTS}} object and write it to a tab-delimited file.

  \code{\link{getMeasureRepAgreement}} calculates the agreement between plate replicates using raw data or normalized data stored in a 
  \code{\linkS4class{cellHTS}} object.

  \code{\link{getDynamicRange}} calculates per-plate dynamic range of data stored in a \code{\linkS4class{cellHTS}} object.

  \code{\link{getZfactor}} calculates per-experiment Z'-factor of data stored in a \code{\linkS4class{cellHTS}} object.
  The Z'-factor is a measure that quantifies the separation between the distribution of positive and negative controls.

%  \code{\link{imageScreen}}  --- complete ----

%  \code{\link{...}}  --- complete ----

%  \code{\link{...}}  --- complete ----

%  \code{\link{...}}  --- complete ----

}

\author{Ligia P. Bras \email{ligia@ebi.ac.uk}}

\references{
Boutros, M., Bras, L.P. and Huber, W. (2006) Analysis of cell-based RNAi screens, \emph{Genome Biology} \bold{7}, R66.
}

\keyword{package}
