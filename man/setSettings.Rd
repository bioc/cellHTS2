\name{settings}

\alias{settings}
\alias{getSettings}
\alias{setSettings}

\title{
  
cellHTS2 HTML report settings

}

\description{
  
Functions to control the output of \code{\link{writeReport}} through
session-wide or call-specific settings.

}

\usage{

setSettings(x)
getSettings()

}

\arguments{
  
  \item{x}{A named list of settings. See details for supported values.}

}

\details{
  
   The \code{\link{writeReport}} function produces a complete audit
   trail of the analysis in the form of an HTML report. The content of
   this report is highly customizable though session-wide and also
   though call-specific settings. The former are supposed to be set using
   the \code{setSettings} function by providing a named nested list of
   values. The latter can be set by passing a similiar list on to
   \code{writeHTML} as the optional \code{settings} argument. The
   current values for all available settings can be queried using the
   \code{getSettings} function.

   Similar to the structure of the HTML report, the available settings
   are broken up into subsections, and the names of these subsection
   have to be matched by the names in the nestes list structure:

   \describe{

     \item{plateList}{The settings for all the plots in the plate list
       section of the report. There are several sub-section:

       \describe{
	 
	 \item{correlation}{The settings for the correlation plots:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 7.5.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 14.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.5.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 12.}
	     
	   }
	 }

	 \item{maplot}{The settings for the correlation plots:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 7.5.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 14.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.5.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 12.}
	     
	   }
	 }
	 
	 
	 \item{histograms}{The settings for the histogram plots:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 8.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 14.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 2.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 10.}
	     
	     \item{type}{The type of plot produced here. One in
	       \emph{histogram} or \emph{density}. Both plot types have
	       pros and cons: histograms can be misleading because of
	       bin size artefacts, whereas density plots hide the sample
	       size information.}
	     
	   }
	 }

	 \item{reproducibility}{The settings for the reproducibility
	   plate plots:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 8.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 12.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.3.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 10.}
	     
	     \item{col}{The color range that gets passed on to
	       \code{\link[prada]{plotPlate}} as argument
	       \code{col}. The default value is \code{brewer.pal(9,
		 "YlOrRd")}.}

	     \item{range}{The data range that gets passed on to
	       \code{plotPlate} as argument \code{xrange}. The default
	       value is \code{ function(x) c(0, quantile(x, 0.95,
		 na.rm=TRUE))}.}

	     \item{include}{A logical indicating whether to create the
	       plate plot or not. The default value is \code{FALSE}.}

	     \item{map}{A logical indicating whether to tooltips
	       containing the plate annotation for each well or not. The
	       default value is \code{FALSE}.}
	     
	   }
	 }

	 \item{average}{The settings for the reproducibility
	   plate plots:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 8.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 12.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.3.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 10.}
	     
	     \item{col}{The color range that gets passed on to
	       \code{\link[prada]{plotPlate}} as argument
	       \code{col}. The default value is \code{brewer.pal(9,
		 "YlOrRd")}.}

	     \item{range}{The data range that gets passed on to
	       \code{plotPlate} as argument \code{xrange}. The default
	       value is \code{ function(x) c(0, quantile(x, 0.95,
		 na.rm=TRUE))}.}

	     \item{include}{A logical indicating whether to create the
	       plate plot or not. The default value is \code{FALSE}.}

	     \item{map}{A logical indicating whether to tooltips
	       containing the plate annotation for each well or not. The
	       default value is \code{FALSE}.}
	   }
	 }

	 \item{intensities}{The settings for the raw data
	   plate plots:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 8.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 12.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.6.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 10.}
	     
	     \item{col}{The color range that gets passed on to
	       \code{plotPlate} as argument \code{col}. The default
	       value is \code{rev(brewer.pal(9, "RdBu"))}.}

	     \item{range}{The data range that gets passed on to
	       \code{plotPlate} as argument \code{xrange}. The default
	       value is \code{function(x) quantile(x, c(0.025, 0.975),
	       na.rm = TRUE)}. A useful alternative setting here would
	       be \code{function(x) c(-1,1) * max(abs(x), na.rm=TRUE)},
	       which forces a value of \code{0} to be white.}

	     \item{include}{A logical indicating whether to create the
	       plate plot or not. The default value is \code{FALSE}.}

	     \item{map}{A logical indicating whether to tooltips
	       containing the plate annotation for each well or not. The
	       default value is \code{FALSE}.}
	     
	   }
	 }
       }
     }

     \item{plateConfiguration}{This controls settings for the plate
       configuration part of the report. Available settings are:
	 
       \describe{
	 
	 \item{size}{The width in inches of the pdf device holding the
           plot. The default value is 14.}
	 
	 \item{fontSize}{The point size of the font for all the text in the
	   pdf version of the plot. The default value is 12.}
	 
	 \item{font}{The font used for all the text in the pdf version
           of the plot. The default is \code{"Helvetica"}.}
	 
	 \item{thumbFactor}{The factor by which the thumbnail png
           version of the plot is smaller compared to the
           high-resolution pdf version. The default value is 2.}
	 
         \item{thumbFontSize}{The point size of the font for all the
	   text in the thumbnail png version of the plot. The default
	   value is 9.}
	 
	 \item{include}{A logical indicating whether to create the plate
	   configuration plot or not. The default value is \code{TRUE}.}
	 
       }
     }

     \item{plateSummaries}{This controls settings for the plate
       summary plots in the report. There are two sub-sections:}
     
     \describe{
       
       \item{boxplot}{
	 
	 \describe{
	   
	   \item{size}{The width in inches of the pdf device holding the
             plot. The default value is 7.5.}
	   
	   \item{fontSize}{The point size of the font for all the text in the
	     pdf version of the plot. The default value is 12.}
	   
	   \item{font}{The font used for all the text in the pdf version
             of the plot. The default is \code{"Helvetica"}.}
	   
	   \item{thumbFactor}{The factor by which the thumbnail png
             version of the plot is smaller compared to the
             high-resolution pdf version. The default value is 1.5.}
	   
           \item{thumbFontSize}{The point size of the font for all the
	     text in the thumbnail png version of the plot. The default
	     value is 11.}
	   
	   \item{col}{The colors used to fill the boxes. A vector of
	     length two, where the first item specifies the color for
	     the raw data panel and the second item specifies the
	     color for the normalized data panel. The default value is
	     \code{c("pink", "lightblue")}.}
	   
	 }
       }
       
       \item{controls}{
	 
	 \describe{
	   
	   \item{size}{The width in inches of the pdf device holding the
             plot. The default value is 7.5.}
	   
	   \item{fontSize}{The point size of the font for all the text in the
	     pdf version of the plot. The default value is 12.}
	   
	   \item{font}{The font used for all the text in the pdf version
             of the plot. The default is \code{"Helvetica"}.}
	   
	   \item{thumbFactor}{The factor by which the thumbnail png
             version of the plot is smaller compared to the
             high-resolution pdf version. The default value is 1.5.}
	   
           \item{thumbFontSize}{The point size of the font for all the
	     text in the thumbnail png version of the plot. The default
	     value is 11.}
	   
	 }
       }	 
     }

     \item{screenSummary}{The settings for all the plots in the screen summary
       section of the report. There are several sub-section:
       
       \describe{
	 
	 \item{scores}{The settings for summary screen image plots:	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 7.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 10.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 9.}
	     
	     \item{col}{The color range used to map the data values
	       into. This has to be a named list of length two, with the
	       first item \code{posNeg} being the color range used when
	       there are both positive and negative values, and the
	       second item being the color range for positive values
	       only. One usually wants the former to be centered on
	       white to blend into the background, and the former to
	       start from white. The default value is
	       \code{list(posNeg=rev(brewer.pal(11, "RdBu"))[c(1:5,
		 rep(6,3), 7:11)], pos=brewer.pal(9, "Greys"))}.}
	     
	     \item{aspect}{The aspect ratio of the plot. The default
	       value is \code{1}.}
	     
	     \item{annotation}{An alternative character vector of
	       annotation mappings. See \code{\link{imageScreen}} for
	       details.}
	     
	     \item{map}{A logical indicating whether to tooltips
	       containing the plate annotation for each well or not. The
	       default value is \code{FALSE}.}

	     \item{range}{The range of values into which the colors will
	       be mapped. A numeric of length 2.}

	     \item{nbImageBins}{The number of color bins used in the
	       map. Default is 256.}
	     
	     \item{nbLegendBins}{The number of color bins shown in the
	       legend. Default is 7.}
	   }
	 }
	 
	 \item{qqplot}{The settings for the Normal Q-Q plots:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 7.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 10.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 9.}
	     
	   }
	 }
	 
	 \item{distribution}{The settings for the density distribution
	   plot:
	   
	   \describe{
	     
	     \item{size}{The width in inches of the pdf device holding the
               plot. The default value is 7.}
	     
	     \item{fontSize}{The point size of the font for all the text in the
	       pdf version of the plot. The default value is 10.}
	     
	     \item{font}{The font used for all the text in the pdf version
               of the plot. The default is \code{"Helvetica"}.}
	     
	     \item{thumbFactor}{The factor by which the thumbnail png
               version of the plot is smaller compared to the
               high-resolution pdf version. The default value is 1.}
	     
             \item{thumbFontSize}{The point size of the font for all the
	       text in the thumbnail png version of the plot. The default
	       value is 9.}
	     
	   }
	 }
       }
     }

     \item{screenResults}{The settings for the screen results panel:
       \describe{
	 \item{keepFieldPattern}{A regular expression indicating which
	   column names to keep. Default is "^plate$|^well$|^score$|^wellAnno$|^finalWellAnno$|raw_|normalized_|GeneID|GeneSymbol".}
	 \item{htmlMaxItems}{The maximal number of cells in the output
	   table to produce an HTML table. Default is 20000.}
	 \item{htmlLinks}{A data frame containing a \code{plate} column, a \code{well}
	   column, followed by the columns containing the HTML hyper
	   links to add to the output table. Default is \code{NULL}, no links.}
       }
     }
     
     \item{controls}{The settings for the annotation of control wells
       and other type of wells. Currently these are only two item:

       \describe{
	 
	 \item{col}{A named vector of colors to be used across all plots
	   for the annotation of the well types. Allowed values are:
	   \code{sample}, \code{neg}, \code{controls}, \code{other},
	   \code{empty}, \code{flagged}, \code{act}, \code{inh} and
	   \code{pos}. Additional user-defined well types that don't
	   fall in any of these groups will be assigned a color from the
	   palette defined in \code{otherCol}.}

	 \item{col}{A color palette from which colors are drawn for well
	   types not defined in \code{col}.}
	   
       }
     }

     \item{controls}{Global report settings:
       \describe{
	 \item{ppi}{A numeric indicating the resolution of the
	   screen. This parameter is used to generate PNG images.
	   Large numbers produce large PNG images. Default is 72.
	 }
       }
     }
   }
 }
 
\value{
  
  The current settings for \code{getSettings}.
  
  \code{setSettings} is called for its side effect of setting
  session-wide settings.
  
}

\author{
Florian Hahne
}


\seealso{
  \code{\link{writeReport}}
}

\examples{

oset <- getSettings()
oset

setSettings(list(plateConfiguration=list(size=2),
list(plateList=list(intensities=list(include=FALSE)))))

getSettings()

setSettings(oset)

}

