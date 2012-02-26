\name{cellHTS-class}
\docType{class}
\alias{cellHTS-class}
\alias{cellHTS}
\alias{class.cellHTS}

\alias{show,cellHTS-method}
\alias{name}
\alias{name<-}
\alias{name,cellHTS-method}
\alias{name<-,cellHTS,character-method}
\alias{channelNames<-}
\alias{channelNames<-,cellHTS,character-method}
\alias{initialize,cellHTS-method}
%\alias{pdim,cellHTS-method}
\alias{nbatch}
\alias{nbatch,cellHTS-method}
\alias{compare2cellHTS}
\alias{compare2cellHTS,cellHTS,cellHTS-method}
\alias{meanSdPlot}
\alias{meanSdPlot,cellHTS-method}
\alias{coerce,chtsImage,data.frame-method}


\title{A class for data from cell-based high-throughput assays
  performed in plate format.}

\description{
  Container for data and experimental meta-data from cell-based
  high-throughput assays performed in plate format.
  Typical applications are RNA interference or small molecular compound screens.
  The class extends the \code{\link[Biobase:class.NChannelSet]{NChannelSet}} class.
  Data are from experiments where the same set of reagents (probes) where used. 
  The class can represent data from multi-channel assays.

  The data can be thought of as being organised in a two- or
  three-dimensional array as follows:
  \itemize{
    \item The first dimension corresponds to reagents (e.g. siRNAs,
    chemical compounds) that were used in the assays. For example, if
    the screen used 100 plates of 384 wells (24 columns, 16 rows), then
    the first dimension has size 38,400, and the \code{cellHTS} object
    keeps track of plate ID, row, and column associated with each element.
    For historic reasons, and because we are using infrastructure that
    was developed for microarray experiments, the following terms are
    used synonymously for the elements of the first dimension: reagents,
    features, probes, genes.
    \item The second dimension corresponds to assays, including
    replicates and different experimental conditions (cell type,
    treatment, genetic background). A potentially confusing terminology is that the data
    structure that annotates the second dimension is called \code{phenoData}.
    This is because we are using infrastructure
    (the \code{\link[Biobase:class.NChannelSet]{NChannelSet}} class) that
    uses this unfortunate term for this purpose. The software provides
    methodology for replicate summarization and scoring, however more
    complicated experimental designs are not directly
    supported. Multi-purpose tools like \code{\link[limma]{lmFit}} in the
    \code{limma} package should be consulted.
    \item The (optional) third dimension corresponds to different
    channels (e.g. different luminescence reporters)    
  }
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("cellHTS",
    assayData, phenoData, ...)}. See the examples below.
}

\section{Slots}{
  
  \describe{

    \item{\code{plateList}:}{ a \code{\link{data.frame}} containing what
      was read from input measurement data files plus a column
      \code{status} of type character containing the string "OK" if the
      data import appeared to have gone well, and the respective error
      or warning message otherwise.}

    \item{\code{intensityFiles}:}{ a list, where each component contains
      a copy of the imported input data files. Its length corresponds to
      the number of rows of \code{plateList}.  }

    \item{\code{state}:}{ a logical vector of length 4 representing the
      processing status of the object. It must have the names
      "configured", "normalized", "scored" and "annotated".  }

    \item{\code{plateConf}:}{ a \code{\link{data.frame}} containing what
      was read from the \emph{configuration file} for the experiment
      (except the first two header rows). It contains at least three
      columns named \code{Plate}, \code{Well} and \code{Content}.
      Columns \code{Plate} and \code{Well} are allowed to contain
      regular expressions.  }

    \item{\code{screenLog}:}{ a \code{\link{data.frame}} containing what
      was read from the \emph{screen log file} for the experiment, in
      case it exists. Contains at least three columns, and column names
      \code{Plate}, \code{Well}, and \code{Flag}. Additional columns are
      \code{Sample} (when there are replicates or more than one sample
      or condition) and \code{Channel} (when there are multiple
      channels).  }

    \item{\code{screenDesc}:}{ a character containing what was read from
      the \emph{description file} of the experiment.  }

    \item{\code{rowcol.effects}:}{ a 3D array of size Features
      (i.e. plate size x number of plates) x Samples x Channels
      containing estimated row and column plate spatial offsets.  }

    \item{\code{overall.effects}:}{ a 3D array of size Features x
      Samples x Channels containing estimated plate overall offsets.}

    \item{\code{assayData}:}{ Object of class
      \code{\link[Biobase:class.AssayData]{AssayData}}, usually an
      environment containing matrices of identical size. Each matrix
      represents a single channel. Columns in each matrix correspond to
      samples (or replicate), rows to features (probes).  Once created,
      \code{cellHTS} manages coordination of samples and channels.  }

    \item{\code{plateData}:}{ A list of data frames, with number of rows
      of each frame equal to the number of plates used in the assay and
      number of columns equal to the number of samples. Each data frame
      in the list should contain factorial annotation information
      relevant for the individual plates, like experimental batches or
      varying types of micro-titre plates. Currently, this information
      will be used for between-batch normalization and quality
      assessment.  }

    \item{\code{phenoData}:}{ Object of class
      \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}}.
      Please see the documentation of the \code{phenoData} slot of
      \code{\link[Biobase:class.NChannelSet]{NChannelSet}} for more
      details.

      It contains information about the screens, and it must have the
      following columns in its \code{data} component: \code{replicate}
      and \code{assay}, where \code{replicate} is expected to be a
      vector of integers giving the replicate number, while \code{assay}
      is expected to be a vector of characters giving the name of the
      biological assay. Both of these vectors should have the same
      length as the number of Samples.

      Once created, \code{cellHTS} coordinates selection and subsetting
      of channels in \code{phenoData}.  }

    \item{\code{featureData}:}{ Object of class
      \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}},
      containing information about the reagents: plate, well, column,
      the well annotation (sample, control, etc.), etc.  For a
      \code{\linkS4class{cellHTS}} object, this slot must contain in its
      \code{data} component at least three mandatory columns named
      \code{plate}, \code{well} and \code{controlStatus}. Column
      \code{plate} is expected to be a numeric vector giving the plate
      number (e.g. 1, 2, ...), \code{well} should be a vector of
      characters (alphanumeric characters) giving the well ID within the
      plate (e.g. A01, B01, H12, etc.).  Column \code{controlStatus}
      should be a factor specifying the annotation for each well with
      possible levels: \emph{empty}, \emph{other}, \emph{neg},
      \emph{sample}, and \emph{pos}. Other levels besides \emph{pos} and
      \emph{neg} may be employed for controls.  }

    \item{\code{experimentData}:}{ Object of class
      \code{\link[Biobase:class.MIAME]{MIAME}} containing descriptions
      of the experiment.}

    \item{\code{annotation}:}{ A \code{"character"} of length 1, which
      can be used to specify the name of an annotation package that goes
      with the reagents used for this experiment.}

    \item{\code{processingInfo}:}{A list containing information about
      which normalization and summarization methods have been used.}

    \item{\code{.__classVersion__}:}{ Object of class
      \code{\link[Biobase:class.Versions]{Versions}}, containing
      automatically created information about the class definition,
      Biobase package version, and other information about the user
      system at the time the instance was created. See
      \code{\link[Biobase]{classVersion}} and
      \code{\link[BiocGenerics]{updateObject}} for examples of use.  }

  } 
}


\section{Extends}{
  Class \code{\link[Biobase:class.NChannelSet]{NChannelSet}}, directly.
}


\section{Methods}{
  
  Methods with class-specific functionality:

  \describe{

    \item{\code{name(object)}}{ \code{signature(object="cellHTS")}.
      Obtains the name of the assay stored in the \code{object}. This
      corresponds to the contents of column \code{assay} of the
      \code{phenoData} slot of the \code{cellHTS} object.  }

    \item{\code{name(object) <- value}}{ \code{signature(object =
      "cellHTS", value = "character")} assign the character of length
      one (\code{value}) to the elements in column \code{assay} of the
      slot \code{phenoData} of \code{object}.  }

    \item{\code{pdim(object)}}{
      \code{signature(object = "cellHTS")}.
      Obtain the plate dimension for the data stored in \code{object}.
    }

    \item{\code{channelNames<-(object, value)}}{
      \code{signature(object = "cellHTS", value = "character")}.
      Replace the channel names in \code{object}.
    }
    
    \item{\code{nbatch(object)}}{
      \code{signature(object = "cellHTS")}.
      Obtain the total number of batches for the data stored in \code{object}.
    }
     
    \item{\code{compare2cellHTS(x, y)}}{ \code{signature(x = "cellHTS",
      y = "cellHTS")}.  Compares two \code{\linkS4class{cellHTS}}
      objects, \code{x} and \code{y}, returning \code{TRUE} if they are
      from the same experiment (i.e. if they derive from the same
      initial \code{cellHTS} object), or \code{FALSE} otherwise.  }

  }

  Methods with functionality derived from class
      \code{\link[Biobase:class.NChannelSet]{NChannelSet}}:
      \code{channel}, \code{channelNames}, \code{selectChannels},
      \code{object[features, samples]}, \code{sampleNames}

      Methods with functionality derived from
      \code{\link[Biobase:class.eSet]{eSet}}: \code{annotation},
      \code{assayData}, \code{assayData<-}, \code{classVersion},
      \code{classVersion<-}, \code{dim}, \code{dims},
      \code{experimentData}, \code{featureData}, \code{phenoData},
      \code{phenoData<-}, \code{pubMedIds}, \code{sampleNames},
      \code{sampleNames<-}, \code{storageMode}, \code{varMetadata},
      \code{isCurrent}, \code{isVersioned}.
      
  Additional methods: 
  \describe{

    \item{\code{initialize}}{used internally for creating objects}

    \item{\code{show}}{invoked automatically when the object is
      displayed to the screen. It prints a summary of the object.}

    \item{\code{state}}{Access the \code{state} slot of a
      \code{\linkS4class{cellHTS}} instance.}

    \item{\code{annotate}}{Annotate the \code{\linkS4class{cellHTS}}
      object using the \emph{screen annotation file}.}
      \item{\code{configure}}{Configure the \code{\linkS4class{cellHTS}}
      object using the the \emph{screen description file}, the
      \emph{screen configuration file} and the \emph{screen log file}.}
      \item{\code{writeTab}}{Write the contents of \code{assayData} slot
      of a \code{\linkS4class{cellHTS}} object to a tab-delimited file.}

    \item{\code{ROC}}{Construct an object of S4 class
      \code{\linkS4class{ROC}}, which represents a
      receiver-operator-characteristic curve, from the data of the
      annotated positive and negative controls in a scored
      \code{\linkS4class{cellHTS}} object.}

    \item{\code{meanSdPlot(x)}}{\code{signature(x = "cellHTS")} plots
      row standard deviations across samples versus row means across
      samples for data stored in slot \code{assayData} of a
      \code{cellHTS} object. If there are multiple channels, row
      standard deviations and row means are calculated across samples
      for each channel separately. Only wells containing "sample" are
      considered. See \code{\link[vsn]{meanSdPlot}} for more details
      about this function.}

  } %describe
}

\author{Ligia P. Bras \email{ligia@ebi.ac.uk}, Wolfgang Huber \email{huber@ebi.ac.uk}}

\seealso{
  \code{\link[Biobase:class.NChannelSet]{NChannelSet}}
  \code{\link[cellHTS2:readPlateList]{readPlateList}}
  \code{\link[cellHTS2:annotate]{annotate}}
  \code{\link[cellHTS2:configure]{configure}}
  \code{\link[cellHTS2:writeTab]{writeTab}}
   \code{\link[cellHTS2:state]{state}}
  \code{\link[cellHTS2:Data]{Data}}
  \code{\link[cellHTS2:normalizePlates]{normalizePlates}}
  \code{\link{ROC}}

}

\examples{
    showClass("cellHTS")
    showMethods(class="cellHTS")
  
    ## An empty cellHTS
    obj <- new("cellHTS")

    data("KcViabSmall")
    KcViabSmall
    state(KcViabSmall)
    ## Replicate 1 as a cellHTS object
    y <- KcViabSmall[,1] 
    compare2cellHTS(KcViabSmall, y)
    data("KcViab")
    compare2cellHTS(KcViab, KcViabSmall)
}

\keyword{classes}
