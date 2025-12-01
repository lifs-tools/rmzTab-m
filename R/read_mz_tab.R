
#' Read mzTab-M file into data frame.
#' @description Read an mzTab tab separated file from the passed in file.
#' @param filename the mzTab file to parse.
#' @export
readMzTab <- function(filename) {
  # read maximum number of columns in file
  ncol <- max(stats::na.omit(utils::count.fields(file=filename, sep = "\t")))
  if (is.infinite(ncol)) {
    stop(paste("Number of columns is infinite.", paste0("Please check file: ", filename)))
  }
  mztab.table = utils::read.table(file=filename, header=FALSE,
                    row.names=NULL, dec = ".", fill = TRUE,
                    col.names = paste0("V", seq_len(ncol)),
                    sep="\t", na.strings="null", quote = "", comment.char = "",
                    blank.lines.skip = TRUE, strip.white = TRUE,
                    colClasses = "character")
  mztab.table
}

#' Read an mzTab-M string into a data frame.
#' @description Read an mzTab tab separated file from the passed in string.
#' This file creates a temporary file on disk to check for the maximum number of columns.
#' The returned object is a data.frame as returned by utils::read.table. 
#' 
#' @param mzTabString the mzTab string to parse.
#' @export
readMzTabString <- function(mzTabString) {
  file <- tempfile(fileext=".mzTab.tmp")
  base::write(mzTabString, file=file)
  # read maximum number of columns in file
  ncol <- max(stats::na.omit(utils::count.fields(file=file, sep = "\t")))
  mztab.table = utils::read.table(file=file, header=FALSE,
                                  row.names=NULL, dec = ".", fill = TRUE,
                                  col.names = paste0("V", seq_len(ncol)),
                                  sep="\t", na.strings="null", quote = "", comment.char = "",
                                  blank.lines.skip = TRUE, strip.white = TRUE,
                                  colClasses = "character")
  mztab.table
}

extractPart <- function(mztab.table, matchColumn, matchValue) {
  mztab.table[startsWith(as.character(mztab.table[,matchColumn]), matchValue),]
}

extractParameter <- function(mztab.table, matchColumn, matchValue, valueColumn = "V3") {
  subTable <- extractPart(mztab.table, matchColumn, matchValue)
  stringValue <- asCharacter(subTable, 1, "V3")
  if(!is.null(stringValue)) {
    param <- Parameter$new()
    param$fromString(NULL, stringValue)
    return(param)
  }
  return(NULL)
}

idRegexp <- "\\[([0-9]+)\\]"

idElementRegexp <- "[a-z\\-_]+\\[([0-9]+)\\].*"

extractIds <- function(mtd.sub.table, column, regexp=idRegexp) {
  sapply(mtd.sub.table[, column], FUN = function(x) {
    regmatches(as.character(x), gregexpr(regexp, as.character(x), perl = TRUE))
  })
}

extractId <- function(idElement, regexp=idRegexp) {
  as.numeric(gsub(idElementRegexp, "\\1", as.character(idElement)))
}
# returns a list of elements / wide data frames for each id in the format that jsonlite::fromJSON returns.
# mapEmptyKeyTo can map either to name, param or parameter
extractIdElements <- function(mtd.sub.table, typePrefix, mapEmptyKeyTo="name") {
  #browser()
  databaseRows <- mtd.sub.table[startsWith(as.character(mtd.sub.table$V2), typePrefix),]
  databaseRows$id <- as.numeric(gsub(idElementRegexp, "\\1", databaseRows$V2))
  uniqueIds <- unique(databaseRows$id)
  dbSubTables <- lapply(uniqueIds, function(x, df) {
    # work on subset of data
    subset <- df[df$id==x,c("V2", "V3")]
    # replace typePrefix
    subset$V2 <- gsub(paste0(typePrefix,"[",x,"]"), replacement="", subset$V2, fixed=TRUE)
    # replace remaining "-"
    subset$V2 <- gsub("-", replacement="", subset$V2, fixed=TRUE)
    if (mapEmptyKeyTo=="name") {
      # implicit mapping to name property
      subset$V2[subset$V2==""] <- "name"
    } else if(mapEmptyKeyTo=="param") {
      # implicit mapping to param, e.g. for database
      subset$V2[subset$V2==""] <- "param"
    } else if(mapEmptyKeyTo=="parameter") {
      # implicit mapping to param, e.g. for database
      subset$V2[subset$V2==""] <- "parameter"
    } else {
      warning(paste("Unsupported mapEmptyKeyTo property:", mapEmptyKeyTo, "Supported are 'name', 'param' and 'parameter'"))
    }
    # pivot table from long to wide format (which is the one JSONLITE expects)
    subsetWide <- tidyr::pivot_wider(subset, names_from="V2", values_from="V3")
    # add id as column
    subsetWide$id <- as.numeric(x)
    subsetWide
  }, df = databaseRows)
  dbSubTables
}

extractUnique <- function(mtd.sub.table, column, regexp) {
  unique(regmatches(as.character(mtd.sub.table$V2), gregexpr(regexp, as.character(mtd.sub.table$V2), perl = TRUE)))
}

asCharacter <- function(mtd.sub.table, row, column) {
  as.character(mtd.sub.table[row, column])
}

# mtdIdElements <- c(
#   "sample_processing",
#   "instrument",
#   "software",
#   "publication",
#   "uri",
#   "external_study_uri",
#   "quantification_method",
#   "study_variable",
#   "ms_run",
#   "assay",
#   "sample",
#   "custom",
#   "cv",
#   "database",
#   "derivatization_agent"
# )

# mtdRegexps <- as.vector(paste0(mtdIdElements, idRegexp))
# names(mtdRegexps) <- mtdIdElements
# 
# 
# 
# buildMtdVersion <- function(mztab.mtd.table) {
#   mztab.table[startsWith(as.character(mztab.table$V2), "mzTab-version"),]
# }
# 
# buildMtdId <- function(mztab.mtd.table) {
#   
# }

#' Extract metadata from data frame.
#' @description Extract the \link{Metadata} from an mztab data frame.
#' @param MzTabDataFrame the mztab data frame
#' @export
extractMetadata = function(MzTabDataFrame) {
  prefix <- "MTD"
  mtd.table <- MzTabDataFrame[startsWith(as.character(MzTabDataFrame$V1), prefix), c("V1","V2","V3")]
  mtd.table
}

extractTable = function(mztab.table, headerPrefix, contentPrefix) {
  headers <- mztab.table[startsWith(as.character(mztab.table$V1), headerPrefix),]
  contents <- mztab.table[startsWith(as.character(mztab.table$V1), contentPrefix),]
  if (nrow(contents) >0) {
    df <- data.frame(contents)
    colnames(df) <- as.character(headers[1,])
    df[, colnames(df) != ""] ## remove empty (e.g. trailing) columns
  } else if (nrow(contents) == 0 & nrow(headers==1)) {
    warning("Empty ",contentPrefix, " section")
    df <- data.frame(headers[NULL,])
    colnames(df) <- as.character(headers[1,])
    df[, colnames(df) != ""] ## remove empty (e.g. trailing) columns
  } else {
    warning("No ",contentPrefix, " section")
    df <- NULL ## Maybe empty dataframe with mandatory columns only ?!
  }
}

#' Extract the SmallMoleculeSummary from data frame.
#' @description Extract the \link{SmallMoleculeSummary} from an mztab data frame.
#' @param mztab.table the mztab data frame
#' @export
extractSmallMoleculeSummary <- function(mztab.table) {
  sml <- extractTable(mztab.table, "SMH", "SML")
  rownames(sml) <- sml[,"SML_ID"]
  sml
}

#' Extract the SmallMoleculeFeatures from data frame.
#' @description Extract the \link{SmallMoleculeFeature} from an mztab data frame.
#' @param mztab.table the mztab data frame
#' @export
extractSmallMoleculeFeatures <- function(mztab.table) {
  smf <- extractTable(mztab.table, "SFH", "SMF")
  rownames(smf) <- smf[,"SMF_ID"]
  smf
}

#' Extract the SmallMoleculeEvidence from data frame.
#' @description Extract the \link{SmallMoleculeEvidence} from an mztab data frame.
#' @param mztab.table the mztab data frame
#' @export
extractSmallMoleculeEvidence <- function(mztab.table) {
  sme <- extractTable(mztab.table, "SEH", "SME")
  rownames(sme) <- sme[,"SME_ID"]
  sme
}

#' Extract the Comment from data frame.
#' @description Extract the \link{Comment}s from an mztab data frame.
#' @param mztab.table the mztab data frame
#' @export
extractComments <- function(mztab.table) {
  prefix <- "COM"
  com.table <- mztab.table[startsWith(as.character(mztab.table$V1), prefix), c("V1", "V2")]
  colnames(com.table) <- c("prefix", "msg")
  com.table$line_number <- rownames(com.table)
  com.table
}

splitList <- function(listString, separator="|") {
  paramFields <- trimws(unlist(strsplit(listString, separator, fixed=TRUE)), which = "both")
}
