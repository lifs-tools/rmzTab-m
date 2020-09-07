
#'
#' Read an mzTab tab separated file from the passed in file.
#' @param filename the mzTab file to parse.
#' @export
readMzTab <- function(filename) {
  # read maximum number of columns in file
  ncol <- max(stats::na.omit(utils::count.fields(file=filename, sep = "\t")))
  mztab.table = utils::read.table(file=filename, header=FALSE,
                    row.names=NULL, dec = ".", fill = TRUE,
                    col.names = paste0("V", seq_len(ncol)),
                    sep="\t", na.strings="null", quote = "", colClasses = "character")
  mztab.table
}

#'
#' Read an mzTab tab separated 'file' from the passed in string.
#' This file creates a temporary file on disk to check for the maximum number of columns.
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
                                  sep="\t", na.strings="null", quote = "", colClasses = "character")
  mztab.table
}

extractPart <- function(mztab.table, matchColumn, matchValue) {
  mztab.table[startsWith(as.character(mztab.table[,matchColumn]), matchValue),]
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

mtdIdElements <- c(
  "sample_processing",
  "instrument",
  "software",
  "publication",
  "uri",
  "external_study_uri",
  "quantification_method",
  "study_variable",
  "ms_run",
  "assay",
  "sample",
  "custom",
  "cv",
  "database",
  "derivatization_agent"
)

mtdRegexps <- as.vector(paste0(mtdIdElements, idRegexp))
names(mtdRegexps) <- mtdIdElements



buildMtdVersion <- function(mztab.mtd.table) {
  mztab.table[startsWith(as.character(mztab.table$V2), "mzTab-version"),]
}

buildMtdId <- function(mztab.mtd.table) {
  
}

#'
#'Read the metadata from an mztab data frame and return the Metadata object.
#'@param the mztab data frame
#'
extractMetadata = function(MzTabDataFrame) {
  prefix <- "MTD"
  mtd.table <- MzTabDataFrame[startsWith(as.character(MzTabDataFrame$V1), prefix), c("V1","V2","V3")]
  mtd.table
}

extractSummary <- function(mztab.table) {
  
  smh <- mztab.table[startsWith(as.character(mztab.table$V1), "SMH"),]
  sml <- mztab.table[startsWith(as.character(mztab.table$V1), "SML"),]
  sml.data.frame <- data.frame(sml)
  print(as.vector(smh[1,]))
  colnames(sml.data.frame) <- as.character(smh[1,])
  rbind(smh, sml)
}

extractFeatures <- function(mztab.table) {
  sfh <- mztab.table[startsWith(as.character(mztab.table$V1), "SFH"),]
  smf <- mztab.table[startsWith(as.character(mztab.table$V1), "SMF"),]
  rbind(sfh, smf)
}

extractEvidence <- function(mztab.table) {
  seh <- mztab.table[startsWith(as.character(mztab.table$V1), "SEH"),]
  sme <- mztab.table[startsWith(as.character(mztab.table$V1), "SME"),]
  rbind(seh, sme)
}

splitList <- function(listString, separator="|") {
  paramFields <- trimws(unlist(strsplit(listString, separator, fixed=TRUE)), which = "both")
}
