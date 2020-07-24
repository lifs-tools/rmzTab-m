
#'
#' Read an mzTab tab separated file from the passed in file.
#' @param filename the mzTab file to parse.
#' @export
readMzTab <- function(filename) {
  # read maximum number of columns in file
  ncol <- max(stats::na.omit(utils::count.fields(file=filename, sep = "\t")))
  print(ncol)
  mztab.table = utils::read.table(file=filename, header=FALSE,
                    row.names=NULL, dec = ".", fill = TRUE,
                    col.names = paste0("V", seq_len(ncol)),
                    sep="\t", na.strings="null", quote = "")
  mztab.table
}

extractPart <- function(mztab.table, matchColumn, matchValue) {
  mztab.table[startsWith(as.character(mztab.table[,matchColumn]), matchValue),]
}

idRegexp <- "\\[([0-9]+)\\]"

extractIds <- function(mtd.sub.table, column, regexp=idRegexp) {
  sapply(mtd.sub.table[, column], FUN = function(x) {
    regmatches(as.character(x), gregexpr(regexp, as.character(x), perl = TRUE))
  })
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

#'
#'Read the metadata from an mztab table and return the Metadata object.
#'@param the mztab table
#'
extractMetadata <- function(mztab.table) {
  prefix <- "MTD"
  browser()
  mtd.table <- mztab.table[startsWith(as.character(mztab.table$V1), prefix), c("V2","V3")]
  mzTabVersion <- asCharacter(extractPart(mtd.table, "V2", "mzTab-version"),1,"V3")
  mzTabId <- asCharacter(extractPart(mtd.table, "V2", "mzTab-ID"),1,"V3")
  title <- asCharacter(extractPart(mtd.table, "V2", "title"),1,"V3")
  description <- asCharacter(extractPart(mtd.table, "V2", "description"),1,"V3")
  sampleProcessings <- extractPart(mtd.table, "V2", "sample_processing")
  instruments <- extractPart(mtd.table, "V2", "instruments")
  softwares <- extractPart(mtd.table, "V2", "software")
  publications <- extractPart(mtd.table, "V2", "publication")
  contacts <- extractPart(mtd.table, "V2", "contact")
  uris <-  extractPart(mtd.table, "V2", "uri")
  extStudyUris <-  extractPart(mtd.table, "V2", "external_study_uri")
  quantMethod <- extractPart(mtd.table, "V2", "quantification_method")
  studyVariables <- extractPart(mtd.table, "V2", "study_variable")
  msRuns <- extractPart(mtd.table, "V2", "ms_run")
  assays <- extractPart(mtd.table, "V2", "assay")
  samples <- extractPart(mtd.table, "V2", "sample")
  custom <- extractPart(mtd.table, "V2", "custom")
  cvs <- extractPart(mtd.table, "V2", "cv")
  databases <- extractPart(mtd.table, "V2", "database")
  derivatizationAgents <- extractPart(mtd.table, "V2", "derivatization_agent");
  smQuantUnit <- extractPart(mtd.table, "V2", "small_molecule-quantification_unit")
  smfQuantUnit <- extractPart(mtd.table, "V2", "small_molecule_feature-quantification_unit")
  smIdentReliability <- extractPart(mtd.table, "V2", "small_molecule-identification_reliability")
  idConfidenceMeasures <- extractPart(mtd.table, "V2", "id_confidence_measure")
  colunitSm <- extractPart(mtd.table, "V2", "colunit-small_molecule")
  colunitSmf <- extractPart(mtd.table, "V2", "colunit-small_molecule_feature")
  colunitSme <- extractPart(mtd.table, "V2", "colunit-small_molecule_evidence")
  
  mtd <- Metadata$new(
    `prefix` = prefix,
    `mzTab-version` = mzTabVersion,
    `mzTab-ID` = mzTabId,
    `title` = title,
    `description` = description,
    `sample_processing` = NULL,
    `instrument` = NULL,
    `software` = NULL,
    `publication` = NULL,
    `contact` = NULL,
    `uri` = NULL,
    `external_study_uri` = NULL,
    `quantification_method` = NULL,
    `study_variable` = NULL,
    `ms_run` = NULL,
    `assay` = NULL,
    `sample` = NULL,
    `custom` = NULL,
    `cv` = NULL,
    `database` = NULL,
    `derivatization_agent` = NULL,
    `small_molecule-quantification_unit` = NULL,
    `small_molecule_feature-quantification_unit` = NULL,
    `small_molecule-identification_reliability` = NULL,
    `id_confidence_measure` = NULL,
    `colunit-small_molecule` = NULL,
    `colunit-small_molecule_feature` = NULL,
    `colunit-small_molecule_evidence` = NULL
  )
  mtd
}

buildMtdVersion <- function(mztab.mtd.table) {
  mztab.table[startsWith(as.character(mztab.table$V2), "mzTab-version"),]
}

buildMtdId <- function(mztab.mtd.table) {
  
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
