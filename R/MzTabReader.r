
#'
#' Read an mzTab tab separated file from the passed in file.
#' @param filename the mzTab file to parse.
#' @export
readMzTab <- function(filename) {
  # read maximum number of columns in file
  ncol <- max(na.omit(count.fields(file=filename, sep = "\t")))
  print(ncol)
  mztab.table = utils::read.table(file=filename, header=FALSE,
                    row.names=NULL, dec = ".", fill = TRUE,
                    col.names = paste0("V", seq_len(ncol)),
                    sep="\t", na.strings="null", quote = "")
  mztab.table
}

extractMetadata <- function(mztab.table) {
  mztab.table[startsWith(as.character(mztab.table$V1), "MTD"),]
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
