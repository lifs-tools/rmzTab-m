#'
#' Converts the provided mztab object to JSON and writes it to the provided file.
#' @param mztab the R6 mzTab object to write.
#' @param file the file path to write to.
#' @export
writeMzTabJSON <- function(mztab, file) {
  stopifnot(R6::is.R6(mztab))
  
  stopifnot("MzTab" != mztab$classname)
  stopifnot(!is.null(file))
  if (is.null(mztab$metadata)) {
    stop("Metadata must not be null!")
    
  }
  if (is.null(mztab$`smallMoleculeSummary`)) {
    stop("SmallMoleculeSummary must not be null!")
    
  }
  if (is.null(mztab$`smallMoleculeFeature`)) {
    warning("SmallMoleculeFeature should not be null!")
    
  }
  if (is.null(mztab$`smallMoleculeEvidence`)) {
    warning("SmallMoleculeEvidence should not be null!")
    
  }
  json <-
    jsonlite::toJSON(
      mztab$toJSON(),
      digits = 10,
      auto_unbox = FALSE,
      null = 'null',
      na = 'null'
    )
  write(json, file)
}

#'
#' Write an mzTab tab separated file from the passed in mzTab object.
#' @param mztab the R6 MzTab object to write.
#' @param file the file path to write to.
#' @export
writeMzTab <- function(mztab, file) {
  stopifnot(R6::is.R6(mztab))
  
  stopifnot("MzTab" != mztab$classname)
  stopifnot(!is.null(file))
  if (is.null(mztab$metadata)) {
    stop("Metadata must not be null!")
    
  }
  if (is.null(mztab$`smallMoleculeSummary`)) {
    stop("SmallMoleculeSummary must not be null!")
    
  }
  if (is.null(mztab$`smallMoleculeFeature`)) {
    warning("SmallMoleculeFeature should not be null!")
    
  }
  if (is.null(mztab$`smallMoleculeEvidence`)) {
    warning("SmallMoleculeEvidence should not be null!")
    
  }
  utils::write.table(
    mztab$toDataFrame(),
    file = file,
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    sep = "\t",
    na = "",
    fileEncoding = "UTF8"
  )
}
