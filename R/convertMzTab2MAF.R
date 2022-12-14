#' @title convertMzTab2MAF
#' 
#' @description Convert from mzTab-M to MetaboLights MAF
#' 
#' @param mzTabfile path to an mzTab-M 2.0 file to be read with rmzTab-M
#' @param MAFfile for an output
#' 
#' @importFrom metabolighteR create.MAF write.MAF
#' @export convertMzTab2MAF
#' 
#' @examples
#' \dontrun{
#' example from the vignette
#' }

convertMzTab2MAF <- function(mzTabfile, MAFfile) {

  mzTabTable <- readMzTab(mzTabfile)

  ## To turn these tables into objects, use the R6 class constructor method `new()`:
  mzTabObject <- MzTab$new()$fromDataFrame(mzTabTable)
  metadata <- mzTabObject$metadata
  msassaynames <- sapply(metadata$assay, function(a) a$name)

  smlTable <- extractSmallMoleculeSummary(mzTabTable)
  smfTable <- extractSmallMoleculeFeatures(mzTabTable)
  #smeTable <- extractSmallMoleculeEvidence(mzTabTable)

  abundances <- as.matrix(smlTable[,which(grepl("abundance_assay", colnames(smlTable)))])
  colnames(abundances) <- msassaynames

  ##
  ## Writing as MAF file
  ##

  #library(metabolighteR)

  maf <- create.MAF(abundances=abundances)

  ## Adding in additional columns from MS-Dial output
  #maf[,""] <- smlTable[,""]

  maf[,"chemical_formula"] <- smlTable[,"chemical_formula"]
  maf[,"smiles"] <- smlTable[,"smiles"]
  maf[,"inchi"] <- smlTable[,"inchi"]
  maf[,"metabolite_identification"] <- smlTable[,"chemical_name"]

  ##
  ## This only works if sml and smf have same dim and same order !!!
  ##
  maf[,"retention_time"] <- smfTable[, "retention_time_in_seconds"]
  maf[,"mass_to_charge"] <- smfTable[, "exp_mass_to_charge"]

  ##
  ## Database handling in mzTab is more powerful than in MAF,
  ## multiple databases are allowed. This is NOT considered here

  maf[,"database"] <- metadata$database[[1]]$id ## https://github.com/lifs-tools/rmzTab-m/issues/24
  maf[,"database_version"] <- metadata$database[[1]]$version
  maf[,"database_identifier"] <- smlTable[,"database_identifier"] ## Mybe strip the mzTab PREFIX ?

  ## Would require normalisation/mapping between scoring systems:
  maf[,"reliability"] <- smlTable[,"best_id_confidence_value"]

  write.MAF(maf, MAFfile)
  return(maf)
}
