#' @title convertMzTab2MAF
#' 
#' @description Convert from mzTab-M to MetaboLights MAF
#' 
#' The `convertMzTab2MAF()` function will read the mzTab-M via `readMzTab()`
#' and extract the `smlTable` and the `smfTable`. Abundances are extracted 
#' from the `smlTable`, while *m/z* and retention time come from the feature table. 
#' 
#' @param mzTabfile path to an mzTab-M 2.0 file to be read with rmzTab-M
#' @param MAFfile for an output
#' 
#' @returns data.frame of the MAF file (invisible)
#' 
#' @importFrom metabolighteR create.MAF write.MAF
#' @export convertMzTab2MAF
#' 
#' @examples
#' \dontrun{
#' mztabfile <- system.file("testdata", 
#'                          c("lcmsms_dda_hydrophilic_height_mzTab.mztab"), 
#'                          package="rmzTabM")
#' MAFfile <- file.path(tempdir(check=TRUE), "m_MTBLS0815_v2_maf.tsv")                      
#' 
#' maf <- convertMzTab2MAF(mzTabfile=mztabfile,
#'                         MAFfile=MAFfile)
#' }

convertMzTab2MAF <- function(mzTabfile, MAFfile) {

  mzTabTable <- readMzTab(mzTabfile)

  ## To turn these tables into objects, use the R6 class constructor method `new()`:
  mzTabObject <- MzTab$new()$fromDataFrame(mzTabTable)
  metadata <- mzTabObject$metadata
  msassaynames <- sapply(metadata$assay, function(a) a$name)

  smlTable <- extractSmallMoleculeSummary(mzTabTable)
  smfTable <- extractSmallMoleculeFeatures(mzTabTable)
  
  # SME not needed here
  # smeTable <- extractSmallMoleculeEvidence(mzTabTable)

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

  maf[,"retention_time"] <- moleculeRT(smlTable, smfTable)
  maf[,"mass_to_charge"] <- moleculeMZ(smlTable, smfTable)

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

checkRefs <- function(table1, refcol, table2) {
  ## Check if all refs in table1 point to a row in table2,
  ## where rownames(table2)

  simplerefs <- not(grepl('\\|', table1[, refcol]))
  any(is.na(rownames(table2)[simplerefs]))

  multirefs <- !simplerefs
  if (any(multirefs)) {
    multirefindices <- lapply(strsplit(table1[, refcol], "|", fixed=TRUE),
                              trimws)
    misses <- sapply(multirefindices, function(x) any(is.na(rownames(table2[x,1]))))
    if (any(misses)) {
      warning(length(which(misses)), " entries have broken REFS")
    }
  }
}

moleculeRT <- function(smlTable, smfTable) {
  ## Initialise empty RTs
  rt <- rep(NA_real_, nrow(smlTable))
  
  ## These are simple & fast:
#  simplerefs <- not(grepl('\\|', smlTable$SMF_ID_REFS))
  simplerefs <- !grepl('\\|', smlTable$SMF_ID_REFS)
  simplerefidx <- smlTable[simplerefs, "SMF_ID_REFS"]
  rt[simplerefs] <- as.numeric(smfTable[simplerefidx, "retention_time_in_seconds"])
  
  multirefs <- !simplerefs
  if (any(multirefs)) {
    ## Other metabolites require some (slower) split/list/mean calculation
    multirefindices <- lapply(strsplit(smlTable[multirefs, "SMF_ID_REFS"], "|", fixed=TRUE),
                              trimws)
    agg <- t(sapply(multirefindices, function(x) {
      rt <- as.numeric(smfTable[x, "retention_time_in_seconds"])
      c(mean=mean(rt), var=var(rt))
    }))
    rownames(agg) <- rownames(smlTable)[multirefs]
    
    weirdos <- which(agg[, "var"] > max(c(0, rt), na.rm = TRUE) * 0.1)
    if (length(weirdos)>0) {
      warning(length(weirdos), " features have RT variance > 10% of max RT")
    }
    rt[multirefs] <-agg[,"mean"]
  }
  return(rt)
}

moleculeMZ <- function(smlTable, smfTable) {
  ## Initialise empty m/z
  mz <- rep(NA_real_, nrow(smlTable))
  
  ## These are simple & fast:
  simplerefs <- !grepl('\\|', smlTable$SMF_ID_REFS)
  simplerefidx <- smlTable[simplerefs, "SMF_ID_REFS"]
  mz[simplerefs] <- as.numeric(smfTable[simplerefidx, "exp_mass_to_charge"])
  
  multirefs <- !simplerefs
  if (any(multirefs)) {
    ## Other metabolites require some (slower) split/list/mean calculation
    multirefindices <- lapply(strsplit(smlTable[multirefs, "SMF_ID_REFS"], "|", fixed=TRUE),
                              trimws)
    intcols=which(grepl("abundance_assay", colnames(smfTable)))
    agg <- t(sapply(multirefindices, function(x) {
      mz <- as.numeric(smfTable[x, "exp_mass_to_charge"])
      intsum <- rowSums(as.matrix(sapply(smfTable[x, intcols], function(y) as.numeric(y))))
      maxind <- which.max(intsum)
      c(minmz=min(mz), maxmz=max(mz), maxintmz=mz[maxind])
    }))
    rownames(agg) <- rownames(smlTable)[multirefs]
    
    mz[multirefs] <-agg[,"maxintmz"]
  }
  return(mz)
}
