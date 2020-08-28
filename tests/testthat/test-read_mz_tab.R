context("test-read_mztab.R")

test_that("reading of mztab TAB format works", {
  # testfile <- system.file("testdata", c("lipidomics-example.mzTab"),package="rmzTabM")
  # mzTabTable <- readMzTab(testfile)
  # mtd <- extractMetadata(mzTabTable)
  testfile <- system.file("testdata", c("lipidomics-example.mzTab"), package="rmzTabM")
  mzTabObject <- MzTab$new()
  mzTabTable <- readMzTab(testfile)
  mtd <- extractMetadata(mzTabTable)
  browser()
  sml <- extractSummary(mzTabTable)
  smf <- extractFeatures(mzTabTable)
  sme <- extractEvidence(mzTabTable)
  # create pipe separated list entries -> 
  # paste(mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$smf_id_refs, collapse = " | ")
  # create a string from a parameter list ->
  # param <- mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$best_id_confidence_measure
  # paste0("[", param$cv_label, ",", param$cv_accession, ",", param$name, ",", param$value, "]")
})
