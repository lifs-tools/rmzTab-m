context("test-read_mztab.R")

test_that("reading of mztab TAB format works", {
  testfile <- system.file("testdata", c("lipidomics-example.mzTab"), package="rmzTabM")
  mzTabTable <- readMzTab(testfile)
  
  sml.table <- extractSmallMoleculeSummary(mzTabTable)
  browser()
  
  mzTabObject <- MzTab$new()
  mzTabObject$fromDataFrame(mzTabTable)
  
  expect_false(is.null(mzTabObject$metadata))
  expect_false(is.null(mzTabObject$smallMoleculeSummary))
  expect_false(is.null(mzTabObject$smallMoleculeFeature))
  expect_false(is.null(mzTabObject$smallMoleculeEvidence))

  expect_equal(object = mzTabObject$`metadata`$`mzTab-version`, "2.0.0-M")
  expect_equal(object = mzTabObject$`metadata`$`mzTab-ID`, "ISAS-2018-1234")
  expect_equal(object = mzTabObject$`metadata`$`description`, "Minimal proposed sample file for identification and quantification of lipids")

  #cat(mzTabObject2$toJSONString())
  expect_length(mzTabObject$`smallMoleculeSummary`,1)
  expect_length(mzTabObject$`smallMoleculeFeature`,4)
  expect_length(mzTabObject$`smallMoleculeEvidence`,4)
  
  # expect_false(is.null(mzTabObject2$smallMoleculeSummary))
  # expect_false(is.null(mzTabObject2$smallMoleculeFeature))
  # expect_false(is.null(mzTabObject2$smallMoleculeEvidence))
  
  # expect_length(mzTabObject2$`smallMoleculeSummary`,1)
  # expect_length(mzTabObject2$`smallMoleculeFeature`,4)
  # expect_length(mzTabObject2$`smallMoleculeEvidence`,4)
  
  # create pipe separated list entries -> 
  # paste(mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$smf_id_refs, collapse = " | ")
  # create a string from a parameter list ->
  # param <- mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$best_id_confidence_measure
  # paste0("[", param$cv_label, ",", param$cv_accession, ",", param$name, ",", param$value, "]")
})
