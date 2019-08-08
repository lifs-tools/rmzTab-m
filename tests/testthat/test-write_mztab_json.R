context("test-write_mztab_json.R")

test_that("reading and writing of mztab json works", {
  testfile <- system.file("testdata", c("lipidomics-example.mzTab.json"),package="rmzTabM")
  mzTabObject <- MzTab$new()
  mzTabObject$fromJSON(testfile)
  #print(mzTabObject$toJSONString())
  expect_false(is.null(mzTabObject$metadata))
  expect_false(is.null(mzTabObject$smallMoleculeSummary))
  expect_false(is.null(mzTabObject$smallMoleculeFeature))
  expect_false(is.null(mzTabObject$smallMoleculeEvidence))
  expect_equal(object = mzTabObject$`metadata`$`mzTab-ID`, "ISAS-2018-1234")
  expect_length(mzTabObject$`smallMoleculeSummary`,1)
  expect_length(mzTabObject$`smallMoleculeFeature`,4)
  expect_length(mzTabObject$`smallMoleculeEvidence`,4)
  tmpfile <- file.path("/","tmp","rmztab-test-write_mztab_json.mzTab")
  writeMzTabJSONToFile(mzTabObject, tmpfile)
  mzTabObject2 <- MzTab$new()
  mzTabObject2$fromJSON(tmpfile)
  expect_false(is.null(mzTabObject2$metadata))
  expect_false(is.null(mzTabObject2$smallMoleculeSummary))
  expect_false(is.null(mzTabObject2$smallMoleculeFeature))
  expect_false(is.null(mzTabObject2$smallMoleculeEvidence))
  #cat(mzTabObject2$toJSONString())
  expect_equal(object = mzTabObject2$`metadata`$`mzTab-ID`, "ISAS-2018-1234")
  expect_length(mzTabObject2$`smallMoleculeSummary`,1)
  expect_length(mzTabObject2$`smallMoleculeFeature`,4)
  expect_length(mzTabObject2$`smallMoleculeEvidence`,4)
})

#' @field mzTab-version 
#' @field mzTab-ID 
#' @field title 
#' @field description 
#' @field sample_processing 
#' @field instrument 
#' @field software 
#' @field publication 
#' @field contact 
#' @field uri 
#' @field external_study_uri 
#' @field quantification_method 
#' @field study_variable 
#' @field ms_run 
#' @field assay 
#' @field sample 
#' @field custom 
#' @field cv 
#' @field database 
#' @field derivatization_agent 
#' @field small_molecule-quantification_unit 
#' @field small_molecule_feature-quantification_unit 
#' @field small_molecule-identification_reliability 
#' @field id_confidence_measure 
#' @field colunit-small_molecule 
#' @field colunit-small_molecule_feature 
#' @field colunit-small_molecule_evidence 
# test_that("conversion of metadata section to table works", {
#   metadata <- Metadata$new()
#   metadata$`mzTab-version` <- "2.0.0-M";
#   metadata$`mzTab-ID` <- "justatestid";
#   metadata$`title` <- "A test title";
#   metadata$`description` <- "A test description";
#   metadata$`sample_processing` <- list();
#   metadata$`instrument` <- list();
#   metadata$`software` <- list();
#   metadata$`publication` <- list();
#   metadata$`contact` <- list();
#   metadata$`uri` <- "";
#   metadata$`external_study_uri` <- "";
#   metadata$`quantification_method` <- Parameter$new();
#   metadata$`study_variable` <- list();
#   metadata$`ms_run` <- list();
#   metadata$`assay` <- list();
#   metadata$`sample` <- list();
#   metadata$`custom` <- list();
#   metadata$`cv` <- list();
#   metadata$`database` <- list();
#   metadata$`derivatization_agent` <- list();
#   metadata$`small_molecule-quantification_unit` <- Parameter$new();
#   metadata$`small_molecule_feature-quantification_unit` <- Parameter$new();
#   metadata$`small_molecule-identification_reliability` <- Parameter$new();
#   metadata$`id_confidence_measure` <- Parameter$new();
#   metadata$`colunit-small_molecule` <- list();
#   metadata$`colunit-small_molecule_feature` <- list();
#   metadata$`colunit-small_molecule_evidence` <- list();
#   metaDataTable <- as.data.frame(metadata)
#   utils::write.table(metaDataTable, file=filename,
#                      row.names=FALSE, col.names=FALSE,
#                      quote=TRUE, sep="\t", na="\"\"")
# })
