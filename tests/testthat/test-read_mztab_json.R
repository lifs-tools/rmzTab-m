context("test-read_mztab_json.R")

test_that("reading of mztab json works", {
  testfile <- system.file("testdata", c("lipidomics-example.mzTab.json"),package="rmzTabM")
  mzTab <- MzTab$new()
  mzTab$fromJSON(testfile)
  #print(mzTab$toJSONString())
  expect_false(is.null(mzTab$metadata))
  expect_equal(mzTab$metadata$`mzTab-version`, "2.0.0-M")
  expect_equal(mzTab$metadata$`mzTab-ID`, "ISAS-2018-1234")
  expect_null(mzTab$metadata$title)
  expect_equal(mzTab$metadata$description, "Minimal proposed sample file for identification and quantification of lipids")
  expect_null(mzTab$metadata$contact)
  expect_length(mzTab$metadata$publication, 1)
  expect_null(mzTab$metadata$uri)
  expect_length(mzTab$metadata$`external_study_uri`, 1)
  expect_length(mzTab$metadata$`instrument`, 1)
  
  expect_null(mzTab$metadata$`quantification_method`$id)
  expect_equal(mzTab$metadata$`quantification_method`$`cv_label`, "MS")
  expect_equal(mzTab$metadata$`quantification_method`$`cv_accession`, "MS:1001838")
  expect_equal(mzTab$metadata$`quantification_method`$`name`, "SRM quantitation analysis")
  
  expect_length(mzTab$metadata$`sample`, 1)
  expect_length(mzTab$metadata$`sample_processing`, 1)
  expect_length(mzTab$metadata$`software`, 2)
  expect_null(mzTab$metadata$`derivatization_agent`)
  
  expect_length(mzTab$metadata$`ms_run`, 1)
  expect_length(mzTab$metadata$`assay`, 1)
  expect_length(mzTab$metadata$`study_variable`, 1)
  
  expect_null(mzTab$metadata$`custom`)
  expect_length(mzTab$metadata$`cv`, 3)
  
  expect_null(mzTab$metadata$`small_molecule-quantification_unit`$id)
  expect_equal(mzTab$metadata$`small_molecule-quantification_unit`$`cv_label`, "UO")
  expect_equal(mzTab$metadata$`small_molecule-quantification_unit`$`cv_accession`, "UO:0000072")
  expect_equal(mzTab$metadata$`small_molecule-quantification_unit`$`name`, "picomolal")
  
  expect_null(mzTab$metadata$`small_molecule_feature-quantification_unit`$id)
  expect_equal(mzTab$metadata$`small_molecule_feature-quantification_unit`$`cv_label`, "UO")
  expect_equal(mzTab$metadata$`small_molecule_feature-quantification_unit`$`cv_accession`, "UO:0000072")
  expect_equal(mzTab$metadata$`small_molecule_feature-quantification_unit`$`name`, "picomolal")
  
  expect_null(mzTab$metadata$`small_molecule-identification_reliability`$id)
  expect_equal(mzTab$metadata$`small_molecule-identification_reliability`$`cv_label`, "MS")
  expect_equal(mzTab$metadata$`small_molecule-identification_reliability`$`cv_accession`, "MS:1002896")
  expect_equal(mzTab$metadata$`small_molecule-identification_reliability`$`name`, "compound identification confidence level")
  
  expect_length(mzTab$metadata$`database`, 3)
  expect_length(mzTab$metadata$`id_confidence_measure`, 1)
  
  expect_null(mzTab$metadata$`colunit-small_molecule`)
  expect_null(mzTab$metadata$`colunit-small_molecule_feature`)
  expect_length(mzTab$metadata$`colunit-small_molecule_evidence`, 1)
  
  expect_null(mzTab$metadata$`colunit-small_molecule_evidence`[[1]]$param$id)
  expect_equal(mzTab$metadata$`colunit-small_molecule_evidence`[[1]]$`column_name`, "opt_global_mass_error")
  expect_equal(mzTab$metadata$`colunit-small_molecule_evidence`[[1]]$param$`cv_label`, "UO")
  expect_equal(mzTab$metadata$`colunit-small_molecule_evidence`[[1]]$param$`cv_accession`, "UO:0000169")
  expect_equal(mzTab$metadata$`colunit-small_molecule_evidence`[[1]]$param$`name`, "parts per million")
  expect_null(mzTab$metadata$`colunit-small_molecule_evidence`[[1]]$param$`value`)
  
  expect_false(is.null(mzTab$smallMoleculeSummary))
  expect_false(is.null(mzTab$smallMoleculeFeature))
  expect_false(is.null(mzTab$smallMoleculeEvidence))
  expect_length(mzTab$`smallMoleculeSummary`,1)
  expect_length(mzTab$`smallMoleculeEvidence`,4)
  expect_length(mzTab$`smallMoleculeFeature`,4)
})

