context("test-read_mztab.R")

test_that("reading of mztab JSON format works", {
  testfile <- system.file("testdata", c("MTBLS263.mztab.json"), package="rmzTabM")
  mzTabObject <- MzTab$new()
  mzTabObject$fromJSON(testfile)
  
  expect_false(is.null(mzTabObject$metadata))
  expect_false(is.null(mzTabObject$smallMoleculeSummary))
  expect_false(is.null(mzTabObject$smallMoleculeFeature))
  expect_false(is.null(mzTabObject$smallMoleculeEvidence))

  expect_equal(mzTabObject$`metadata`$`mzTab-version`, "2.0.0-M")
  expect_equal(mzTabObject$`metadata`$`mzTab-ID`, "JetBike Test")
  expect_equal(mzTabObject$`metadata`$`description`, NULL)
    
  expect_length(mzTabObject$`smallMoleculeSummary`, 17)
  expect_length(mzTabObject$`smallMoleculeFeature`, 19)
  expect_length(mzTabObject$`smallMoleculeEvidence`, 19)
  
  expect_equal(mzTabObject$metadata$`quantification_method`$`cv_accession`, "MS:1001834")
  expect_equal(mzTabObject$metadata$`study_variable`[[1]]$`name`, "Replicates")
  expect_length(mzTabObject$metadata$`study_variable`[[1]]$`assay_refs`, 1)
  expect_length(mzTabObject$metadata$`ms_run`, 6)
  expect_length(mzTabObject$metadata$`assay`, 6)
})

test_that("reading of mzTab TAB format of MTBLS263 works", {
  testfile <- system.file("testdata", c("MTBLS263.mztab"), package="rmzTabM")
  mzTabTable <- readMzTab(testfile)
  
  # sml.table <- extractSmallMoleculeSummary(mzTabTable)
  # browser()
  mzTabObject <- MzTab$new()
  mzTabObject$fromDataFrame(mzTabTable)
  
})

test_that("reading of mzTab TAB format from MS-Dial 4.12 (mzTab exporter 1.05) works", {
  testfile <- system.file("testdata", c("lcmsms_dda_hydrophilic_height_mzTab.mztab"), package="rmzTabM")
  mzTabTable <- readMzTab(testfile)

  mzTabObject <- MzTab$new()
  mzTabObject$fromDataFrame(mzTabTable)

  sml.table <- extractSmallMoleculeSummary(mzTabTable)
  expect_equal(object = nrow(sml), 5144)
  expect_equal(object = ncol(sml), 32)
  
  ## Parse input without SME section, https://github.com/lifs-tools/rmzTab-m/issues/28
  smeindex <- mzTabTable[,1]=="SEH" | mzTabTable[,1]=="SME"
  mzTabTableWithoutSME <- mzTabTable[!smeindex, ]
  sme <- extractTable(mzTabTableWithoutSME, "SEH", "SME")
  expect_null(sme) # Without Any SEH/SME lines don't do anything
  
  smeindex <- mzTabTable[,1]=="SME" ## Header but no content
  mzTabTableWithoutSME <- mzTabTable[!smeindex, ]
  sme <- extractTable(mzTabTableWithoutSME, "SEH", "SME")
  expect_equal(object = nrow(sme), 0)
})

test_that("reading of lipidomics-example mzTab TAB format works", {
  testfile <- system.file("testdata", c("lipidomics-example.mzTab"), package="rmzTabM")
  mzTabTable <- readMzTab(testfile)
  
  # sml.table <- extractSmallMoleculeSummary(mzTabTable)
  
  mzTabObject <- MzTab$new()
  mzTabObject$fromDataFrame(mzTabTable)
  
  expect_false(is.null(mzTabObject$metadata))
  expect_false(is.null(mzTabObject$smallMoleculeSummary))
  expect_false(is.null(mzTabObject$smallMoleculeFeature))
  expect_false(is.null(mzTabObject$smallMoleculeEvidence))

  expect_equal(object = mzTabObject$`metadata`$`mzTab-version`, "2.0.0-M")
  expect_equal(object = mzTabObject$`metadata`$`mzTab-ID`, "ISAS-2018-1234")
  expect_equal(object = mzTabObject$`metadata`$`description`, "Minimal proposed sample file for identification and quantification of lipids")

  expect_length(mzTabObject$`smallMoleculeSummary`,1)
  expect_length(mzTabObject$`smallMoleculeFeature`,4)
  expect_length(mzTabObject$`smallMoleculeEvidence`,4)
  
  expect_false(is.null(mzTabObject$metadata$study_variable))
  expect_equal(mzTabObject$metadata$study_variable[[1]]$name, "Sphingolipid SRM Quantitation")
  expect_length(mzTabObject$metadata$study_variable[[1]]$assay_refs, 1)
  
  expect_length(mzTabObject$metadata$`ms_run`, 1)
  expect_equal(mzTabObject$metadata$`ms_run`[[1]]$`instrument_ref`, 1)
  expect_length(mzTabObject$metadata$`assay`, 1)
  expect_equal(mzTabObject$metadata$assay[[1]]$`sample_ref`, 1)
  expect_length(mzTabObject$metadata$assay[[1]]$`ms_run_ref`, 1)
  expect_equal(mzTabObject$metadata$assay[[1]]$`ms_run_ref`[[1]], 1)
  expect_length(mzTabObject$metadata$`database`, 3)
  
  expect_length(mzTabObject$metadata$software, 2)
  expect_length(mzTabObject$metadata$software[[1]]$setting, 2)
  expect_equal(mzTabObject$metadata$software[[1]]$setting[[1]], "ScheduledSRMWindow: 2 min")
  expect_equal(mzTabObject$metadata$software[[1]]$setting[[2]], "CycleTime: 2 s")
  
  expect_equal(mzTabObject$metadata$instrument[[1]]$source$name, "Electrospray Ionization")
  expect_length(mzTabObject$metadata$instrument[[1]]$analyzer, 1)
  
  expect_equal(mzTabObject$metadata$`small_molecule-quantification_unit`$`cv_accession`, "UO:0000072")
  
  expect_false(is.null(mzTabObject$smallMoleculeSummary[[1]]$abundance_assay))
  
  expect_equal(mzTabObject$smallMoleculeEvidence[[1]]$spectra_ref[[1]]$ms_run, 1)
  expect_equal(mzTabObject$smallMoleculeEvidence[[1]]$spectra_ref[[1]]$reference, "controllerType=0 controllerNumber=1 scan=731")
  
  # create pipe separated list entries -> 
  # paste(mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$smf_id_refs, collapse = " | ")
  # create a string from a parameter list ->
  # param <- mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$best_id_confidence_measure
  # paste0("[", param$cv_label, ",", param$cv_accession, ",", param$name, ",", param$value, "]")
})
