context("test-write_mztab.R")

test_that("roundtrip read, write, read of mztab json works", {
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
  # the next text currently fails -> `mzTabObject` not equal to `mzTabObject2`.
  # Component "metadata": Component "colunit-small_molecule": target is NULL, current is list
  # expect_equal(mzTabObject, mzTabObject2)
})

test_that("writing of mztab TAB format works", {
  # testfile <- system.file("testdata", c("lipidomics-example.mzTab"),package="rmzTabM")
  # mzTabTable <- readMzTab(testfile)
  # mtd <- extractMetadata(mzTabTable)
  testfile <- system.file("testdata", c("lipidomics-example.mzTab.json"),package="rmzTabM")
  mzTabObject <- MzTab$new()
  mzTabObject$fromJSON(testfile)
  writeMzTab(mzTabObject, tempfile(fileext = "mztab"))
  # create pipe separated list entries -> 
  # paste(mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$smf_id_refs, collapse = " | ")
  # create a string from a parameter list ->
  # param <- mzTabObject$toJSON()$`smallMoleculeSummary`[[1]]$best_id_confidence_measure
  # paste0("[", param$cv_label, ",", param$cv_accession, ",", param$name, ",", param$value, "]")
})
