context("test-read_mztab_json.R")

test_that("reading of mztab json works", {
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
  expect_length(mzTabObject$`smallMoleculeEvidence`,4)
  expect_length(mzTabObject$`smallMoleculeFeature`,4)
})

