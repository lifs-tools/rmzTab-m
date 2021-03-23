context("Test ValidateMzTab")

test_that("validation of mzTab Object via REST API of mztab JSON works", {
  testfile <-
    system.file("testdata", c("lipidomics-example.mzTab.json"), package = "rmzTabM")
  mzTabObject <- MzTab$new()
  mzTabObject$fromJSON(testfile)
  
  validationMessages <- validateMzTab(
    mzTabObject,
    validationMode = "json",
    validationLevel = "info",
    maxErrors = 100,
    semanticValidation = FALSE
  )
  
  expect_equal(length(validationMessages), 0)
})

test_that("validation of mzTab Object via REST API of mztab TSV works", {
  testfile <-
    system.file("testdata", c("lipidomics-example.mzTab"), package = "rmzTabM")
  mzTabString <- readChar(testfile, file.info(testfile)$size)
  
  validationMessages <- validateMzTab(
    mzTabString,
    validationMode = "plain",
    validationLevel = "info",
    maxErrors = 100,
    semanticValidation = FALSE
  )
  
  expect_equal(length(validationMessages), 0)
})
