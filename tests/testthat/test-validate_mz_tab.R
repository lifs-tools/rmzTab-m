context("Test ValidateMzTab")

test_that("validation of mzTab Object via REST API of mztab JSON works", {
  skip_if_not(apps_lifstools_statusOK, "apps.lifs-tools.org not reachable")
  
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

test_that("validation of mzTab file as string via REST API of mztab TSV works", {
  skip_if_not(apps_lifstools_statusOK, "apps.lifs-tools.org not reachable")
  
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

# this test needs a new version of the mzTab Validator web app
# test_that("validation of mzTab Object via REST API of mztab TSV works", {
#   testfile <-
#     system.file("testdata", c("lipidomics-example.mzTab"), package = "rmzTabM")
#   mzTabTable <- readMzTab(testfile)
#   mzTabObject <- MzTab$new()
#   mzTabObject$fromDataFrame(mzTabTable)
#   
#   validationMessages <- validateMzTab(
#     mzTabObject,
#     validationMode = "json",
#     validationLevel = "info",
#     maxErrors = 100,
#     semanticValidation = FALSE
#   )
#   expect_equal(length(validationMessages), 5)
# })
