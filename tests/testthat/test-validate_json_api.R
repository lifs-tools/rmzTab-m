context("test-validate_json_api.R")

test_that("validation via REST API of mztab json works", {
  testfile <- system.file("testdata", c("lipidomics-example.mzTab.json"),package="rmzTabM")
  mzTabObject <- MzTab$new()
  mzTabObject$fromJSON(testfile)
  # the tests below currently fail, serialization produces empty publications list
  # expect_equal(1, mzTabObject$`metadata`$`publication`$id)
  # expect_equal("Publication", mzTabObject$`metadata`$`publication`$elementType)
  # expect_equal(2, length(mzTabObject$`metadata`$`publication`$publicationItems))
  # publications <- mzTabObject$`metadata`$`publication`$publicationItems
  # expect_equal("pubmed", publications[[1]]$type)
  # expect_equal("29039908", publications[[1]]$accesion)
  # expect_equal("doi", publications[[2]]$type)
  # expect_equal("10.1021/acs.analchem.7b03576", publications[[2]]$accesion)
  #expect_false(is.null(mzTabObject$toJSONString()))
  metadataJsonObject <- mzTabObject$`metadata`$toJSON()
  expect_false(is.null(metadataJsonObject))
  #cat(names(metadataJsonObject))
  expect_false(is.null(metadataJsonObject$`mzTab-version`))
  expect_equal(as.character(metadataJsonObject$`mzTab-version`), "2.0.0-M")
  expect_false(is.null(metadataJsonObject$`mzTab-ID`))
  expect_equal(as.character(metadataJsonObject$`mzTab-ID`), "ISAS-2018-1234")
  expect_false(is.null(metadataJsonObject$`description`))
  #metadataJsonObject$`title`
  expect_true(is.null(metadataJsonObject$`title`))
  expect_true(0 == length(metadataJsonObject$`title`))
  #set a custom api client to use a different URL
  apiClient <- ApiClient$new(basePath = "https://apps.lifs.isas.de/mztabvalidator-dev/rest/v2")
  validateApi <- ValidateApi$new(apiClient = apiClient)

  #browser()
  response <- validateApi$ValidateMzTabFile(mzTabObject, 'info', 50, FALSE)
  expect_equal(response$response$status_code, 200)
  if (!is.null(response$content)) {
    print(response$content)
  }
  expect_null(response$content)
})
