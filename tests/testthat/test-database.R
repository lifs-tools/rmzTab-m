# Automatically generated by openapi-generator (https://openapi-generator.tech)
# Please update as you see appropriate

context("Test Database")

model.instance <- Database$new()

ref.json <- '{
      "id" : 1,
      "param" : {
        "id" : null,
        "cv_label" : "",
        "cv_accession" : "",
        "name" : "Pubchem",
        "value" : null
      },
      "prefix" : "PUBCHEM-CPD",
      "version" : "02.12.2017",
      "uri" : "https://www.ncbi.nlm.nih.gov/pccompound"
    }'

test_that("Database$fromJSONString() works", {
  
  model.instance <- model.instance$fromJSONString(ref.json)
  expect_equal(model.instance$`id`, 1)
  expect_null(model.instance$`param`$`id`)
  expect_equal(model.instance$`param`$`cv_label`, '')
  expect_equal(model.instance$`param`$`cv_accession`, '')
  expect_equal(model.instance$`param`$`name`, 'Pubchem')
  expect_null(model.instance$`param`$`value`)
  expect_equal(model.instance$`prefix`, "PUBCHEM-CPD")
  expect_equal(model.instance$`version`, "02.12.2017")
  expect_equal(model.instance$`uri`, "https://www.ncbi.nlm.nih.gov/pccompound")
  }
)

test_that("Database$fromDataFrame() works", {
  testfile <- system.file("testdata", c("lipidomics-example.mzTab"), package="rmzTabM")
  mzTabTable <- readMzTab(testfile)
  metadataTable <- extractMetadata(mzTabTable)
  idElements <- extractIdElements(metadataTable, "database", "param")

  model.instance <- Database$new()
  model.instance$fromDataFrame(idElements[[1]])

  expect_equal(model.instance$`id`, 1)
  expect_null(model.instance$`param`$`id`)
  expect_equal(model.instance$`param`$`cv_label`, '')
  expect_equal(model.instance$`param`$`cv_accession`, '')
  expect_equal(model.instance$`param`$`name`, 'Pubchem')
  expect_null(model.instance$`param`$`value`)
  expect_equal(model.instance$`prefix`, "PUBCHEM-CPD")
  expect_equal(model.instance$`version`, "02.12.2017")
  expect_equal(model.instance$`uri`, "https://www.ncbi.nlm.nih.gov/pccompound")
}
)
