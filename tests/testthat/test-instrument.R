# Automatically generated by openapi-generator (https://openapi-generator.tech)
# Please update as you see appropriate

context("Test Instrument")

model.instance <- Instrument$new()

ref.json <- '{
      "id" : 1,
      "name" : {
        "id" : null,
        "cv_label" : "MS",
        "cv_accession" : "MS:1001911",
        "name" : "Q Exactive",
        "value" : null
      },
      "source" : {
        "id" : null,
        "cv_label" : "MS",
        "cv_accession" : "MS:1000073",
        "name" : "electrospray ionization",
        "value" : null
      },
      "analyzer" : [ {
        "id" : null,
        "cv_label" : "MS",
        "cv_accession" : "MS:1000081",
        "name" : "quadrupole",
        "value" : null
      }, {
        "id" : null,
        "cv_label" : "MS",
        "cv_accession" : "MS:1000484",
        "name" : "orbitrap",
        "value" : null
      } ],
      "detector" : {
        "id" : null,
        "cv_label" : "MS",
        "cv_accession" : "MS:1000624",
        "name" : "inductive detector",
        "value" : null
      }
    }'

test_that("Instrument$fromJSONString() works", {
  
  model.instance <- model.instance$fromJSONString(ref.json)
  expect_equal(model.instance$`id`, 1)
  expect_null(model.instance$`name`$`id`)
  expect_equal(model.instance$`name`$`cv_label`, "MS")
  expect_equal(model.instance$`name`$`cv_accession`, "MS:1001911")
  expect_equal(model.instance$`name`$`name`, "Q Exactive")
  expect_null(model.instance$`name`$`value`)
  expect_null(model.instance$`source`$`id`)
  expect_equal(model.instance$`source`$`cv_label`, "MS")
  expect_equal(model.instance$`source`$`cv_accession`, "MS:1000073")
  expect_equal(model.instance$`source`$`name`, "electrospray ionization")
  expect_null(model.instance$`source`$`value`)
  expect_equal(length(model.instance$`analyzer`), 2)
  expect_null(model.instance$`analyzer`[[1]]$`id`)
  expect_equal(model.instance$`analyzer`[[1]]$`cv_label`, "MS")
  expect_equal(model.instance$`analyzer`[[1]]$`cv_accession`, "MS:1000081")
  expect_equal(model.instance$`analyzer`[[1]]$`name`, "quadrupole")
  expect_null(model.instance$`analyzer`[[1]]$`value`)
  expect_null(model.instance$`analyzer`[[2]]$`id`)
  expect_equal(model.instance$`analyzer`[[2]]$`cv_label`, "MS")
  expect_equal(model.instance$`analyzer`[[2]]$`cv_accession`, "MS:1000484")
  expect_equal(model.instance$`analyzer`[[2]]$`name`, "orbitrap")
  expect_null(model.instance$`analyzer`[[2]]$`value`)
  
  expect_equal(model.instance$`detector`$`cv_label`, "MS")
  expect_equal(model.instance$`detector`$`cv_accession`, "MS:1000624")
  expect_equal(model.instance$`detector`$`name`, "inductive detector")
  expect_null(model.instance$`detector`$`value`)
  
  restored.model.instance <- Instrument$new()
  restored.model.instance$fromJSONString(model.instance$toJSONString())
  expect_equal(model.instance, restored.model.instance)
})

test_that("Instrument$fromDataFrame() works", {
  instrumentMtd <- 
    '
MTD\tinstrument[1]-name\t[MS, MS:1002581, QTRAP 6500 , ]
MTD\tinstrument[1]-source\t[MS, MS:1000073, Electrospray Ionization, ]
MTD\tinstrument[1]-analyzer[1]\t[MS, MS:1000082, quadrupole ion trap, ]
MTD\tinstrument[1]-analyzer[2]\t[MS, MS:1000484, orbitrap, ]
MTD\tinstrument[1]-detector\t[MS, MS:1000624, inductive detector, ]
'
  mzTabTable <- readMzTabString(instrumentMtd)
  metadataTable <- extractMetadata(mzTabTable)
  idElements <- extractIdElements(metadataTable, "instrument", "name")
  model.instance <- Instrument$new()
  model.instance$fromDataFrame(idElements[[1]])
  
  expect_equal(model.instance$`id`, 1)
  expect_equal(model.instance$`name`$cv_label, 'MS')
  expect_equal(model.instance$`name`$cv_accession, 'MS:1002581')
  expect_equal(model.instance$`name`$name, 'QTRAP 6500')
  expect_null(model.instance$`name`$value)
  
  expect_equal(model.instance$`source`$cv_label, 'MS')
  expect_equal(model.instance$`source`$cv_accession, 'MS:1000073')
  expect_equal(model.instance$`source`$name, 'Electrospray Ionization')
  expect_null(model.instance$`source`$value)
  
  expect_equal(length(model.instance$`analyzer`), 2)
  expect_equal(model.instance$`analyzer`[[1]]$cv_label, "MS")
  expect_equal(model.instance$`analyzer`[[1]]$cv_accession, "MS:1000082")
  expect_equal(model.instance$`analyzer`[[1]]$name, "quadrupole ion trap")
  expect_null(model.instance$`analyzer`[[1]]$value)
  
  expect_equal(model.instance$`analyzer`[[2]]$cv_label, "MS")
  expect_equal(model.instance$`analyzer`[[2]]$cv_accession, "MS:1000484")
  expect_equal(model.instance$`analyzer`[[2]]$name, "orbitrap")
  expect_null(model.instance$`analyzer`[[2]]$value)
  
  expect_equal(model.instance$`detector`$cv_label, "MS")
  expect_equal(model.instance$`detector`$cv_accession, "MS:1000624")
  expect_equal(model.instance$`detector`$name, "inductive detector")
  expect_null(model.instance$`detector`$value)
})

