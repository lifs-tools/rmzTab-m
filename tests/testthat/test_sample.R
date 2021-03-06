# Automatically generated by openapi-generator (https://openapi-generator.tech)
# Please update as you see appropriate

context("Test Sample")

model.instance <- Sample$new()

test_that("id", {
  # tests for the property `id` (integer)

  # uncomment below to test the property 
  #expect_equal(model.instance$`id`, "EXPECTED_RESULT")
})

test_that("elementType", {
  # tests for the property `elementType` (character)

  # uncomment below to test the property 
  #expect_equal(model.instance$`elementType`, "EXPECTED_RESULT")
})

test_that("name", {
  # tests for the property `name` (character)
  # The sample&#39;s name.

  # uncomment below to test the property 
  #expect_equal(model.instance$`name`, "EXPECTED_RESULT")
})

test_that("custom", {
  # tests for the property `custom` (array[Parameter])
  # Additional user or cv parameters.

  # uncomment below to test the property 
  #expect_equal(model.instance$`custom`, "EXPECTED_RESULT")
})

test_that("species", {
  # tests for the property `species` (array[Parameter])
  # Biological species information on the sample.

  # uncomment below to test the property 
  #expect_equal(model.instance$`species`, "EXPECTED_RESULT")
})

test_that("tissue", {
  # tests for the property `tissue` (array[Parameter])
  # Biological tissue information on the sample.

  # uncomment below to test the property 
  #expect_equal(model.instance$`tissue`, "EXPECTED_RESULT")
})

test_that("cell_type", {
  # tests for the property `cell_type` (array[Parameter])
  # Biological cell type information on the sample.

  # uncomment below to test the property 
  #expect_equal(model.instance$`cell_type`, "EXPECTED_RESULT")
})

test_that("disease", {
  # tests for the property `disease` (array[Parameter])
  # Disease information on the sample.

  # uncomment below to test the property 
  #expect_equal(model.instance$`disease`, "EXPECTED_RESULT")
})

test_that("description", {
  # tests for the property `description` (character)
  # A free form description of the sample.

  # uncomment below to test the property 
  #expect_equal(model.instance$`description`, "EXPECTED_RESULT")
})

