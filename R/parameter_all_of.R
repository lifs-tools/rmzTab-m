# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title ParameterAllOf
#' @description ParameterAllOf Class
#' @format An \code{R6Class} generator object
#' @field cv_label  character [optional]
#'
#' @field cv_accession  character [optional]
#'
#' @field name  character 
#'
#' @field value  character 
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ParameterAllOf <- R6::R6Class(
  'ParameterAllOf',
  public = list(
    `cv_label` = NULL,
    `cv_accession` = NULL,
    `name` = NULL,
    `value` = NULL,
    initialize = function(`name`, `value`, `cv_label`='', `cv_accession`='', ...){
      local.optional.var <- list(...)
      if (!missing(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!missing(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!is.null(`cv_label`)) {
        stopifnot(is.character(`cv_label`), length(`cv_label`) == 1)
        self$`cv_label` <- `cv_label`
      }
      if (!is.null(`cv_accession`)) {
        stopifnot(is.character(`cv_accession`), length(`cv_accession`) == 1)
        self$`cv_accession` <- `cv_accession`
      }
    },
    toJSON = function() {
      ParameterAllOfObject <- list()
      if (!is.null(self$`cv_label`)) {
        ParameterAllOfObject[['cv_label']] <-
          self$`cv_label`
      }
      if (!is.null(self$`cv_accession`)) {
        ParameterAllOfObject[['cv_accession']] <-
          self$`cv_accession`
      }
      if (!is.null(self$`name`)) {
        ParameterAllOfObject[['name']] <-
          self$`name`
      }
      if (!is.null(self$`value`)) {
        ParameterAllOfObject[['value']] <-
          self$`value`
      }

      ParameterAllOfObject
    },
    fromJSON = function(ParameterAllOfJson) {
      ParameterAllOfObject <- jsonlite::fromJSON(ParameterAllOfJson)
      if (!is.null(ParameterAllOfObject$`cv_label`)) {
        self$`cv_label` <- ParameterAllOfObject$`cv_label`
      }
      if (!is.null(ParameterAllOfObject$`cv_accession`)) {
        self$`cv_accession` <- ParameterAllOfObject$`cv_accession`
      }
      if (!is.null(ParameterAllOfObject$`name`)) {
        self$`name` <- ParameterAllOfObject$`name`
      }
      if (!is.null(ParameterAllOfObject$`value`)) {
        self$`value` <- ParameterAllOfObject$`value`
      }
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`cv_label`)) {
        sprintf(
        '"cv_label":
          "%s"
                ',
        self$`cv_label`
        )},
        if (!is.null(self$`cv_accession`)) {
        sprintf(
        '"cv_accession":
          "%s"
                ',
        self$`cv_accession`
        )},
        if (!is.null(self$`name`)) {
        sprintf(
        '"name":
          "%s"
                ',
        self$`name`
        )},
        if (!is.null(self$`value`)) {
        sprintf(
        '"value":
          "%s"
                ',
        self$`value`
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(ParameterAllOfJson) {
      ParameterAllOfObject <- jsonlite::fromJSON(ParameterAllOfJson)
      self$`cv_label` <- ParameterAllOfObject$`cv_label`
      self$`cv_accession` <- ParameterAllOfObject$`cv_accession`
      self$`name` <- ParameterAllOfObject$`name`
      self$`value` <- ParameterAllOfObject$`value`
      self
    }
  )
)
