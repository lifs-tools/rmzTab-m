# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title InstrumentAllOf
#' @description InstrumentAllOf Class
#' @format An \code{R6Class} generator object
#' @field name  \link{Parameter} [optional]
#'
#' @field source  \link{Parameter} [optional]
#'
#' @field analyzer  list( \link{Parameter} ) [optional]
#'
#' @field detector  \link{Parameter} [optional]
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
InstrumentAllOf <- R6::R6Class(
  'InstrumentAllOf',
  public = list(
    `name` = NULL,
    `source` = NULL,
    `analyzer` = NULL,
    `detector` = NULL,
    initialize = function(`name`=NULL, `source`=NULL, `analyzer`=NULL, `detector`=NULL, ...){
      local.optional.var <- list(...)
      if (!is.null(`name`)) {
        stopifnot(R6::is.R6(`name`))
        self$`name` <- `name`
      }
      if (!is.null(`source`)) {
        stopifnot(R6::is.R6(`source`))
        self$`source` <- `source`
      }
      if (!is.null(`analyzer`)) {
        stopifnot(is.vector(`analyzer`), length(`analyzer`) != 0)
        sapply(`analyzer`, function(x) stopifnot(R6::is.R6(x)))
        self$`analyzer` <- `analyzer`
      }
      if (!is.null(`detector`)) {
        stopifnot(R6::is.R6(`detector`))
        self$`detector` <- `detector`
      }
    },
    toJSON = function() {
      InstrumentAllOfObject <- list()
      if (!is.null(self$`name`)) {
        InstrumentAllOfObject[['name']] <-
          self$`name`$toJSON()
      }
      if (!is.null(self$`source`)) {
        InstrumentAllOfObject[['source']] <-
          self$`source`$toJSON()
      }
      if (!is.null(self$`analyzer`)) {
        InstrumentAllOfObject[['analyzer']] <-
          lapply(self$`analyzer`, function(x) x$toJSON())
      }
      if (!is.null(self$`detector`)) {
        InstrumentAllOfObject[['detector']] <-
          self$`detector`$toJSON()
      }

      InstrumentAllOfObject
    },
    fromJSON = function(InstrumentAllOfJson) {
      InstrumentAllOfObject <- jsonlite::fromJSON(InstrumentAllOfJson)
      if (!is.null(InstrumentAllOfObject$`name`)) {
        nameObject <- Parameter$new()
        nameObject$fromJSON(jsonlite::toJSON(InstrumentAllOfObject$name, auto_unbox = TRUE, digits = NA))
        self$`name` <- nameObject
      }
      if (!is.null(InstrumentAllOfObject$`source`)) {
        sourceObject <- Parameter$new()
        sourceObject$fromJSON(jsonlite::toJSON(InstrumentAllOfObject$source, auto_unbox = TRUE, digits = NA))
        self$`source` <- sourceObject
      }
      if (!is.null(InstrumentAllOfObject$`analyzer`)) {
        self$`analyzer` <- ApiClient$new()$deserializeObj(InstrumentAllOfObject$`analyzer`, "array[Parameter]", loadNamespace("rmzTabM"))
      }
      if (!is.null(InstrumentAllOfObject$`detector`)) {
        detectorObject <- Parameter$new()
        detectorObject$fromJSON(jsonlite::toJSON(InstrumentAllOfObject$detector, auto_unbox = TRUE, digits = NA))
        self$`detector` <- detectorObject
      }
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`name`)) {
        sprintf(
        '"name":
        %s
        ',
        jsonlite::toJSON(self$`name`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`source`)) {
        sprintf(
        '"source":
        %s
        ',
        jsonlite::toJSON(self$`source`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`analyzer`)) {
        sprintf(
        '"analyzer":
        [%s]
',
        paste(sapply(self$`analyzer`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox=TRUE, digits = NA)), collapse=",")
        )},
        if (!is.null(self$`detector`)) {
        sprintf(
        '"detector":
        %s
        ',
        jsonlite::toJSON(self$`detector`$toJSON(), auto_unbox=TRUE, digits = NA)
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(InstrumentAllOfJson) {
      InstrumentAllOfObject <- jsonlite::fromJSON(InstrumentAllOfJson)
      self$`name` <- Parameter$new()$fromJSON(jsonlite::toJSON(InstrumentAllOfObject$name, auto_unbox = TRUE, digits = NA))
      self$`source` <- Parameter$new()$fromJSON(jsonlite::toJSON(InstrumentAllOfObject$source, auto_unbox = TRUE, digits = NA))
      self$`analyzer` <- ApiClient$new()$deserializeObj(InstrumentAllOfObject$`analyzer`, "array[Parameter]", loadNamespace("rmzTabM"))
      self$`detector` <- Parameter$new()$fromJSON(jsonlite::toJSON(InstrumentAllOfObject$detector, auto_unbox = TRUE, digits = NA))
      self
    }
  )
)
