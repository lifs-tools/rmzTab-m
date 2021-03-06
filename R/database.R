# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title Database
#' @description Database Class
#' @format An \code{R6Class} generator object
#' @field id  integer 
#'
#' @field elementType  character 
#'
#' @field param  \link{Parameter} 
#'
#' @field prefix  character 
#'
#' @field version  character 
#'
#' @field uri  character 
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Database <- R6::R6Class(
  'Database',
  public = list(
    `id` = NULL,
    `elementType` = NULL,
    `param` = NULL,
    `prefix` = NULL,
    `version` = NULL,
    `uri` = NULL,
    initialize = function(`id`, `elementType`, `param`, `prefix`, `version`, `uri`, ...){
      local.optional.var <- list(...)
      if (!missing(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!missing(`elementType`)) {
        stopifnot(is.character(`elementType`), length(`elementType`) == 1)
        self$`elementType` <- `elementType`
      }
      if (!missing(`param`)) {
        stopifnot(R6::is.R6(`param`))
        self$`param` <- `param`
      }
      if (!missing(`prefix`)) {
        stopifnot(is.character(`prefix`), length(`prefix`) == 1)
        self$`prefix` <- `prefix`
      }
      if (!missing(`version`)) {
        stopifnot(is.character(`version`), length(`version`) == 1)
        self$`version` <- `version`
      }
      if (!missing(`uri`)) {
        stopifnot(is.character(`uri`), length(`uri`) == 1)
        self$`uri` <- `uri`
      }
    },
    toJSON = function() {
      DatabaseObject <- list()
      if (!is.null(self$`id`)) {
        DatabaseObject[['id']] <-
          self$`id`
      }
      if (!is.null(self$`elementType`)) {
        DatabaseObject[['elementType']] <-
          self$`elementType`
      }
      if (!is.null(self$`param`)) {
        DatabaseObject[['param']] <-
          self$`param`$toJSON()
      }
      if (!is.null(self$`prefix`)) {
        DatabaseObject[['prefix']] <-
          self$`prefix`
      }
      if (!is.null(self$`version`)) {
        DatabaseObject[['version']] <-
          self$`version`
      }
      if (!is.null(self$`uri`)) {
        DatabaseObject[['uri']] <-
          self$`uri`
      }

      DatabaseObject
    },
    fromJSON = function(DatabaseJson) {
      DatabaseObject <- jsonlite::fromJSON(DatabaseJson)
      if (!is.null(DatabaseObject$`id`)) {
        self$`id` <- DatabaseObject$`id`
      }
      if (!is.null(DatabaseObject$`elementType`)) {
        self$`elementType` <- DatabaseObject$`elementType`
      }
      if (!is.null(DatabaseObject$`param`)) {
        paramObject <- Parameter$new()
        paramObject$fromJSON(jsonlite::toJSON(DatabaseObject$param, auto_unbox = TRUE, digits = NA))
        self$`param` <- paramObject
      }
      if (!is.null(DatabaseObject$`prefix`)) {
        self$`prefix` <- DatabaseObject$`prefix`
      }
      if (!is.null(DatabaseObject$`version`)) {
        self$`version` <- DatabaseObject$`version`
      }
      if (!is.null(DatabaseObject$`uri`)) {
        self$`uri` <- DatabaseObject$`uri`
      }
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
        sprintf(
        '"id":
          %d
                ',
        self$`id`
        )},
        if (!is.null(self$`elementType`)) {
        sprintf(
        '"elementType":
          "%s"
                ',
        self$`elementType`
        )},
        if (!is.null(self$`param`)) {
        sprintf(
        '"param":
        %s
        ',
        jsonlite::toJSON(self$`param`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`prefix`)) {
        sprintf(
        '"prefix":
          "%s"
                ',
        self$`prefix`
        )},
        if (!is.null(self$`version`)) {
        sprintf(
        '"version":
          "%s"
                ',
        self$`version`
        )},
        if (!is.null(self$`uri`)) {
        sprintf(
        '"uri":
          "%s"
                ',
        self$`uri`
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(DatabaseJson) {
      DatabaseObject <- jsonlite::fromJSON(DatabaseJson)
      self$`id` <- DatabaseObject$`id`
      self$`elementType` <- DatabaseObject$`elementType`
      self$`param` <- Parameter$new()$fromJSON(jsonlite::toJSON(DatabaseObject$param, auto_unbox = TRUE, digits = NA))
      self$`prefix` <- DatabaseObject$`prefix`
      self$`version` <- DatabaseObject$`version`
      self$`uri` <- DatabaseObject$`uri`
      self
    }
  )
)
