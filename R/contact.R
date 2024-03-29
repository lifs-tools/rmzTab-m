# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@cebitec.uni-bielefeld.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title Contact
#' @description Contact Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#'
#' @field name  character [optional]
#'
#' @field affiliation  character [optional]
#'
#' @field email  character [optional]
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Contact <- R6::R6Class(
  'Contact',
  public = list(
    `id` = NULL,
    `name` = NULL,
    `affiliation` = NULL,
    `email` = NULL,
    #' @description Create a Contact
    #' @param id Contact id.
    #' @param name Contact name.
    #' @param affiliation Contact affiliation / address.
    #' @param email Contact email.
    #' @param ... local optional variable arguments
    #' 
    initialize = function(`id`=NULL, `name`=NULL, `affiliation`=NULL, `email`=NULL, ...){
      local.optional.var <- list(...)
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!is.null(`affiliation`)) {
        stopifnot(is.character(`affiliation`), length(`affiliation`) == 1)
        self$`affiliation` <- `affiliation`
      }
      if (!is.null(`email`)) {
        stopifnot(is.character(`email`), length(`email`) == 1)
        self$`email` <- `email`
      }
    },
    #' @description Serialize to list object suitable for jsonlite
    toJSON = function() {
      ContactObject <- list()
      if (!is.null(self$`id`)) {
        ContactObject[['id']] <-
          rmzTabM::safe_unbox(self$`id`)
      }
      if (!is.null(self$`name`)) {
        ContactObject[['name']] <-
          rmzTabM::safe_unbox(self$`name`)
      }
      if (!is.null(self$`affiliation`)) {
        ContactObject[['affiliation']] <-
          rmzTabM::safe_unbox(self$`affiliation`)
      }
      if (!is.null(self$`email`)) {
        ContactObject[['email']] <-
          rmzTabM::safe_unbox(self$`email`)
      }

      ContactObject
    },
    #' @description Deserialize from jsonlite list object
    #' @param ContactJson list object.    
    fromJSON = function(ContactJson) {
      ContactObject <- jsonlite::fromJSON(ContactJson)
      if (!is.null(ContactObject$`id`)) {
        self$`id` <- ContactObject$`id`
      }
      if (!is.null(ContactObject$`name`)) {
        self$`name` <- ContactObject$`name`
      }
      if (!is.null(ContactObject$`affiliation`)) {
        self$`affiliation` <- ContactObject$`affiliation`
      }
      if (!is.null(ContactObject$`email`)) {
        self$`email` <- ContactObject$`email`
      }
    },
    #' @description Serialize to JSON string.    
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
        sprintf(
        '"id":
          %d
                ',
        rmzTabM::safe_unbox(self$`id`)
        )},
        if (!is.null(self$`name`)) {
        sprintf(
        '"name":
          "%s"
                ',
        rmzTabM::safe_unbox(self$`name`)
        )},
        if (!is.null(self$`affiliation`)) {
        sprintf(
        '"affiliation":
          "%s"
                ',
        rmzTabM::safe_unbox(self$`affiliation`)
        )},
        if (!is.null(self$`email`)) {
        sprintf(
        '"email":
          "%s"
                ',
        rmzTabM::safe_unbox(self$`email`)
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    #' @description Deserialize from JSON string
    #' @param ContactJson JSON string    
    fromJSONString = function(ContactJson) {
      ContactObject <- jsonlite::fromJSON(ContactJson)
      self$`id` <- ContactObject$`id`
      self$`name` <- ContactObject$`name`
      self$`affiliation` <- ContactObject$`affiliation`
      self$`email` <- ContactObject$`email`
      self
    },
    #' @description Serialize to data frame
    toDataFrame = function() {
      idPrefix <- paste0("contact[", self$`id`, "]")
      elements <- data.frame(PREFIX=character(), KEY=character(), VALUE=character())
      if (!is.null(self$`name`)) {
        elements <-
          rbind(elements,
                list(
                  PREFIX = "MTD",
                  KEY = paste(idPrefix, "name", sep = "-"),
                  VALUE = self$`name`
                ),
                stringsAsFactors = FALSE)
      }
      if (!is.null(self$`affiliation`)) {
        elements <-
          rbind(elements,
                list(
                  PREFIX = "MTD",
                  KEY = paste(idPrefix, "affiliation", sep = "-"),
                  VALUE = self$`affiliation`
                ),
                stringsAsFactors = FALSE)
      }
      if (!is.null(self$`email`)) {
        elements <-
          rbind(elements,
                list(
                  PREFIX = "MTD",
                  KEY = paste(idPrefix, "email", sep = "-"),
                  VALUE = self$`email`
                ),
                stringsAsFactors = FALSE)
      }
      elements
    },
    #' @description Deserialize from contact data frame
    #' @param ContactDataFrame contact data frame
    fromDataFrame = function(ContactDataFrame) {
      stopifnot(nrow(ContactDataFrame)==1)
      if (rlang::has_name(ContactDataFrame, "id")) {
        self$`id` <- as.numeric(ContactDataFrame$`id`)
      }
      if (rlang::has_name(ContactDataFrame, "name")) {
        self$`name` <- ContactDataFrame$`name`
      }
      if (rlang::has_name(ContactDataFrame, "affiliation")) {
        self$`affiliation` <- ContactDataFrame$`affiliation`
      }
      if (rlang::has_name(ContactDataFrame, "email")) {
        self$`email` <- ContactDataFrame$`email`
      }
      self
    }
  )
)
