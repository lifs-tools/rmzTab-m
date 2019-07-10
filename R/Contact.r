# mzTab-M reference implementation and validation API.
# 
# This is the mzTab-M reference implementation and validation API service.
# 
# OpenAPI spec version: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://github.com/swagger-api/swagger-codegen.git


#' Contact Class
#'
#' The contact’s name, affiliation and e-mail. Several contacts can be given by indicating the number in the square brackets after \&quot;contact\&quot;. A contact has to be supplied in the format [first name] [initials] [last name]. 
#'
#' @field id 
#' @field elementType 
#' @field name 
#' @field affiliation 
#' @field email 
#'
#' mzTab-M specification example(s)
#' \preformatted{
#' MTD	contact[1]-name	James D. Watson
#' MTD	contact[1]-affiliation	Cambridge University, UK
#' MTD	contact[1]-email	watson@cam.ac.uk
#' MTD	contact[2]-name	Francis Crick
#' MTD	contact[2]-affiliation	Cambridge University, UK
#' MTD	contact[2]-email	crick@cam.ac.uk
#'
#' }
#' 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Contact <- R6::R6Class(
  'Contact',
  public = list(
    `id` = NULL,
    `elementType` = NULL,
    `name` = NULL,
    `affiliation` = NULL,
    `email` = NULL,
    initialize = function(`id`, `elementType`, `name`, `affiliation`, `email`){
      if (!missing(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!missing(`elementType`)) {
        stopifnot(is.character(`elementType`), length(`elementType`) == 1)
        self$`elementType` <- `elementType`
      }
      if (!missing(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!missing(`affiliation`)) {
        stopifnot(is.character(`affiliation`), length(`affiliation`) == 1)
        self$`affiliation` <- `affiliation`
      }
      if (!missing(`email`)) {
        stopifnot(is.character(`email`), length(`email`) == 1)
        self$`email` <- `email`
      }
    },
    toJSON = function() {
      ContactObject <- list()
      if (!is.null(self$`id`)) {
        ContactObject[['id']] <- self$`id`
      }
      if (!is.null(self$`elementType`)) {
        ContactObject[['elementType']] <- self$`elementType`
      }
      if (!is.null(self$`name`)) {
        ContactObject[['name']] <- self$`name`
      }
      if (!is.null(self$`affiliation`)) {
        ContactObject[['affiliation']] <- self$`affiliation`
      }
      if (!is.null(self$`email`)) {
        ContactObject[['email']] <- self$`email`
      }

      ContactObject
    },
    fromJSON = function(ContactJson) {
      ContactObject <- jsonlite::fromJSON(ContactJson, simplifyVector = FALSE)
      if (!is.null(ContactObject$`id`)) {
        self$`id` <- ContactObject$`id`
      }
      if (!is.null(ContactObject$`elementType`)) {
        self$`elementType` <- ContactObject$`elementType`
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
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "elementType": %s,
           "name": %s,
           "affiliation": %s,
           "email": %s
        }',
        self$`id`,
        self$`elementType`,
        self$`name`,
        self$`affiliation`,
        self$`email`
      )
    },
    fromJSONString = function(ContactJson) {
      ContactObject <- jsonlite::fromJSON(ContactJson, simplifyVector = FALSE)
      self$`id` <- ContactObject$`id`
      self$`elementType` <- ContactObject$`elementType`
      self$`name` <- ContactObject$`name`
      self$`affiliation` <- ContactObject$`affiliation`
      self$`email` <- ContactObject$`email`
    }
  )
)