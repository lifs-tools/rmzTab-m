# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title ValidatePlain operations
#' @description openapi.ValidatePlain
#' @format An \code{R6Class} generator object
#' @field apiClient Handles the client-server communication.
#'
#' @section Methods:
#' \describe{
#' \strong{ ValidatePlainMzTabFile } \emph{  }
#' Validates an mzTab file in plain text representation / tab-separated format and reports syntactic, structural, and semantic errors. 
#'
#' \itemize{
#' \item \emph{ @param } mztabfile character
#' \item \emph{ @param } level Enum < [info, warn, error] > 
#' \item \emph{ @param } max.errors integer
#' \item \emph{ @param } semantic.validation character
#' \item \emph{ @returnType } list( \link{ValidationMessage} ) \cr
#'
#'
#' \item status code : 200 | Validation Okay
#'
#' \item return type : array[ValidationMessage] 
#' \item response headers :
#'
#' \tabular{ll}{
#' }
#' \item status code : 415 | Unsupported content type
#'
#'
#' \item response headers :
#'
#' \tabular{ll}{
#' }
#' \item status code : 422 | Invalid input
#'
#' \item return type : array[ValidationMessage] 
#' \item response headers :
#'
#' \tabular{ll}{
#' }
#' \item status code : 0 | Unexpected error
#'
#' \item return type : Error 
#' \item response headers :
#'
#' \tabular{ll}{
#' }
#' }
#'
#' }
#'
#'
#' @examples
#' \dontrun{
#' ####################  ValidatePlainMzTabFile  ####################
#'
#' library(openapi)
#' var.mztabfile <- 'mztabfile_example' # character | mzTab file that should be validated.
#' var.level <- 'info' # character | The level of errors that should be reported, one of ERROR, WARN, INFO.
#' var.max.errors <- 100 # integer | The maximum number of errors to return.
#' var.semantic.validation <- FALSE # character | Whether a semantic validation against the default rule set should be performed.
#'
#' api.instance <- ValidatePlainApi$new()
#'
#' result <- api.instance$ValidatePlainMzTabFile(var.mztabfile, level=var.level, max.errors=var.max.errors, semantic.validation=var.semantic.validation)
#'
#'
#' }
#' @importFrom R6 R6Class
#' @importFrom caTools base64encode
#' @export
ValidatePlainApi <- R6::R6Class(
  'ValidatePlainApi',
  public = list(
    apiClient = NULL,
    initialize = function(apiClient){
      if (!missing(apiClient)) {
        self$apiClient <- apiClient
      }
      else {
        self$apiClient <- ApiClient$new()
      }
    },
    ValidatePlainMzTabFile = function(mztabfile, level='info', max.errors=100, semantic.validation=FALSE, ...){
      apiResponse <- self$ValidatePlainMzTabFileWithHttpInfo(mztabfile, level, max.errors, semantic.validation, ...)
      resp <- apiResponse$response
      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        apiResponse$content
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        apiResponse
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        apiResponse
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        apiResponse
      }
    },

    ValidatePlainMzTabFileWithHttpInfo = function(mztabfile, level='info', max.errors=100, semantic.validation=FALSE, ...){
      args <- list(...)
      queryParams <- list()
      headerParams <- c()

      if (missing(`mztabfile`)) {
        stop("Missing required parameter `mztabfile`.")
      }

      queryParams['level'] <- level

      queryParams['maxErrors'] <- max.errors

      queryParams['semanticValidation'] <- semantic.validation

      if (!missing(`mztabfile`)) {
        body <- `mztabfile`$toJSONString()
      } else {
        body <- NULL
      }

      urlPath <- "/validatePlain"

      resp <- self$apiClient$CallApi(url = paste0(self$apiClient$basePath, urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body,
                                 ...)

      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        deserializedRespObj <- tryCatch(
          self$apiClient$deserialize(resp, "array[ValidationMessage]", loadNamespace("rmzTabM")),
          error = function(e){
             stop("Failed to deserialize response")
          }
        )
        ApiResponse$new(deserializedRespObj, resp)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        ApiResponse$new(paste("Server returned " , httr::status_code(resp) , " response status code."), resp)
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        ApiResponse$new("API client error", resp)
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        ApiResponse$new("API server error", resp)
      }
    }
  )
)
