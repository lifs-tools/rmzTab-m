# mzTab-M reference implementation and validation API.
# 
# This is the mzTab-M reference implementation and validation API service.
# 
# OpenAPI spec version: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' @title ValidatePlain operations
#' @description rmztab.ValidatePlain
#'
#' @field path Stores url path of the request.
#' @field apiClient Handles the client-server communication.
#' @field userAgent Set the user agent of the request.
#'
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'
#' validate_plain_mz_tab_file  
#'
#' }
#' 
#' @export
ValidatePlainApi <- R6::R6Class(
  'ValidatePlainApi',
  public = list(
    userAgent = "Swagger-Codegen/1.0.0/r",
    apiClient = NULL,
    initialize = function(apiClient){
      if (!missing(apiClient)) {
        self$apiClient <- apiClient
      }
      else {
        self$apiClient <- ApiClient$new()
      }
    },
    validate_plain_mz_tab_file = function(mztabfile, level, max_errors, semantic_validation, ...){
      args <- list(...)
      queryParams <- list()
      headerParams <- character()

      if (!missing(`level`)) {
        queryParams['level'] <- level
      }

      if (!missing(`max_errors`)) {
        queryParams['maxErrors'] <- max_errors
      }

      if (!missing(`semantic_validation`)) {
        queryParams['semanticValidation'] <- semantic_validation
      }

      if (!missing(`mztabfile`)) {
        body <- `mztabfile`$toJSONString()
      } else {
        body <- NULL
      }

      urlPath <- "/validatePlain"
      resp <- self$apiClient$callApi(url = paste0(self$apiClient$basePath, urlPath),
                                 method = "POST",
                                 queryParams = queryParams,
                                 headerParams = headerParams,
                                 body = body, 
                                 ...)
      
      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        returnObject <- ValidationMessage$new()
        result <- returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        Response$new(returnObject, resp)
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        Response$new("API client error", resp)
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        Response$new("API server error", resp)
      }

    }
  )
) 