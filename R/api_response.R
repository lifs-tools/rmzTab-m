#' @docType class
#' @title ApiResponse
#' @description ApiResponse Class
#' @format An \code{R6Class} generator object
#' @field content The deserialized response body.
#' @field response The raw response from the endpoint.
#' @export
ApiResponse  <- R6::R6Class(
  'ApiResponse',
  public = list(
    content = NULL,
    response = NULL,
    #'@description Create an ApiResponse
    #'@param content The deserialized response body.
    #'@param response The raw response from the endpoint.
    initialize = function(content, response){
      self$content <- content
      self$response <- response
    }
  )
)
