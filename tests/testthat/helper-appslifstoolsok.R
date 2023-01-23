## Check if Online and whether Validator Tests can be run 

apps_lifstools_statusOK <- FALSE

tryCatch({
  # response <- GET("http://httpbin.org/delay/6", timeout(5))
  # response <- GET("http://httpbin.org/delay/1", timeout(5))
  respone <- GET("https://apps.lifs-tools.org/mztabvalidator/v2/api-docs", timeout(5))
  apps_lifstools_statusOK <- httr::http_status(response)$category=="Success"
}, error = function(x) {message(conditionMessage(x)); apps_lifstools_statusOK <- FALSE} )
