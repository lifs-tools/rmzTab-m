% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_plain_api.R
\docType{class}
\name{ValidatePlainApi}
\alias{ValidatePlainApi}
\title{ValidatePlain operations}
\format{An \code{R6Class} generator object}
\description{
openapi.ValidatePlain
}
\section{Methods}{

\describe{
\strong{ ValidatePlainMzTabFile } \emph{  }
Validates an mzTab file in plain text representation / tab-separated format and reports syntactic, structural, and semantic errors. 

\itemize{
\item \emph{ @param } mztabfile character
\item \emph{ @param } level Enum < [info, warn, error] > 
\item \emph{ @param } max.errors integer
\item \emph{ @param } semantic.validation character
\item \emph{ @returnType } list( \link{ValidationMessage} ) \cr


\item status code : 200 | Validation Okay

\item return type : array[ValidationMessage] 
\item response headers :

\tabular{ll}{
}
\item status code : 415 | Unsupported content type


\item response headers :

\tabular{ll}{
}
\item status code : 422 | Invalid input

\item return type : array[ValidationMessage] 
\item response headers :

\tabular{ll}{
}
\item status code : 0 | Unexpected error

\item return type : Error 
\item response headers :

\tabular{ll}{
}
}

}
}

\examples{
\dontrun{
####################  ValidatePlainMzTabFile  ####################

library(openapi)
var.mztabfile <- 'mztabfile_example' # character | mzTab file that should be validated.
var.level <- 'info' # character | The level of errors that should be reported, one of ERROR, WARN, INFO.
var.max.errors <- 100 # integer | The maximum number of errors to return.
var.semantic.validation <- FALSE # character | Whether a semantic validation against the default rule set should be performed.

api.instance <- ValidatePlainApi$new()

result <- api.instance$ValidatePlainMzTabFile(var.mztabfile, level=var.level, max.errors=var.max.errors, semantic.validation=var.semantic.validation)


}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{apiClient}}{Handles the client-server communication.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ValidatePlainApi$new()}}
\item \href{#method-ValidatePlainMzTabFile}{\code{ValidatePlainApi$ValidatePlainMzTabFile()}}
\item \href{#method-ValidatePlainMzTabFileWithHttpInfo}{\code{ValidatePlainApi$ValidatePlainMzTabFileWithHttpInfo()}}
\item \href{#method-clone}{\code{ValidatePlainApi$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ValidatePlainApi$new(apiClient)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ValidatePlainMzTabFile"></a>}}
\subsection{Method \code{ValidatePlainMzTabFile()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ValidatePlainApi$ValidatePlainMzTabFile(
  mztabfile,
  level = "info",
  max.errors = 100,
  semantic.validation = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ValidatePlainMzTabFileWithHttpInfo"></a>}}
\subsection{Method \code{ValidatePlainMzTabFileWithHttpInfo()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ValidatePlainApi$ValidatePlainMzTabFileWithHttpInfo(
  mztabfile,
  level = "info",
  max.errors = 100,
  semantic.validation = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ValidatePlainApi$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
