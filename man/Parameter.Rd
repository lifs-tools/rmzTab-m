% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter.R
\docType{class}
\name{Parameter}
\alias{Parameter}
\title{Parameter}
\format{
An \code{R6Class} generator object
}
\description{
Parameter Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{integer [optional]}

\item{\code{cv_label}}{character [optional]}

\item{\code{cv_accession}}{character [optional]}

\item{\code{name}}{character}

\item{\code{value}}{character}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Parameter$new()}}
\item \href{#method-toJSON}{\code{Parameter$toJSON()}}
\item \href{#method-fromJSON}{\code{Parameter$fromJSON()}}
\item \href{#method-toJSONString}{\code{Parameter$toJSONString()}}
\item \href{#method-fromJSONString}{\code{Parameter$fromJSONString()}}
\item \href{#method-toString}{\code{Parameter$toString()}}
\item \href{#method-fromString}{\code{Parameter$fromString()}}
\item \href{#method-clone}{\code{Parameter$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a Parameter
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$new(name, value, id = NULL, cv_label = "", cv_accession = "", ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{Parameter name.}

\item{\code{value}}{Parameter value.}

\item{\code{id}}{Parameter id.}

\item{\code{cv_label}}{The label of the controlled vocabulary.}

\item{\code{cv_accession}}{The accession number of this Parameter.}

\item{\code{...}}{local optional variable arguments}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
Serialize to list object suitable for jsonlite
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
Deserialize from jsonlite list object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$fromJSON(ParameterJson)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ParameterJson}}{list object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toJSONString"></a>}}
\if{latex}{\out{\hypertarget{method-toJSONString}{}}}
\subsection{Method \code{toJSONString()}}{
Serialize to JSON string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$toJSONString()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSONString"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSONString}{}}}
\subsection{Method \code{fromJSONString()}}{
Deserialize from JSON string
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$fromJSONString(ParameterJson)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ParameterJson}}{JSON string}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toString"></a>}}
\if{latex}{\out{\hypertarget{method-toString}{}}}
\subsection{Method \code{toString()}}{
Serialize to string
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$toString()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromString"></a>}}
\if{latex}{\out{\hypertarget{method-fromString}{}}}
\subsection{Method \code{fromString()}}{
Deserialize from string
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$fromString(id, ParameterString)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{Optional id of the Parameter.}

\item{\code{ParameterString}}{String representation of the Parameter.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
