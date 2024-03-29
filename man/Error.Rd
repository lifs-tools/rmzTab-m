% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error.R
\docType{class}
\name{Error}
\alias{Error}
\title{Error}
\format{
An \code{R6Class} generator object
}
\description{
Error Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{code}}{integer}

\item{\code{message}}{character}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Error$new()}}
\item \href{#method-toJSON}{\code{Error$toJSON()}}
\item \href{#method-fromJSON}{\code{Error$fromJSON()}}
\item \href{#method-toJSONString}{\code{Error$toJSONString()}}
\item \href{#method-fromJSONString}{\code{Error$fromJSONString()}}
\item \href{#method-clone}{\code{Error$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create an Error
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Error$new(code, message, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{code}}{Error code.}

\item{\code{message}}{Error message.}

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
\if{html}{\out{<div class="r">}}\preformatted{Error$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
Deserialize from jsonlite list object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Error$fromJSON(ErrorJson)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ErrorJson}}{list object.}
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
\if{html}{\out{<div class="r">}}\preformatted{Error$toJSONString()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSONString"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSONString}{}}}
\subsection{Method \code{fromJSONString()}}{
Deserialize from JSON string
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Error$fromJSONString(ErrorJson)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ErrorJson}}{JSON string}
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
\if{html}{\out{<div class="r">}}\preformatted{Error$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
