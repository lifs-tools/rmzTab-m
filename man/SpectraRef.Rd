% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectra_ref.R
\docType{class}
\name{SpectraRef}
\alias{SpectraRef}
\title{SpectraRef}
\format{
An \code{R6Class} generator object
}
\description{
SpectraRef Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{ms_run}}{\link{MsRun}}

\item{\code{reference}}{character}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{SpectraRef$new()}}
\item \href{#method-toJSON}{\code{SpectraRef$toJSON()}}
\item \href{#method-fromJSON}{\code{SpectraRef$fromJSON()}}
\item \href{#method-toJSONString}{\code{SpectraRef$toJSONString()}}
\item \href{#method-fromJSONString}{\code{SpectraRef$fromJSONString()}}
\item \href{#method-toString}{\code{SpectraRef$toString()}}
\item \href{#method-fromString}{\code{SpectraRef$fromString()}}
\item \href{#method-clone}{\code{SpectraRef$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a SpectraRef
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$new(ms_run, reference, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ms_run}}{The ms run \link{MsRun} .}

\item{\code{reference}}{The reference to a particular spectrum using the id_format defined in \link{MsRun}.}

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
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
Deserialize from jsonlite list object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$fromJSON(SpectraRefJson)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SpectraRefJson}}{list object.}
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
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$toJSONString()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSONString"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSONString}{}}}
\subsection{Method \code{fromJSONString()}}{
Deserialize from JSON string
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$fromJSONString(SpectraRefJson)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SpectraRefJson}}{JSON string}
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
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$toString()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromString"></a>}}
\if{latex}{\out{\hypertarget{method-fromString}{}}}
\subsection{Method \code{fromString()}}{
Deerialize from string
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$fromString(SpectraRefString)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SpectraRefString}}{The SpectraRef string representation.}
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
\if{html}{\out{<div class="r">}}\preformatted{SpectraRef$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
