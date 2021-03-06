% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/small_molecule_evidence.R
\docType{class}
\name{SmallMoleculeEvidence}
\alias{SmallMoleculeEvidence}
\title{SmallMoleculeEvidence}
\format{An \code{R6Class} generator object}
\description{
SmallMoleculeEvidence Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{prefix}}{character [optional]}

\item{\code{header_prefix}}{character [optional]}

\item{\code{sme_id}}{integer}

\item{\code{evidence_input_id}}{character}

\item{\code{database_identifier}}{character}

\item{\code{chemical_formula}}{character [optional]}

\item{\code{smiles}}{character [optional]}

\item{\code{inchi}}{character [optional]}

\item{\code{chemical_name}}{character [optional]}

\item{\code{uri}}{character [optional]}

\item{\code{derivatized_form}}{\link{Parameter} [optional]}

\item{\code{adduct_ion}}{character [optional]}

\item{\code{exp_mass_to_charge}}{numeric}

\item{\code{charge}}{integer}

\item{\code{theoretical_mass_to_charge}}{numeric}

\item{\code{spectra_ref}}{list( \link{SpectraRef} )}

\item{\code{identification_method}}{\link{Parameter}}

\item{\code{ms_level}}{\link{Parameter}}

\item{\code{id_confidence_measure}}{list( numeric ) [optional]}

\item{\code{rank}}{integer}

\item{\code{opt}}{list( \link{OptColumnMapping} ) [optional]}

\item{\code{comment}}{list( \link{Comment} ) [optional]}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{SmallMoleculeEvidence$new()}}
\item \href{#method-toJSON}{\code{SmallMoleculeEvidence$toJSON()}}
\item \href{#method-fromJSON}{\code{SmallMoleculeEvidence$fromJSON()}}
\item \href{#method-toJSONString}{\code{SmallMoleculeEvidence$toJSONString()}}
\item \href{#method-fromJSONString}{\code{SmallMoleculeEvidence$fromJSONString()}}
\item \href{#method-clone}{\code{SmallMoleculeEvidence$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SmallMoleculeEvidence$new(
  sme_id,
  evidence_input_id,
  database_identifier,
  exp_mass_to_charge,
  charge,
  theoretical_mass_to_charge,
  spectra_ref,
  identification_method,
  ms_level,
  rank,
  prefix = "SME",
  header_prefix = "SEH",
  chemical_formula = NULL,
  smiles = NULL,
  inchi = NULL,
  chemical_name = NULL,
  uri = NULL,
  derivatized_form = NULL,
  adduct_ion = NULL,
  id_confidence_measure = NULL,
  opt = NULL,
  comment = NULL,
  ...
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toJSON"></a>}}
\subsection{Method \code{toJSON()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SmallMoleculeEvidence$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSON"></a>}}
\subsection{Method \code{fromJSON()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SmallMoleculeEvidence$fromJSON(SmallMoleculeEvidenceJson)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toJSONString"></a>}}
\subsection{Method \code{toJSONString()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SmallMoleculeEvidence$toJSONString()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSONString"></a>}}
\subsection{Method \code{fromJSONString()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SmallMoleculeEvidence$fromJSONString(SmallMoleculeEvidenceJson)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SmallMoleculeEvidence$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
