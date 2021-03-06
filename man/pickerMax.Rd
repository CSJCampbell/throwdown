% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pickers.R
\name{pickerMax}
\alias{pickerMax}
\title{Pick Match to Maximize Advantage over Revealed}
\usage{
pickerMax(active, revealed, exclude = NA, type = c("pair", "reveal"))
}
\arguments{
\item{active}{numeric vector choices of active player}

\item{revealed}{numeric vector choices revealed by non-active player for next round}

\item{exclude}{integer vector of indices of active 
to exclude when picking active for type reveal.}

\item{type}{single character, select modality:\itemize{
    \item "pair", pick index of revealed to create pairing.
    \item "reveal", pick index of active to reveal.
}}
}
\value{
single numeric
}
\description{
Select index of active or revealed 
to give biggest victory by active.
If type is "reveal", pick index of active to reveal 
(always max unless win is not possible).
If type is "pair", pick index of revealed to pair 
(always max unless win is not possible).
If multiple matchups are equally advantageous, pick first.
}
\examples{
pickerMax(active = 2, revealed = c(3, 1))
pickerMax(active = 1, revealed = 1:2)
pickerMax(active = 1, revealed = 1)
pickerMax(active = 2, revealed = c(3, 1), type = "reveal")
pickerMax(active = 1:5, revealed = c(3, 1), type = "reveal")
pickerMax(active = rep(5, 5), revealed = 1:5, type = "reveal")
pickerMax(active = rep(5, 5), revealed = 1:5, 
    exclude = 1, type = "reveal")
pickerMax(active = 4, revealed = 4, 
    exclude = 1)
}

