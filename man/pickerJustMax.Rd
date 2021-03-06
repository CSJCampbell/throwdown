% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pickers.R
\name{pickerJustMax}
\alias{pickerJustMax}
\title{Pick Match to Best Revealed by Smallest Margin}
\usage{
pickerJustMax(active, revealed, exclude = NA, type = c("pair", "reveal"))
}
\arguments{
\item{active}{single numeric}

\item{revealed}{length 1 or 2 numeric}
}
\value{
length 1 numeric value 1
}
\description{
Select index of active or revealed 
to give biggest victory by active.
If type is "reveal", pick index of active to reveal 
(always just max unless win is not possible).
If type is "pair", pick index of revealed to pair 
(always just max unless win is not possible).
}
\examples{
pickerJustMax(active = 2, revealed = c(3, 1))
pickerJustMax(active = 3, revealed = 1:2)
pickerJustMax(active = 3, revealed = 4:5)
pickerJustMax(active = 1, revealed = 1)
pickerJustMax(active = 1, revealed = 5)
pickerJustMax(active = 1:5, revealed = 1:5, type = "reveal")
pickerJustMax(active = 1:5, revealed = rep(3, times = 5), type = "reveal")
}

