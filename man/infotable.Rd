% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infotable.r
\name{infotable}
\alias{infotable}
\title{Extract information on income brackets from variable labels in the ISSP dataset}
\usage{
infotable(variable, print = F)
}
\arguments{
\item{variable}{The input variable from the iSSP dataset.}

\item{print}{Logical. If TRUE, the resulting table will be printed.}
}
\value{
A data frame containing information on income brackets extracted from
        the variable labels.
}
\description{
This function takes a variable from the iSSP dataset, extracts information on
income brackets from its labels, and returns a data frame containing details
about the income brackets. It serves as the base function for the wave-specific functions (e.g., infotable_2005)
}
\details{
The function uses the \code{alabs} function to generate a frequency
         table for the input variable, removes labels with certain NA patterns,
         and extracts numerical values from the remaining labels to form
         income brackets. The resulting data frame includes information on
         the lower and upper bounds of the brackets.
}
\examples{
# Example with a variable 'income_variable' from the iSSP dataset
data <- infotable(income_variable, print = TRUE)

}
