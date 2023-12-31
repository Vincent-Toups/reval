% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reval.R
\name{check_float}
\alias{check_float}
\title{Check if values in a column can be parsed as floats}
\usage{
check_float(dataset, variable_name, format = NA, allow_na = FALSE)
}
\arguments{
\item{dataset}{A data frame containing the data to be checked.}

\item{variable_name}{The name of the variable/column to check.}

\item{format}{An optional specification of the format that the float values should conform to.}

\item{allow_na}{Logical flag indicating whether NA values are allowed. Defaults to FALSE.}
}
\value{
A tibble that records the name of the check, whether it passed, a message summarizing the check results, and the row numbers of any values that failed the check.
}
\description{
Check if values in a column can be parsed as floats
}
\examples{
\dontrun{
check_float(dataset = df, variable_name = "column1", format = NA, allow_na = FALSE)
}
}
