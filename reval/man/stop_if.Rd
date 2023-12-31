% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reval.R
\name{stop_if}
\alias{stop_if}
\title{Conditionally Stop Execution}
\usage{
stop_if(cond, msg, ...)
}
\arguments{
\item{cond}{A logical value. If TRUE, the execution stops.}

\item{msg}{A character string. The error message to be returned if the condition is true.}

\item{...}{Further arguments to be passed to the base::sprintf function for the error message.}
}
\value{
No return value. The function either stops the execution with an error or does nothing.
}
\description{
Stop the execution of the program if the condition is true.
This function is useful for debugging, as it can provide additional context around errors.
}
\examples{
# Execution continues
stop_if(FALSE, "This is an error message")
# Execution stops with an error
stop_if(TRUE, "This is an error message")
}
\author{
Vincent Toups
}
