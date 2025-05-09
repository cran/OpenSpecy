% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_spec.R
\name{split_spec}
\alias{split_spec}
\title{Split Open Specy objects}
\usage{
split_spec(x)
}
\arguments{
\item{x}{a list of OpenSpecy objects}
}
\value{
A list of Open Specy objects each with 1 spectrum.
}
\description{
Convert a list of Open Specy objects with any number of spectra into
a list of Open Specy objects with one spectrum each.
}
\details{
Function will accept a list of Open Specy objects of any length and will split
them to their individual components. For example a list of two objects,
an Open Specy with only one spectrum and an Open Specy with 50 spectra
will return a list of length 51 each with Open Specy objects that only have
one spectrum.
}
\examples{
data("test_lib")
data("raman_hdpe")
listed <- list(test_lib, raman_hdpe)
test <- split_spec(listed)
test2 <- split_spec(list(test_lib))

}
\seealso{
\code{\link{c_spec}()} for combining \code{OpenSpecy} objects.
\code{\link{collapse_spec}()} for summarizing \code{OpenSpecy} objects.
}
\author{
Zacharias Steinmetz, Win Cowger
}
