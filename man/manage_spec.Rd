% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage_spec.R
\name{c_spec}
\alias{c_spec}
\alias{c_spec.default}
\alias{c_spec.OpenSpecy}
\alias{c_spec.list}
\alias{sample_spec}
\alias{sample_spec.default}
\alias{sample_spec.OpenSpecy}
\alias{merge_map}
\alias{merge_map.default}
\alias{merge_map.OpenSpecy}
\alias{merge_map.list}
\title{Manage spectral objects}
\usage{
c_spec(x, ...)

\method{c_spec}{default}(x, ...)

\method{c_spec}{OpenSpecy}(x, ...)

\method{c_spec}{list}(x, range = NULL, res = 5, ...)

sample_spec(x, ...)

\method{sample_spec}{default}(x, ...)

\method{sample_spec}{OpenSpecy}(x, size = 1, prob = NULL, ...)

merge_map(x, ...)

\method{merge_map}{default}(x, ...)

\method{merge_map}{OpenSpecy}(x, ...)

\method{merge_map}{list}(x, origins = NULL, ...)
}
\arguments{
\item{x}{a list of \code{OpenSpecy} objects or of file paths.}

\item{range}{a numeric providing your own wavenumber ranges or character
argument called \code{"common"} to let \code{c_spec()} find the common
wavenumber range of the supplied spectra. \code{NULL} will interpret the
spectra having all the same wavenumber range.}

\item{res}{defaults to \code{NULL}, the resolution you want the output
wavenumbers to be.}

\item{size}{the number of spectra to sample.}

\item{prob}{probabilities to use for the sampling.}

\item{origins}{a list with 2 value vectors of x y coordinates for the offsets of each image.}

\item{\ldots}{further arguments passed to submethods.}
}
\value{
\code{c_spec()} and \code{sample_spec()} return \code{OpenSpecy} objects.
}
\description{
\code{c_spec()} concatenates \code{OpenSpecy} objects.
\code{sample_spec()} samples spectra from an \code{OpenSpecy} object.
\code{merge_map()} merge two \code{OpenSpecy} objects from spectral maps.
}
\examples{
# Concatenating spectra
spectra <- lapply(c(read_extdata("raman_hdpe.csv"),
                    read_extdata("ftir_ldpe_soil.asp")), read_any)
common <- c_spec(spectra, range = "common", res = 5)
range <- c_spec(spectra, range = c(1000, 2000), res = 5)

# Sampling spectra
tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
sampled <- sample_spec(tiny_map, size = 3)

}
\seealso{
\code{\link[OpenSpecy]{conform_spec}()} for conforming wavenumbers
}
\author{
Zacharias Steinmetz, Win Cowger
}
