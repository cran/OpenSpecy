% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_opus_raw.R
\name{read_opus_raw}
\alias{read_opus_raw}
\title{Read a Bruker OPUS spectrum binary raw string}
\usage{
read_opus_raw(rw, type = "spec", atm_comp_minus4offset = FALSE)
}
\arguments{
\item{rw}{a raw vector}

\item{type}{character vector of spectra types to extract from OPUS binary
file. Default is \code{"spec"}, which will extract the final spectra, e.g.
expressed in absorbance (named \code{AB} in Bruker OPUS programs). Possible
additional values for the character vector supplied to \code{type} are
\code{"spec_no_atm_comp"} (spectrum of the sample without compensation for
atmospheric gases, water vapor and/or carbon dioxide),
\code{"sc_sample"} (single channel spectrum of the sample measurement),
\code{"sc_ref"} (single channel spectrum of the reference measurement),
\code{"ig_sample"} (interferogram of the sample measurement) and \code{"ig_ref"}
(interferogram of the reference measurement).}

\item{atm_comp_minus4offset}{logical; whether spectra after atmospheric
compensation are read with an offset of -4 bytes from Bruker OPUS
files. Default is \code{FALSE}.}
}
\value{
A list of 10 elements:

\describe{
\item{\code{metadata}}{a \code{data.frame} containing metadata from the OPUS file.}
\item{\code{spec}}{if \code{"spec"} was requested in the \code{type} option, a matrix of
the spectrum of the sample (otherwise set to \code{NULL}).}
\item{\code{spec_no_atm_comp}}{if \code{"spec_no_atm_comp"} was requested in the
\code{type} option, a matrix of the spectrum of the sample without atmospheric
compensation (otherwise set to \code{NULL}).}
\item{\code{sc_sample}}{if \code{"sc_sample"} was requested in the \code{type} option, a
matrix of the single channel spectrum of the sample (otherwise set to
\code{NULL}).}
\item{\code{sc_ref}}{if \code{"sc_ref"} was requested in the \code{type} option, a matrix
of the single channel spectrum of the reference (otherwise set to \code{NULL}).}
\item{\code{ig_sample}}{if \code{"ig_sample"} was requested in the \code{type} option, a
matrix of the interferogram of the sample (otherwise set to \code{NULL}).}
\item{\code{ig_ref}}{if \code{"ig_ref"} was requested in the \code{type} option, a matrix
of the interferogram of the reference (otherwise set to \code{NULL}).}
\item{\code{wavenumbers}}{if \code{"spec"} or \code{"spec_no_atm_comp"} was requested in
the \code{type} option, a numeric vector of the wavenumbers of the spectrum of
the sample (otherwise set to \code{NULL}).}
\item{\code{wavenumbers_sc_sample}}{if \code{"sc_sample"} was requested in the \code{type}
option, a numeric vector of the wavenumbers of the single channel spectrum
of the sample (otherwise set to \code{NULL}).}
\item{\code{wavenumbers_sc_ref}}{if \code{"sc_ref"} was requested in the \code{type}
option, a numeric vector of the wavenumbers of the single channel spectrum
of the reference (otherwise set to \code{NULL}).}
}
}
\description{
Read single binary acquired with an Bruker Vertex FTIR Instrument
}
\details{
The type of spectra returned by the function when using
\code{type = "spec"} depends on the setting of the Bruker instrument: typically,
it can be either absorbance or reflectance.

The type of spectra to extract from the file can also use Bruker's OPUS
software naming conventions, as follows:
\itemize{
\item \code{ScSm} corresponds to \code{sc_sample}
\item \code{ScRf} corresponds to \code{sc_ref}
\item \code{IgSm} corresponds to \code{ig_sample}
\item \code{IgRf} corresponds to \code{ig_ref}
}
}
\seealso{
\code{\link{read_opus}()}
}
\author{
Philipp Baumann and Pierre Roudier
}
