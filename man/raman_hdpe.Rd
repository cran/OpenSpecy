% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raman_hdpe.R
\docType{data}
\name{raman_hdpe}
\alias{raman_hdpe}
\title{Sample Raman spectrum}
\format{
An threepart list of class \code{\link{OpenSpecy}} containing:
\tabular{ll}{
\code{wavenumber}: \tab spectral wavenumbers [1/cm] (vector of 964 rows) \cr
\code{spectra}: \tab absorbance values \link{-}
(a \code{\link[data.table]{data.table}} with 964 rows and 1 column) \cr
\code{metadata}: \tab spectral metadata \cr
}
}
\description{
Raman spectrum of high-density polyethylene (HDPE) provided by
Horiba Scientific.
}
\examples{
data(raman_hdpe)
print(raman_hdpe)

}
\references{
Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD, Hemabessiere L,
Lee E, Mill L, et al. (2020). “Critical Review of Processing and
Classification Techniques for Images and Spectra in Microplastic Research.”
\emph{Applied Spectroscopy}, \strong{74}(9), 989–1010.
\doi{10.1177/0003702820929064}.
}
\author{
Zacharias Steinmetz, Win Cowger
}
\keyword{data}
