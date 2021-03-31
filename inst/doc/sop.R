## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(OpenSpecy)

## ----eval=FALSE---------------------------------------------------------------
#  run_app()

## ---- fig.align="center", out.width="98%", echo=FALSE-------------------------
knitr::include_graphics("images/mainpage.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/googletranslate.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/samplefile.jpg")

## ----eval=FALSE---------------------------------------------------------------
#  data("raman_hdpe")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/uploadfile.jpg")

## ----sample_data, echo=FALSE, out.width="50%"---------------------------------
knitr::kable(head(raman_hdpe), caption = "Sample data `raman_hdpe`")

## ---- eval=FALSE--------------------------------------------------------------
#  read_text(".csv")
#  read_asp(".asp")
#  read_0(".0")

## ---- fig.align='center', echo=FALSE------------------------------------------
knitr::include_graphics("images/intensityadjustment.jpg")

## -----------------------------------------------------------------------------
library(dplyr)

raman_adj <- raman_hdpe %>%
  adj_intens()

head(raman_adj)

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/metadatainput.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/sharemetadata.jpg")

## ----eval=FALSE---------------------------------------------------------------
#  share_spec(raman_hdpe,
#             metadata = c(user_name = "Win Cowger",
#                          contact_info = "wincowger@gmail.com",
#                          spectrum_type = "Raman",
#                          spectrum_identity = "HDPE")
#             )

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/preprocessplot.png")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/smoothing.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/deselection.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/smoothingpoly.jpg")

## ----smoothing, fig.cap = "Sample `raman_hdpe` spectrum with different smoothing polynomials (p) from Cowger et al. (2020).", fig.width=6, echo=FALSE----
library(ggplot2)
data("raman_hdpe")

compare_smoothing <- rbind(
  cbind(smoothing = "none", adj_intens(raman_hdpe)),
  cbind(smoothing = "p = 1", smooth_intens(raman_hdpe, p = 1)),
  cbind(smoothing = "p = 4", smooth_intens(raman_hdpe, p = 4))
)

ggplot(compare_smoothing, aes(wavenumber, intensity)) +
  geom_line(aes(color = smoothing)) +
  theme_minimal()

## ---- eval=FALSE--------------------------------------------------------------
#  smooth_intens(raman_hdpe, p = 1)
#  smooth_intens(raman_hdpe, p = 4)

## -----------------------------------------------------------------------------
raman_smooth <- raman_adj %>% 
  smooth_intens()

head(raman_smooth)

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/baselinecorrectionpoly.jpg")

## ----subtraction, fig.cap = "Sample `raman_hdpe` spectrum with different degrees of background subtraction (Cowger et al., 2020).", fig.width=6, echo=FALSE----
library(ggplot2)
data("raman_hdpe")

compare_subtraction <- rbind(
  cbind(fitting = "none", adj_intens(raman_hdpe)),
  cbind(fitting = "degree = 2", subtr_bg(raman_hdpe, degree = 2)),
  cbind(fitting = "degree = 8", subtr_bg(raman_hdpe, degree = 8))
)

ggplot(compare_subtraction, aes(wavenumber, intensity)) +
  geom_line(aes(color = fitting)) +
  theme_minimal()

## -----------------------------------------------------------------------------
raman_bgc <- raman_smooth %>% 
  subtr_bg()

head(raman_bgc)

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/rangeselection.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/download.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/spectrumtype.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/spectrumtoanalyze.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/regiontomatch.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/matches.jpg")

## ----eval=FALSE---------------------------------------------------------------
#  # Fetch current spectral library from https://osf.io/x7dpz/
#  get_lib()
#  # Load library into global environment
#  spec_lib <- load_lib()
#  # Match spectrum with library and retrieve meta data
#  match_spec(raman_bgc, library = spec_lib, which = "raman")

## ---- fig.align="center", out.width="98%", echo=FALSE-------------------------
knitr::include_graphics("images/selectionmetadata.jpg")

## ---- eval=FALSE--------------------------------------------------------------
#  find_spec(sample_name == 5381, library = spec_lib, which = "raman")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/matchplot.png")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/horiba-1.png")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/horiba-2.png")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/horiba-3.png")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/horiba-4.png")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/horiba-5.png")

## ---- fig.align="center", out.width="98%", echo=FALSE-------------------------
knitr::include_graphics("images/spectragryph-1.png")

## ---- fig.align="center", out.width="98%", echo=FALSE-------------------------
knitr::include_graphics("images/spectragryph-2.png")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/uploadfile.jpg")

## ---- fig.align="center", echo=FALSE------------------------------------------
knitr::include_graphics("images/flowchart.png")

