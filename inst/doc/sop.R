## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

data.table::setDTthreads(2)

## ----setup--------------------------------------------------------------------
library(OpenSpecy)

## ----eval=FALSE---------------------------------------------------------------
#  run_app()

## ----eval=FALSE---------------------------------------------------------------
#  read_any("path/to/your/data")

## ----eval=FALSE---------------------------------------------------------------
#  read_text(".csv")
#  read_asp(".asp")
#  read_opus(".0")

## -----------------------------------------------------------------------------
data("raman_hdpe")

## -----------------------------------------------------------------------------
spectral_map <- read_extdata("CA_tiny_map.zip") |> 
  read_any() # preserves some metadata
asp_example <- read_extdata("ftir_ldpe_soil.asp") |> 
  read_any() 
ps_example <- read_extdata("ftir_ps.0") |> 
  read_any() # preserves some metadata
csv_example <- read_extdata("raman_hdpe.csv") |> 
  read_any()
json_example <- read_extdata("raman_hdpe.json") |> 
  read_any() # read in exactly as an OpenSpecy object

## -----------------------------------------------------------------------------
scratch_OpenSpecy <- as_OpenSpecy(x = seq(1000,2000, by = 5), 
                                  spectra =  data.frame(runif(n = 201)), 
                                  metadata = list(file_name = "fake_noise")) 

## -----------------------------------------------------------------------------
# Access the wavenumbers
scratch_OpenSpecy$wavenumber

## -----------------------------------------------------------------------------
# Access the spectra
scratch_OpenSpecy$spectra

## -----------------------------------------------------------------------------
# Access the metadata
scratch_OpenSpecy$metadata

## -----------------------------------------------------------------------------
# Performs checks to ensure that OpenSpecy objects are adhering to our standards;
# returns TRUE if it passes. 
check_OpenSpecy(scratch_OpenSpecy)

# Checks only the object type to make sure it has OpenSpecy type
is_OpenSpecy(scratch_OpenSpecy)

## -----------------------------------------------------------------------------
print(scratch_OpenSpecy) # shows the raw object

## -----------------------------------------------------------------------------
summary(scratch_OpenSpecy) # summarizes the contents of the spectra

## -----------------------------------------------------------------------------
head(scratch_OpenSpecy) # shows the top wavenumbers and intensities

## ----eval=F-------------------------------------------------------------------
#  write_spec(scratch_OpenSpecy, "test_scratch_OpenSpecy.yml", digits = 5)
#  write_spec(scratch_OpenSpecy, "test_scratch_OpenSpecy.json", digits = 5)

## ----eval=F-------------------------------------------------------------------
#  hyperspecy <- as_hyperSpec(scratch_OpenSpecy)

## ----fig.align="center", fig.width=5------------------------------------------
plot(scratch_OpenSpecy) # quick and efficient

## ----fig.align="center", out.width="100%"-------------------------------------
# This will min-max normalize your data even if it isn't already but are not
# changing your underlying data
plotly_spec(scratch_OpenSpecy, json_example)

## ----eval=F-------------------------------------------------------------------
#  heatmap_spec(spectral_map,
#               z = spectral_map$metadata$x)

## ----fig.align="center", out.width="100%"-------------------------------------
interactive_plot(spectral_map, select = 100, z = spectral_map$metadata$x)

## ----fig.align="center", fig.width=5------------------------------------------
c_spec(list(asp_example, ps_example), range = "common", res = 5) |> 
  plot()

## -----------------------------------------------------------------------------
# Extract the 150th spectrum. 
filter_spec(spectral_map, 150)

## -----------------------------------------------------------------------------
# Extract the spectrum with column name "8_5". 
filter_spec(spectral_map, "8_5") |> 
  print()

## ----fig.align="center", fig.width=5------------------------------------------
# Extract the spectra with a logical argument based on metadata
filter_spec(spectral_map, spectral_map$metadata$y == 1) |>
  plot()

## ----fig.align="center", fig.width=5------------------------------------------
sample_spec(spectral_map, size = 5) |>
  plot()

## -----------------------------------------------------------------------------
processed <- process_spec(raman_hdpe,
                          active = TRUE,
                          adj_intens = FALSE,
                          adj_intens_args = list(type = "none"),
                          conform_spec = TRUE,
                          conform_spec_args = list(range = NULL, res = 5,
                                                   type = "interp"),
                          restrict_range = FALSE,
                          restrict_range_args = list(min = 0, max = 6000),
                          flatten_range = FALSE,
                          flatten_range_args = list(min = 2200, max = 2420),
                          subtr_baseline = FALSE,
                          subtr_baseline_args = list(type = "polynomial",
                                                     degree = 8, raw = FALSE,
                                                     baseline = NULL),
                          smooth_intens = TRUE,
                          smooth_intens_args = list(polynomial = 3, window = 11,
                                                    derivative = 1, abs = TRUE),
                          make_rel = TRUE)
summary(processed)
summary(raman_hdpe)

## ----eval=FALSE---------------------------------------------------------------
#  plotly_spec(raman_hdpe, processed)

## ----eval=F-------------------------------------------------------------------
#  sig_noise(processed, metric = "run_sig_over_noise") >
#    sig_noise(raman_hdpe, metric = "run_sig_over_noise")

## ----out.width="100%"---------------------------------------------------------
spectral_map_p <- spectral_map |>
  process_spec(flatten_range = T)

spectral_map_p$metadata$sig_noise <- sig_noise(spectral_map_p)

heatmap_spec(spectral_map_p, sn = spectral_map_p$metadata$sig_noise, min_sn = 5)

## ----fig.align="center", out.width="100%"-------------------------------------
trans_raman_hdpe <- raman_hdpe
trans_raman_hdpe$spectra <- 2 - trans_raman_hdpe$spectra^2
    
rev_trans_raman_hdpe <- trans_raman_hdpe |> 
  adj_intens(type = "transmittance")

plotly_spec(trans_raman_hdpe, rev_trans_raman_hdpe)

## -----------------------------------------------------------------------------
conform_spec(raman_hdpe) |> # default convert res to 5 wavenumbers.
  summary()

# Force one spectrum to have the exact same wavenumbers as another
conform_spec(asp_example, range = ps_example$wavenumber, res = NULL) |> 
  summary()

# Specify the wavenumber resolution and use a rolling join instead of linear
# approximation (faster for large datasets). 
conform_spec(spectral_map, range = ps_example$wavenumber, res = 10,
             type = "roll") |> 
  summary()

## ----smooth_intens, fig.cap = "Sample `raman_hdpe` spectrum with different smoothing polynomials.", fig.width=5, fig.align="center"----
none <- make_rel(raman_hdpe)
p1 <- smooth_intens(raman_hdpe, polynomial = 1, derivative = 0, abs = F)
p4 <- smooth_intens(raman_hdpe, polynomial = 4, derivative = 0, abs = F)

c_spec(list(none, p1, p4)) |> 
  plot()

## ----compare_derivative, fig.cap = "Sample `raman_hdpe` spectrum with different derivatives.", fig.width=5, fig.align="center"----
none <- make_rel(raman_hdpe)
d1 <- smooth_intens(raman_hdpe, derivative = 1, abs = T)
d2 <- smooth_intens(raman_hdpe, derivative = 2, abs = T)

c_spec(list(none, d1, d2)) |> 
  plot()

## ----subtr_baseline, fig.cap = "Sample `raman_hdpe` spectrum with different degrees of background subtraction (Cowger et al., 2020).", fig.width=5, fig.align="center"----
alternative_baseline <- smooth_intens(raman_hdpe, polynomial = 1, window = 51,
                                      derivative = 0, abs = F, make_rel = F) |>
  flatten_range(min = 2700, max = 3200, make_rel = F)

none <- make_rel(raman_hdpe)
d2 <- subtr_baseline(raman_hdpe, type = "manual",
                     baseline = alternative_baseline)
d8 <- subtr_baseline(raman_hdpe, degree = 8)

c_spec(list(none, d2, d8)) |> 
  plot()

## ----restrict_range, fig.cap = "Sample `raman_hdpe` spectrum with different degrees of range restriction.", fig.width=5, fig.align="center"----
none <- make_rel(raman_hdpe)
r1 <- restrict_range(raman_hdpe, min = 1000, max = 2000)
r2 <- restrict_range(raman_hdpe, min = c(1000, 1800), max = c(1200, 2000))

compare_ranges <- c_spec(list(none, r1, r2), range = "common")
# Common argument crops the ranges to the most common range between the spectra
# when joining. 

plot(compare_ranges)

## ----flatten_range, fig.cap = "Sample `raman_hdpe` spectrum with different degrees of background subtraction (Cowger et al., 2020).", fig.width=5, fig.align="center"----
single <- filter_spec(spectral_map, 120) # Function to filter spectra by index
                                         # number or name or a logical vector. 
none <- make_rel(single)
f1 <- flatten_range(single) #default flattening the CO2 region. 
f2 <- flatten_range(single, min = c(1000, 2500), max = c(1200, 3000))

compare_flats <- c_spec(list(none, f1, f2))

plot(compare_flats)

## ----make_rel, fig.cap = "Sample `raman_hdpe` spectrum with one being relative and the other untransformed."----
relative <- make_rel(raman_hdpe)

## ----eval=F-------------------------------------------------------------------
#  get_lib(type = "derivative")

## ----eval=F-------------------------------------------------------------------
#  lib <- load_lib(type = "derivative")

## ----eval = F-----------------------------------------------------------------
#  data("test_lib")
#  data("raman_hdpe")
#  
#  processed <- process_spec(x = raman_hdpe,
#                            conform_spec = F, #We will conform during matching.
#                            smooth_intens = T #Conducts the default derivative transformation.
#                            )
#  
#  # Check to make sure that the signal to noise ratio of the processed spectra is
#  # greater than 10.
#  print(sig_noise(processed) > 10)
#  plotly_spec(raman_hdpe, processed)

## ----eval=FALSE---------------------------------------------------------------
#  matches <- match_spec(x = processed, library = test_lib, conform = T,
#                        add_library_metadata = "sample_name", top_n = 5)
#  print(matches[,c("object_id", "library_id", "match_val", "SpectrumType",
#                   "SpectrumIdentity")])

## ----eval=FALSE---------------------------------------------------------------
#  get_metadata(x = test_lib, logic = matches[[1,"library_id"]], rm_empty = T)

## ----eval=FALSE---------------------------------------------------------------
#  plotly_spec(processed, filter_spec(test_lib, logic = matches[[1,"library_id"]]))

## ----eval = F-----------------------------------------------------------------
#  data("test_lib")
#  test_map <- read_any(read_extdata("CA_tiny_map.zip"))
#  
#  test_map_processed <- process_spec(test_map, conform_spec_args = list(
#    range = test_lib$wavenumber, res = NULL)
#    )
#  
#  identities <- match_spec(test_map_processed, test_lib, order = test_map,
#                           add_library_metadata = "sample_name", top_n = 1)
#  
#  features <- ifelse(identities$match_val > 0.7,
#                     tolower(identities$polymer_class), "unknown")
#  
#  id_map <- def_features(x = test_map_processed, features = features)
#  
#  id_map$metadata$identities <- features
#  # Also should probably be implemented automatically in the function when a
#  # character value is provided.
#  
#  # Collapses spectra to their median for each particle
#  test_collapsed <- collapse_spec(id_map)

## ----eval = F-----------------------------------------------------------------
#  data("test_lib")
#  test_map <- read_any(read_extdata("CA_tiny_map.zip"))
#  
#  # Characterize the total signal as a threshold value.
#  snr <- sig_noise(test_map,metric = "log_tot_sig")
#  
#  # Use this to find your particles and the sig_noise value to use for
#  # thresholding.
#  heatmap_spec(test_map, z = snr)
#  
#  # Set define the feature regions based on the threshold. 400 appeared to be
#  # where I suspected my particle to be in the previous heatmap.
#  id_map <- def_features(x = test_map, features = snr > 400)
#  
#  # Check that the thresholding worked as expected.
#  heatmap_spec(id_map, z = id_map$metadata$feature_id)
#  
#  # Collapse the spectra to their medians based on the threshold. Important to
#  # note here that the particles with id -88 are anything from the FALSE values
#  # so they should be your background.
#  collapsed_id_map <- id_map |>
#    collapse_spec()
#  
#  # Process the collapsed spectra.
#  id_map_processed <- process_spec(collapsed_id_map, conform_spec_args = list(
#    range = test_lib$wavenumber, res = NULL)
#    )
#  
#  # Check the spectra.
#  plot(id_map_processed)
#  
#  # Get the matches of the collapsed spectra for the particles.
#  matches <- match_spec(id_map_processed, test_lib,
#                        add_library_metadata = "sample_name", top_n = 1)

