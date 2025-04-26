## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig_width = 6,
  fig_height = 4,
  dpi = 72
)

data.table::setDTthreads(2)

## ----setup--------------------------------------------------------------------
library(OpenSpecy)

## ---- eval=FALSE--------------------------------------------------------------
#  run_app()

## ---- eval=FALSE--------------------------------------------------------------
#  spectra <- read_any("path/to/your/data")

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

## ---- eval=F------------------------------------------------------------------
#  write_spec(scratch_OpenSpecy, "test_scratch_OpenSpecy.yml", digits = 5)
#  write_spec(scratch_OpenSpecy, "test_scratch_OpenSpecy.json", digits = 5)
#  write_spec(scratch_OpenSpecy, "test_scratch_OpenSpecy.csv", digits = 5)

## ---- eval=F------------------------------------------------------------------
#  hyperspecy <- as_hyperSpec(scratch_OpenSpecy)

## ---- fig.align="center", fig.width=5-----------------------------------------
plot(scratch_OpenSpecy) # quick and efficient

## ---- eval=F------------------------------------------------------------------
#  # This will min-max normalize your data even if it isn't already but are not
#  # changing your underlying data
#  plotly_spec(scratch_OpenSpecy, json_example)

## ---- eval=F------------------------------------------------------------------
#  heatmap_spec(spectral_map,
#               z = spectral_map$metadata$x)

## ---- eval=F------------------------------------------------------------------
#  interactive_plot(spectral_map, select = 100, z = spectral_map$metadata$x)

## -----------------------------------------------------------------------------
combined <- c_spec(list(asp_example, ps_example), range = "common", res = 8) 

combined |> 
  plot()

## -----------------------------------------------------------------------------
plot(combined |> make_rel(), offset = 3, legend_var = "file_name")

## ---- eval=F------------------------------------------------------------------
#  # Extract the 150th spectrum by index number.
#  filter_spec(spectral_map, 150)
#  # Extract the 150th spectrum by spectrum name.
#  filter_spec(spectral_map, "9_5")
#  # Extract the 150th spectrum by logical test.
#  filter_spec(spectral_map, spectral_map$metadata$x == 9 &  spectral_map$metadata$y == 5)
#  
#  #Test that they are the same.
#  identical(filter_spec(spectral_map, 150), filter_spec(spectral_map, "9_5"))
#  
#  identical(filter_spec(spectral_map, 150), filter_spec(spectral_map, spectral_map$metadata$y == 9 & spectral_map$metadata$x == 5))

## -----------------------------------------------------------------------------
sample_spec(spectral_map, size = 5) |>
  plot()

## ----eval=FALSE---------------------------------------------------------------
#  processed <- process_spec(raman_hdpe)

## ---- eval=FALSE--------------------------------------------------------------
#  plotly_spec(raman_hdpe, processed)

## ---- eval=F------------------------------------------------------------------
#  # Automatic signal to noise ratio comparison
#  sig_noise(processed, metric = "run_sig_over_noise") >
#    sig_noise(raman_hdpe, metric = "run_sig_over_noise")
#  
#  #Manual signal to noise ratio calculation
#  sig_noise(processed, metric = "sig_over_noise", sig_min = 2700, sig_max = 3000, noise_min = 1500, noise_max = 2500) >
#    sig_noise(raman_hdpe, metric = "sig_over_noise", sig_min = 2700, sig_max = 3000, noise_min = 1500, noise_max = 2500)

## ---- eval=FALSE--------------------------------------------------------------
#  #Remove CO2 region
#  spectral_map_p <- spectral_map |>
#    process_spec(flatten_range = T)
#  
#  #Calculate signal times noise
#  spectral_map_p$metadata$sig_noise <- sig_noise(spectral_map_p, metric = "run_sig_over_noise")
#  
#  #Plot result
#  heatmap_spec(spectral_map_p, sn = spectral_map_p$metadata$sig_noise, min_sn = 5)

## ---- eval=FALSE--------------------------------------------------------------
#  trans_raman_hdpe <- raman_hdpe
#  trans_raman_hdpe$spectra <- 2 - trans_raman_hdpe$spectra^2
#  
#  rev_trans_raman_hdpe <- trans_raman_hdpe |>
#    adj_intens(type = "transmittance")
#  
#  plotly_spec(trans_raman_hdpe, rev_trans_raman_hdpe)

## ---- eval=FALSE--------------------------------------------------------------
#  conform_spec(raman_hdpe, res = 8) |> # Convert res to 8 wavenumbers.
#    summary()
#  
#  # Force one spectrum to have the exact same wavenumbers as another
#  conform_spec(asp_example, range = ps_example$wavenumber, res = NULL) |>
#    summary()
#  

## ----smooth_intens, fig.cap = "Sample `raman_hdpe` spectrum with different smoothing polynomials."----
none <- make_rel(raman_hdpe)
p1 <- smooth_intens(raman_hdpe, polynomial = 1, derivative = 0, abs = F)
p4 <- smooth_intens(raman_hdpe, polynomial = 4, derivative = 0, abs = F)

c_spec(list(none, p1, p4)) |> 
  plot()

## ----compare_derivative, fig.cap = "Sample `raman_hdpe` spectrum with different derivatives."----
none <- make_rel(raman_hdpe)
window <- calc_window_points(raman_hdpe, 100) #Calculate the number of points needed for a 190 wavenumber window.
d1 <- smooth_intens(raman_hdpe, derivative = 1, window = window, abs = T)
d2 <- smooth_intens(raman_hdpe, derivative = 2, window = window, abs = T)

c_spec(list(none, d1, d2)) |> 
  plot()

## ----subtr_baseline, fig.cap = "Sample `raman_hdpe` spectrum with different degrees of background subtraction (Cowger et al., 2020)."----
alternative_baseline <- smooth_intens(raman_hdpe, polynomial = 1, window = 51,
                                      derivative = 0, abs = F, make_rel = F) |>
  flatten_range(min = 2700, max = 3200, make_rel = F) #Manual baseline with heavily smoothed spectra

none <- make_rel(raman_hdpe) #raw
d <- subtr_baseline(raman_hdpe, type = "manual",
                     baseline = alternative_baseline) #manual subtraction
d8 <- subtr_baseline(raman_hdpe, degree = 8) #standard imodpolyfit
dr <- subtr_baseline(raman_hdpe, refit_at_end = T) #optionally retain baseline noise with refitting

c_spec(list(none, d, d8, dr)) |> 
  plot(offset = 0.25)

## ----restrict_range, fig.cap = "Sample `raman_hdpe` spectrum with different degrees of range restriction."----
none <- make_rel(raman_hdpe)

#Specify one range
r1 <- restrict_range(raman_hdpe, min = 1000, max = 2000) |>
    conform_spec(range = none$wavenumber, res = NULL, allow_na = T)

#Specify multiple ranges
r2 <- restrict_range(raman_hdpe, min = c(1000, 1800), max = c(1200, 2000)) |>
        conform_spec(range = none$wavenumber, res = NULL, allow_na = T)

compare_ranges <- c_spec(list(none, r1, r2), range = "common")
# Common argument crops the ranges to the most common range between the spectra
# when joining. 

plot(compare_ranges)

## ----flatten_range, fig.cap = "Sample `raman_hdpe` spectrum with different degrees of background subtraction (Cowger et al., 2020)."----
single <- filter_spec(spectral_map, 120) # Function to filter spectra by index
                                         # number or name or a logical vector. 
none <- make_rel(single)
f1 <- flatten_range(single) #default flattening the CO2 region. 
f2 <- flatten_range(single, min = c(1000, 2500), max = c(1200, 3000)) #multple range example

compare_flats <- c_spec(list(none, f1, f2), range = "common")

plot(compare_flats, offset = 0.25)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  raman_hdpe |> plot()
#  
#  make_rel(raman_hdpe) |> plot()
#  

## ---- eval=F------------------------------------------------------------------
#  get_lib(type = "derivative")

## ---- eval=F------------------------------------------------------------------
#  lib <- load_lib(type = "derivative")

## ---- eval = F----------------------------------------------------------------
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
#  
#  #Plot to assess the accuracy of the processing visually
#  plotly_spec(raman_hdpe, processed)

## ---- eval=FALSE--------------------------------------------------------------
#  matches <- match_spec(x = processed, library = test_lib, conform = T,
#                        add_library_metadata = "sample_name", top_n = 5)[order(match_val, decreasing = T)]
#  print(matches[,c("object_id", "library_id", "match_val", "SpectrumType",
#                   "SpectrumIdentity")])

## ---- eval=FALSE--------------------------------------------------------------
#  get_metadata(x = test_lib, logic = matches[[1,"library_id"]], rm_empty = T)

## ---- eval=FALSE--------------------------------------------------------------
#  plotly_spec(processed, filter_spec(test_lib, logic = matches[[1,"library_id"]]))

## ---- eval = F----------------------------------------------------------------
#  #Test library
#  data("test_lib")
#  
#  #Example hyperspectral image with one cellulose acetate particle in the middle of it.
#  test_map <- read_any(read_extdata("CA_tiny_map.zip"))
#  
#  #Process the map to conform to the library.
#  test_map_processed <- process_spec(test_map, conform_spec_args = list(
#    range = test_lib$wavenumber, res = NULL)
#    )
#  
#  #Identify every spectrum in the map.
#  identities <- match_spec(test_map_processed, test_lib, order = test_map,
#                           add_library_metadata = "sample_name", top_n = 1)
#  
#  #Relabel any spectra with low correlation coefficients.
#  features <- ifelse(identities$match_val > 0.7,
#                     tolower(identities$polymer_class), "unknown")
#  
#  #Use spectra identities to identify particle regions as those that have the same material type and are touching.
#  id_map <- def_features(x = test_map_processed, features = features)
#  
#  id_map$metadata$identities <- features
#  # Also should probably be implemented automatically in the function when a
#  # character value is provided.
#  heatmap_spec(id_map, z = id_map$metadata$identities)
#  
#  # Collapses spectra to their median for each particle
#  test_collapsed <- collapse_spec(id_map)
#  
#  # Plot spectra for each identified particle
#  plot(test_collapsed, offset = 1, legend_var = "feature_id")

## ---- eval = F----------------------------------------------------------------
#  # Read in test library
#  data("test_lib")
#  
#  # Example dataset with one cellulose acetate particle.Conduct spatial smoothing to average each spectrum using adjacent spectra.
#  test_map <- read_any(read_extdata("CA_tiny_map.zip"),
#                       spectral_smooth = T,
#                       sigma = c(1, 1, 1))
#  
#  # Characterize the signal times noise to determine where particle regions are.
#  snr <- sig_noise(test_map, metric = "sig_times_noise")
#  
#  # Use this to find your particles and the idal signal times noise value to use for thresholding.
#  heatmap_spec(test_map, z = snr)
#  
#  # Define the feature regions based on the threshold. Pixels from the background in the heatmap above were below 0.05 while my particle's pixels were above so I set snr > 0.05.
#  id_map <- def_features(x = test_map, features = snr > 0.05)
#  
#  # Check that the thresholding worked as expected. Here we see a single particle region identified separate from the background.
#  heatmap_spec(id_map, z = id_map$metadata$feature_id)
#  
#  # Collapse the spectra to their medians based on the threshold. Important to
#  # note here that the particles with id -88 are anything from the FALSE values
#  # so they should be your background.
#  collapsed_id_map <- id_map |>
#    collapse_spec()
#  
#  # Process the collapsed spectra to have the same transformation and units as the library.
#  id_map_processed <- process_spec(collapsed_id_map, conform_spec_args = list(
#    range = test_lib$wavenumber, res = NULL)
#    )
#  
#  # Check the spectra for the background and particle. Background has considerable signal in it too suggesting double bounce along the edges of the particle.
#  plot(id_map_processed, offset = 1, legend_var = "feature_id")
#  
#  # Get the matches of the collapsed spectra for the particles.
#  matches <- match_spec(id_map_processed, test_lib,
#                        add_library_metadata = "sample_name", top_n = 1)
#  

