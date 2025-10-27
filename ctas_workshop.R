#===============================================================================
# Clinical Timeseries Anomaly Spotter (CTAS) Workshop
# Author: Pekka Tiikkainen
# Date: 2025-11-17
#===============================================================================

setwd("C:/Users/glutk/OneDrive - Bayer/Projects/Time line similarity/phuse_workshop")

# Load required libraries
library(ctas)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(tidyr)
library(scales)
library(RColorBrewer)
library(pharmaversesdtm)
library(dbscan)

source("ctas_workshop_functions.R")

# Set seed for reproducibility
set.seed(42)

#===============================================================================
# SECTION 1: PREPARING STUDY DATA
#===============================================================================


# Pull and wrange clinical trial data from the pharmaversesdtm package.
input_data <- wrangle_input_data()

data <- input_data[[1]]
parameters <- input_data[[2]]
subjects <- input_data[[3]]


# Introduce anomalies for specific sites
anomaly_configs <- c(
  "site_id=716;anomaly_type=co_cluster;anomaly_magnitude=0.5;parameter_id=HGB",
  "site_id=708;anomaly_type=variance;anomaly_magnitude=20;parameter_id=PROT"
)

anomalies <- apply_anomaly_configs(data, subjects, anomaly_configs)

data_with_anomalies <- anomalies$data



#===============================================================================
# SECTION 2: RUNNING CTAS ANALYSIS
#===============================================================================

# Create empty custom timeseries and reference groups (required by CTAS)
custom_timeseries <- data.frame(
  timeseries_id = character(0),
  parameter_id = character(0),
  timepoint_combo = character(0)
)

custom_reference_groups <- data.frame(
  parameter_id = character(0),
  feature = character(0),
  ref_group = character(0)
)

# Define features to calculate
features_to_calculate <- c(
  "autocorr",
  "average", 
  "sd",
  "unique_value_count_relative",
  "range",
  "lof"
) %>% paste(collapse = ";")


# Run CTAS analysis
ctas_results <- process_a_study(
  data = data_with_anomalies,
  subjects = subjects,
  parameters = parameters,
  custom_timeseries = custom_timeseries,
  custom_reference_groups = custom_reference_groups,
  default_timeseries_features_to_calculate = features_to_calculate,
  default_minimum_timepoints_per_series = 3,
  default_minimum_subjects_per_series = 20,
  default_max_share_missing_timepoints_per_series = 0.3,
  default_generate_change_from_baseline = FALSE,
  autogenerate_timeseries = TRUE
)


# Extract individual results tables as separate data frames
timeseries_features <- ctas_results$timeseries_features
timeseries <- ctas_results$timeseries
PCA_coordinates <- ctas_results$PCA_coordinates
site_scores <- ctas_results$site_scores

#===============================================================================
# SECTION 3: ANALYZING RESULTS
#===============================================================================

# An overview table for site scores
results_summary_df <- create_results_summary_df()

#############################################################
# Analysis of temperature measurement anomalies at site 710
#############################################################

# Compare time series features of temperature measurements (first five timepoints) of site 710 with other sites
feature_comparison_plot_710_temp <- create_site_feature_comparison("710", "ts_126_autogen_original")
plot(feature_comparison_plot_710_temp)

# Compare site 710's temperature time series with the study average. The site gets flagged as their
# temperature measurements are around 0.5 degree greater than the study average.
timeseries_plot_710_temp <- create_site_timeseries_with_area("710", "ts_126_autogen_original")
plot(timeseries_plot_710_temp)

# Plot temperature time series for each of 710's subjects in its own trellis. In addition to exhibiting
# systematically larger values, the number of unique values per timeseries is also lower than at the study overall.
individual_timeseries_plot_710_temp <- create_individual_timeseries_trellis("710", "ts_126_autogen_original", ncol=5)
plot(individual_timeseries_plot_710_temp)

#############################################################
# Protein measurement anomalies at site 708
#############################################################

# Compare time series features for protein measurements of site 708 with other sites.
feature_comparison_plot_708_protein <- create_site_feature_comparison("708", "ts_80_autogen_original", bin_count=10)
plot(feature_comparison_plot_708_protein)

# We spiked increased variation into site 708's protein measurements. It is evident that this the range
# results is much wider than in the study overall.
timeseries_plot_708_prot <- create_site_timeseries_with_area("708", "ts_80_autogen_original")
plot(timeseries_plot_708_prot)

# Plot protein time series for each of 708's subjects in its own trellis
individual_timeseries_plot_708_prot <- create_individual_timeseries_trellis("708", "ts_80_autogen_original", ncol=5)
plot(individual_timeseries_plot_708_prot)

#############################################################
# Hemoglobin time series co-clustering at site 716.
#############################################################

# Plot hemoglobin profile similarities and highlight site 716 subjects to visualize co-clustering.
pca_plot_716_hgb <- create_pca_plot(PCA_coordinates, subjects, timeseries_id = "ts_46_autogen_original", highlight_sites = "716")
plot(pca_plot_716_hgb)

# Compare 716's hemoglobin time series with the study average
timeseries_plot_716_hgb <- create_site_timeseries_with_area("716", "ts_46_autogen_original")
plot(timeseries_plot_716_hgb)





