#' Introduce anomalies in specific sites
#' @param data Original time series data (long format)
#' @param subjects Subject information (must include subject_id and site)
#' @param anomalous_sites Vector of site names to make anomalous
#' @param anomaly_type Type of anomaly:
#'        "shift"   -> constant shift to all measurements
#'        "variance"-> adds random noise to increase variance
#'        "trend"   -> adds linear trend over time
#'        "repeat_values" -> forces repeated identical values in runs
#'        "co_cluster" -> makes time series within anomalous sites more similar
#' @param anomaly_magnitude Magnitude of anomaly (used by shift/variance/trend/co_cluster)
#'        For co_cluster: fraction to move towards site mean (0-1, e.g., 0.5 = move halfway)
#' @param repeat_prob Probability to start a repeated-value run when scanning timepoints (0-1)
#' @param repeat_run_min Minimum length of a repeated-value run (>=2 recommended)
#' @param repeat_run_max Maximum length of a repeated-value run (>= repeat_run_min)
#' @param affected_share Share of subjects at anomalous sites to apply the anomaly to (0-1)
#' @return Modified data with anomalies
introduce_site_anomalies <- function(data, subjects, anomalous_sites, 
                                     anomaly_type = "shift", anomaly_magnitude = 20,
                                     repeat_prob = 0.25, repeat_run_min = 2, repeat_run_max = 3,
                                     affected_share = 1.0, target_parameter = "all") {
  
  stopifnot(repeat_run_min >= 1, repeat_run_max >= repeat_run_min)
  stopifnot(affected_share >= 0 && affected_share <= 1)
  
  # Get subjects from anomalous sites
  anomalous_subjects <- subjects %>%
    dplyr::filter(site %in% anomalous_sites) %>%
    dplyr::pull(subject_id)
  
  if (length(anomalous_subjects) == 0) return(data)
  
  # Optionally only affect a subset of anomalous-site subjects
  n_affect <- ceiling(length(anomalous_subjects) * affected_share)
  subjects_to_affect <- if (n_affect > 0) sample(anomalous_subjects, n_affect) else character(0)
  
  # Filter by parameter if specified
  if(target_parameter != "all") {
    
    modified_data <- data %>%
      filter(parameter_id == target_parameter)
    
    unmodified_data <- data %>%
      filter(parameter_id != target_parameter)
    
  } else {
    
    modified_data <- data
    
  }
  
  if (anomaly_type == "shift") {
    
    modified_data <- modified_data %>%
      dplyr::mutate(result = ifelse(subject_id %in% anomalous_subjects,
                                    result + anomaly_magnitude,
                                    result))
    
  } else if (anomaly_type == "variance") {
    
    modified_data <- modified_data %>%
      dplyr::group_by(subject_id) %>%
      dplyr::mutate(result = ifelse(subject_id %in% anomalous_subjects,
                                    result + rnorm(dplyr::n(), 0, anomaly_magnitude),
                                    result)) %>%
      dplyr::ungroup()
    
  } else if (anomaly_type == "trend") {
    
    modified_data <- modified_data %>%
      dplyr::mutate(result = ifelse(subject_id %in% anomalous_subjects,
                                    result + (timepoint_rank - 1) * anomaly_magnitude,
                                    result))
    
  } else if (anomaly_type == "co_cluster") {
    
    # For co_cluster, anomaly_magnitude should be between 0 and 1
    cluster_strength <- pmax(0, pmin(1, anomaly_magnitude))
    
    # Calculate site-specific means for each parameter and timepoint
    site_means <- modified_data %>%
      dplyr::left_join(subjects, by = "subject_id") %>%
      dplyr::filter(site %in% anomalous_sites) %>%
      dplyr::group_by(site, parameter_id, timepoint_rank) %>%
      dplyr::summarise(site_mean = mean(result, na.rm = TRUE), .groups = "drop")
    
    # Move anomalous subjects' values closer to their site mean
    modified_data <- modified_data %>%
      dplyr::left_join(subjects, by = "subject_id") %>%
      dplyr::left_join(site_means, by = c("site", "parameter_id", "timepoint_rank")) %>%
      dplyr::mutate(
        result = ifelse(
          subject_id %in% subjects_to_affect & !is.na(site_mean),
          result + cluster_strength * (site_mean - result),
          result
        )
      ) %>%
      dplyr::select(-site, -site_mean)  # Remove temporary columns
    
  } else if (anomaly_type == "repeat_values") {
    
    # Helper to apply repeated-value runs within one subject-parameter series
    apply_repeat_runs <- function(df, repeat_prob, run_min, run_max) {
      # Ensure time order
      df <- df[order(df$timepoint_rank), ]
      
      n <- nrow(df)
      if (n < 2) return(df)  # Need at least 2 timepoints to form a run
      
      runs_created <- 0
      i <- 1
      while (i <= n) {
        if (stats::runif(1) < repeat_prob) {
          L <- sample(run_min:run_max, size = 1)
          end_idx <- min(n, i + L - 1)
          
          # Choose the value to repeat
          base_val <- df$result[i]
          
          # If base is NA, try previous non-NA, baseline, then median, then 0
          if (is.na(base_val)) {
            # previous non-NA within series
            prev_idx <- if (i > 1) max(which(!is.na(df$result[1:(i - 1)])), na.rm = TRUE) else NA
            if (is.finite(prev_idx)) base_val <- df$result[prev_idx]
            # use baseline at start if available
            if (is.na(base_val) && !is.null(df$baseline)) {
              base_val <- df$baseline[i]
            }
            # series median if still NA
            if (is.na(base_val) && any(!is.na(df$result))) {
              base_val <- stats::median(df$result, na.rm = TRUE)
            }
            # fallback
            if (is.na(base_val)) base_val <- 0
          }
          
          df$result[i:end_idx] <- base_val
          runs_created <- runs_created + 1
          i <- end_idx + 1
        } else {
          i <- i + 1
        }
      }
      
      # Ensure at least one repeated run was created
      if (runs_created == 0) {
        if (n >= 2) {
          start <- sample(1:(n - 1), size = 1)
          L <- sample(run_min:run_max, size = 1)
          end_idx <- min(n, start + L - 1)
          
          base_val <- df$result[start]
          if (is.na(base_val)) {
            prev_idx <- if (start > 1) max(which(!is.na(df$result[1:(start - 1)])), na.rm = TRUE) else NA
            if (is.finite(prev_idx)) base_val <- df$result[prev_idx]
            if (is.na(base_val) && !is.null(df$baseline)) base_val <- df$baseline[start]
            if (is.na(base_val) && any(!is.na(df$result))) base_val <- stats::median(df$result, na.rm = TRUE)
            if (is.na(base_val)) base_val <- 0
          }
          df$result[start:end_idx] <- base_val
        }
      }
      
      df
    }
    
    # Apply only to the chosen subset of anomalous-site subjects
    modified_data <- modified_data %>%
      dplyr::group_by(subject_id, parameter_id) %>%
      dplyr::group_modify(~{
        if (.y$subject_id %in% subjects_to_affect) {
          apply_repeat_runs(.x, repeat_prob = repeat_prob,
                            run_min = repeat_run_min, run_max = repeat_run_max)
        } else {
          .x
        }
      }) %>%
      dplyr::ungroup()
    
  } else {
    warning("Unknown anomaly_type: ", anomaly_type, ". Returning data unchanged.")
  }
  
  
  modified_data <- bind_rows(modified_data, unmodified_data)
  
  return(modified_data)
}



apply_anomaly_configs <- function(data_original, subjects, anomaly_configs) {
  data_with_anomalies <- data_original
  anomalous_sites <- c()
  
  for(config in anomaly_configs) {
    # Parse key=value pairs
    pairs <- strsplit(config, ";")[[1]]
    config_params <- list()
    
    for(pair in pairs) {
      kv <- strsplit(pair, "=")[[1]]
      if(length(kv) == 2) {
        key <- trimws(kv[1])
        value <- trimws(kv[2])
        config_params[[key]] <- value
      } else {
        warning(paste("Invalid key=value pair:", pair, "in config:", config))
      }
    }
    
    # Extract and validate required parameters
    site_id <- config_params[["site_id"]]
    anomaly_type <- config_params[["anomaly_type"]]
    anomaly_magnitude <- as.numeric(config_params[["anomaly_magnitude"]])
    parameter_id <- config_params[["parameter_id"]]
    repeat_prob <- as.numeric(config_params[["repeat_prob"]])
    repeat_run_min <- as.numeric(config_params[["repeat_run_min"]])
    repeat_run_max <- as.numeric(config_params[["repeat_run_max"]])
    affected_share <- as.numeric(config_params[["affected_share"]])
    
    # Validation
    if(is.null(site_id) || is.null(anomaly_type) || is.null(parameter_id)) {
      warning(paste("Missing required parameters in config:", config, "- skipping"))
      next
    }
    
    if(anomaly_type != "repeat_values") {
      if(is.na(anomaly_magnitude)) {
        warning(paste("Invalid anomaly_magnitude in config:", config, "- skipping"))
        next
      }
    }
    
    if(!anomaly_type %in% c("shift", "variance", "trend", "repeat_values", "co_cluster")) {
      warning(paste("Unknown anomaly_type:", anomaly_type, "in config:", config, "- skipping"))
      next
    }
    
    if(!site_id %in% subjects$site) {
      warning(paste("Site", site_id, "not found in subjects data - skipping"))
      next
    }
    
    cat(paste("Applying anomaly_type =", anomaly_type, 
              ", anomaly_magnitude =", anomaly_magnitude, 
              "to site_id =", site_id, 
              "for parameter_id =", parameter_id, "\n"))
    
    if(anomaly_type == "repeat_values") {
      
      data_with_anomalies <- introduce_site_anomalies(
        data_with_anomalies, 
        subjects, 
        site_id, 
        anomaly_type = anomaly_type,
        repeat_prob = repeat_prob, 
        repeat_run_min = repeat_run_min,
        repeat_run_max = repeat_run_max,
        affected_share = affected_share,
        target_parameter = parameter_id
      )
      
    } else {
      
      data_with_anomalies <- introduce_site_anomalies(
        data_with_anomalies, 
        subjects, 
        site_id, 
        anomaly_type = anomaly_type, 
        anomaly_magnitude = anomaly_magnitude,
        target_parameter = parameter_id
      )
      
    }
    
    
    
    
    anomalous_sites <- c(anomalous_sites, site_id)
  }
  
  return(list(
    data = data_with_anomalies,
    anomalous_sites = unique(anomalous_sites)
  ))
  
}



# Function to create site comparison histograms
create_site_feature_comparison <- function(target_site, target_timeseries_id, bin_count = 15) {
  
  # Get feature data for the specified timeseries and join with subject site info
  feature_data <- timeseries_features %>%
    filter(timeseries_id == target_timeseries_id) %>%
    mutate(
      site_group = ifelse(site == target_site, 
                          paste("Site", target_site), 
                          "All Other Sites")
    )
  
  # Look up the parameter name and timepoint count for the timeseries
  timeseries_this <- timeseries %>%
    filter(timeseries_id == target_timeseries_id)
  
  parameter_name <- timeseries_this$parameter_id[[1]]
  timepoint_count <- timeseries_this$timepoint_count[[1]]
  
  # Create named vector for colors
  color_values <- c("#74A9CF", "#FD8D3C")
  names(color_values) <- c("All Other Sites", paste("Site", target_site))
  
  # Create the plot
  p <- ggplot(feature_data, aes(x = feature_value, fill = site_group)) +
    geom_histogram(
      data = filter(feature_data, site_group == "All Other Sites"),
      aes(y = -after_stat(count/sum(count))), # Negative y for bottom histograms, scaled by proportion
      alpha = 0.7,
      bins = bin_count
    ) +
    geom_histogram(
      data = filter(feature_data, site_group == paste("Site", target_site)),
      aes(y = after_stat(count/sum(count))), # Positive y for top histograms, scaled by proportion
      alpha = 0.7,
      bins = bin_count
    ) +
    facet_wrap(~ feature, scales = "free_x", ncol = 4) + # Changed to free_x only
    scale_fill_manual(values = color_values) +
    scale_y_continuous(
      labels = function(x) paste0(abs(x) * 100, "%"), # Show as percentages
      name = "Proportion of Site Group"
    ) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    labs(
      title = paste("Feature Distribution Comparison: Site", target_site, "vs All Other Sites"),
      subtitle = paste("Parameter:", parameter_name, ", timepoint count: ", timepoint_count),
      x = "Feature Value",
      fill = "Site Group"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}






# Function to create side-by-side timeseries with contours for target site and other sites
create_site_timeseries_with_area <- function(target_site, target_timeseries_id, 
                                             coverage_levels = c(0.50, 0.60, 0.70, 0.80, 0.90, 0.95),
                                             show_mean = TRUE) {
  
  # Load patchwork for combining plots (install if needed: install.packages("patchwork"))
  if (!require(patchwork)) {
    stop("Please install patchwork package: install.packages('patchwork')")
  }
  
  # Get timeseries info to determine parameter and baseline
  ts_info <- timeseries %>%
    filter(timeseries_id == target_timeseries_id) %>%
    select(parameter_id, baseline, timepoint_combo) %>%
    mutate(timepoint_combo = str_split(timepoint_combo, ';'))
  
  if (nrow(ts_info) == 0) {
    stop("Timeseries ID not found")
  }
  
  # Get parameter name for title
  param_info <- parameters %>%
    filter(parameter_id == ts_info$parameter_id[1]) %>%
    select(parameter_name)
  
  # Filter and prepare the data
  plot_data <- data_with_anomalies %>%
    filter(parameter_id == ts_info$parameter_id[1]
           & timepoint_rank %in% ts_info$timepoint_combo[[1]]) %>%
    left_join(subjects, by = "subject_id") %>%
    rowwise() %>%
    mutate(
      is_target_site = site == target_site,
      # Use appropriate measurement based on baseline setting
      measurement = ifelse(ts_info$baseline[1] == "original", result, result - baseline)
    ) %>%
    filter(!is.na(measurement)) %>%
    arrange(subject_id, timepoint_rank)
  
  # Separate target site and other sites data
  target_site_data <- plot_data %>%
    filter(is_target_site == TRUE)
  
  other_sites_data <- plot_data %>%
    filter(is_target_site == FALSE)
  
  # Sort coverage levels in descending order (so we plot widest bands first)
  coverage_levels <- sort(coverage_levels, decreasing = TRUE)
  
  # Create color palette from dark blue to light blue
  blue_palette <- colorRampPalette(c("#08519c", "#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff"))(length(coverage_levels))
  
  # Create color palette from dark red to light red for target site
  red_palette <- colorRampPalette(c("#a50f15", "#de2d26", "#fb6a4a", "#fc9272", "#fcbba1", "#fee5d9"))(length(coverage_levels))
  
  # Function to calculate coverage contours for a dataset
  calculate_coverage_contours <- function(data, palette) {
    coverage_data_list <- lapply(seq_along(coverage_levels), function(i) {
      coverage <- coverage_levels[i]
      alpha <- (1 - coverage) / 2
      lower_q <- alpha
      upper_q <- 1 - alpha
      
      data %>%
        group_by(timepoint_rank, timepoint_1_name) %>%
        summarise(
          lower_bound = quantile(measurement, lower_q, na.rm = TRUE),
          upper_bound = quantile(measurement, upper_q, na.rm = TRUE),
          mean_val = mean(measurement, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(coverage = coverage,
               coverage_label = paste0(coverage * 100, "%"),
               color = palette[i])
    })
    bind_rows(coverage_data_list)
  }
  
  # Calculate contours for both datasets
  target_coverage_data <- calculate_coverage_contours(target_site_data, red_palette)
  other_coverage_data <- calculate_coverage_contours(other_sites_data, blue_palette)
  
  # Calculate mean for each group
  target_mean <- target_site_data %>%
    group_by(timepoint_rank, timepoint_1_name) %>%
    summarise(mean_val = mean(measurement, na.rm = TRUE), .groups = "drop")
  
  other_mean <- other_sites_data %>%
    group_by(timepoint_rank, timepoint_1_name) %>%
    summarise(mean_val = mean(measurement, na.rm = TRUE), .groups = "drop")
  
  # Get subject counts
  target_site_count <- target_site_data %>%
    distinct(subject_id) %>%
    nrow()
  
  other_sites_count <- other_sites_data %>%
    distinct(subject_id) %>%
    nrow()
  
  # Calculate common y-axis limits
  y_min <- min(c(target_coverage_data$lower_bound, other_coverage_data$lower_bound), na.rm = TRUE)
  y_max <- max(c(target_coverage_data$upper_bound, other_coverage_data$upper_bound), na.rm = TRUE)
  y_range <- y_max - y_min
  y_limits <- c(y_min - 0.05 * y_range, y_max + 0.05 * y_range)
  
  # Get x-axis breaks and labels
  x_breaks <- unique(plot_data$timepoint_rank)
  x_labels <- plot_data %>%
    distinct(timepoint_rank, timepoint_1_name) %>%
    arrange(timepoint_rank) %>%
    pull(timepoint_1_name)
  
  # Create base y-axis label
  y_label <- ifelse(ts_info$baseline[1] == "original", "Measurement", "Baseline-Adjusted Measurement")
  
  # Function to create a plot
  create_contour_plot <- function(coverage_data, mean_data, palette, title, color_name, 
                                  subject_count, legend_position = "right", show_y_text = TRUE) {
    p <- ggplot()
    
    # Add coverage ribbons
    for (i in seq_along(coverage_levels)) {
      coverage_subset <- coverage_data %>% filter(coverage == coverage_levels[i])
      p <- p + geom_ribbon(data = coverage_subset, 
                           aes(x = timepoint_rank, ymin = lower_bound, ymax = upper_bound,
                               fill = coverage_label),
                           alpha = 0.6)
    }
    
    # Manual fill scale
    p <- p + scale_fill_manual(
      name = "Coverage",
      values = setNames(palette, paste0(coverage_levels * 100, "%")),
      breaks = paste0(sort(coverage_levels, decreasing = FALSE) * 100, "%")
    )
    
    # Add mean line if requested
    if (show_mean) {
      mean_color <- ifelse(color_name == "red", "darkred", "darkblue")
      p <- p + geom_line(data = mean_data, 
                         aes(x = timepoint_rank, y = mean_val),
                         color = mean_color, size = 1, linetype = "dashed", alpha = 0.9)
    }
    
    # Customize axes
    p <- p + scale_x_continuous(breaks = x_breaks, labels = x_labels) +
      scale_y_continuous(limits = y_limits)
    
    # Add labels and theme
    p <- p + labs(
      title = title,
      x = "Timepoint",
      y = if(show_y_text) y_label else "",
      caption = paste("n =", subject_count, "subjects",
                      if (show_mean) "| Dashed line = mean" else "")
    ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = if(show_y_text) element_text() else element_blank(),
        axis.title.y = if(show_y_text) element_text() else element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 9, color = "gray50"),
        legend.position = legend_position
      )
    
    return(p)
  }
  
  # Create both plots
  p_target <- create_contour_plot(
    target_coverage_data, 
    target_mean, 
    red_palette, 
    paste("Site", target_site),
    "red",
    target_site_count,
    legend_position = "left",
    show_y_text = TRUE
  )
  
  p_other <- create_contour_plot(
    other_coverage_data, 
    other_mean, 
    blue_palette, 
    "All Other Sites",
    "blue",
    other_sites_count,
    legend_position = "right",
    show_y_text = FALSE
  )
  
  # Combine plots side by side
  combined_plot <- p_target + p_other +
    plot_annotation(
      title = paste("Time Series Comparison:", param_info$parameter_name[1]),
      subtitle = "Coverage contours showing distribution at each timepoint",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, color = "gray50", hjust = 0.5)
      )
    )
  
  return(combined_plot)
}


# Function to create trellis plot of individual timeseries for a site
create_individual_timeseries_trellis  <- function(target_site, target_timeseries_id, ncol = 4) {
  
  # Get timeseries info to determine parameter and baseline
  ts_info <- timeseries %>%
    filter(timeseries_id == target_timeseries_id) %>%
    select(parameter_id, baseline, timepoint_combo) %>%
    mutate(timepoint_combo = str_split(timepoint_combo, ';'))
  
  if (nrow(ts_info) == 0) {
    stop("Timeseries ID not found")
  }
  
  # Get parameter name for title
  param_info <- parameters %>%
    filter(parameter_id == ts_info$parameter_id[1]) %>%
    select(parameter_name)
  
  # Filter and prepare the data
  plot_data <- data_with_anomalies %>%
    filter(parameter_id == ts_info$parameter_id[1]
           & timepoint_rank %in% ts_info$timepoint_combo[[1]]) %>%
    left_join(subjects, by = "subject_id") %>%
    filter(site == target_site) %>%
    rowwise() %>%
    mutate(
      # Use appropriate measurement based on baseline setting
      measurement = ifelse(ts_info$baseline[1] == "original", result, result - baseline)
    ) %>%
    filter(!is.na(measurement)) %>%
    arrange(subject_id, timepoint_rank)
  
  
  
  
  # Create the trellis plot
  p <- ggplot(plot_data, aes(x = timepoint_rank, y = measurement)) +
    geom_line(color = "steelblue", size = 0.8) +
    geom_point(color = "darkred", size = 2) +
    geom_text(
      aes(label = round(measurement, 1)), 
      vjust = -0.5,  # Position above the point
      size = 3,
      color = "black"
    ) +
    facet_wrap(~ subject_id, scales = "free_y", ncol = ncol) +
    scale_x_continuous(
      breaks = unique(plot_data$timepoint_rank),
      labels = function(x) {
        # Get timepoint names for the breaks
        timepoint_names <- plot_data %>%
          distinct(timepoint_rank, timepoint_1_name) %>%
          arrange(timepoint_rank)
        timepoint_names$timepoint_1_name[match(x, timepoint_names$timepoint_rank)]
      }
    ) +
    labs(
      title = paste("Individual Time Series -", param_info$parameter_name[1], "- Site", target_site),
      subtitle = paste("Timeseries ID:", target_timeseries_id, "| Each panel shows one subject"),
      x = "Timepoint",
      y = ifelse(ts_info$baseline[1] == "original", "Measurement", "Baseline-Adjusted Measurement")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}


#' Create PCA plot showing subject similarity
#' @param pca_coordinates PCA coordinates data frame
#' @param subjects Data frame with subject information
#' @param timeseries_id Timeseries ID to plot
#' @param highlight_sites Vector of site names to highlight with different colors
#' @return ggplot object
create_pca_plot <- function(pca_coordinates, subjects, timeseries_id, 
                            highlight_sites = NULL) {
  
  plot_data <- pca_coordinates %>%
    filter(timeseries_id == !!timeseries_id) %>%
    left_join(subjects, by = "subject_id")
  
  if(nrow(plot_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No PCA data available") +
             theme_void() +
             labs(title = paste("No PCA data for", timeseries_id)))
  }
  
  # Create color mapping with highly distinguishable colors
  if(!is.null(highlight_sites) && length(highlight_sites) > 0) {
    # Filter highlight_sites to only those present in the data
    available_highlight_sites <- intersect(highlight_sites, unique(plot_data$site))
    
    if(length(available_highlight_sites) > 0) {
      # Define easily distinguishable colors
      distinct_colors <- c(
        "#E31A1C", # Red
        "#1F78B4", # Blue  
        "#33A02C", # Green
        "#FF7F00", # Orange
        "#6A3D9A", # Purple
        "#A6761D", # Brown
        "#FB9A99", # Pink
        "#666666", # Gray
        "#B2DF8A", # Light Green
        "#FDBF6F", # Light Orange
        "#CAB2D6", # Light Purple
        "#FFFF99"  # Light Yellow
      )
      
      # Use only as many colors as needed
      highlight_colors <- distinct_colors[1:length(available_highlight_sites)]
      names(highlight_colors) <- available_highlight_sites
      
      # Create site category for coloring and plotting order
      plot_data <- plot_data %>%
        mutate(
          site_category = case_when(
            site %in% available_highlight_sites ~ site,
            TRUE ~ "Other Sites"
          ),
          # Create plotting order: Other Sites first (will be plotted in background)
          plot_order = ifelse(site_category == "Other Sites", 1, 2)
        ) %>%
        arrange(plot_order)  # Other sites first, then highlighted sites
      
      # Combine colors
      all_colors <- c(highlight_colors, "Other Sites" = "lightblue")
      
      # Set factor levels to control legend order (highlighted sites first, then "Other Sites")
      plot_data$site_category <- factor(
        plot_data$site_category, 
        levels = c(available_highlight_sites, "Other Sites")
      )
      
      # Split data for layered plotting
      other_sites_data <- plot_data %>% filter(site_category == "Other Sites")
      highlighted_sites_data <- plot_data %>% filter(site_category != "Other Sites")
      
      # Create the plot with layered points
      p <- ggplot() +
        # Plot other sites first (background)
        geom_point(data = other_sites_data,
                   aes(x = pc1, y = pc2, color = site_category),
                   size = 2.5, alpha = 0.6) +
        # Plot highlighted sites on top (foreground)
        geom_point(data = highlighted_sites_data,
                   aes(x = pc1, y = pc2, color = site_category),
                   size = 3.5, alpha = 0.9) +  # Larger and more opaque
        scale_color_manual(
          values = all_colors,
          name = "Study Sites"
        )
      
    } else {
      # No highlighted sites found in data
      plot_data <- plot_data %>%
        mutate(site_category = "All Sites")
      all_colors <- c("All Sites" = "lightblue")
      
      p <- ggplot(plot_data, aes(x = pc1, y = pc2, color = site_category)) +
        geom_point(size = 3, alpha = 0.8)
    }
    
  } else {
    # No highlighting requested
    plot_data <- plot_data %>%
      mutate(site_category = "All Sites")
    all_colors <- c("All Sites" = "lightblue")
    
    p <- ggplot(plot_data, aes(x = pc1, y = pc2, color = site_category)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(
        values = all_colors,
        name = "Study Sites"
      )
  }
  
  # Add labels and theme
  p <- p +
    labs(
      title = paste("Subject Similarity (PCA):", timeseries_id),
      subtitle = "Principal components of time series features",
      x = "First Principal Component",
      y = "Second Principal Component"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))  # Larger legend symbols
  
  return(p)
}

wrangle_input_data <- function() {
  
  # Visits to consider for each domain
  lb_visit_nums <- c(1, 4, 5, 7, 8, 9, 10, 11, 12, 13)
  eg_visit_nums <- c(1, 2, 3, 3.5, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
  vs_visit_nums <- c(1, 2, 3, 3.5, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
  
  # Get relevant data domains
  
  # Labs
  lb <- pharmaversesdtm::lb %>%
    select(USUBJID, LBTESTCD, LBTEST, LBCAT, LBSTRESN, LBBLFL, VISITNUM, VISIT) %>%
    mutate(parameter_category_1 = "LB") %>%
    rename(subject_id = USUBJID, parameter_id = LBTESTCD, parameter_category_2 = LBCAT,
           parameter_name = LBTEST, result = LBSTRESN, baseline_flag = LBBLFL, timepoint_1_name = VISIT) %>%
    filter(VISITNUM %in% lb_visit_nums)
  
  # Vital signs
  vs <- pharmaversesdtm::vs %>%
    # Consider measurements made standing/lying down as separate tests, not as timepoints
    mutate(VSTESTCD = if_else(!is.na(VSELTM), paste0(VSTESTCD, "_", VSELTM), VSTESTCD),
           VSTEST = if_else(!is.na(VSTPT), paste0(VSTEST, "_", VSTPT), VSTEST)
    ) %>%
    select(USUBJID, VSTESTCD, VSTEST, VSSTRESN, VSBLFL, VISITNUM, VISIT, VSTPTNUM, VSTPT) %>%
    mutate(parameter_category_1 = "VS") %>%
    rename(subject_id = USUBJID, parameter_id = VSTESTCD, parameter_name = VSTEST, 
           result = VSSTRESN, baseline_flag = VSBLFL, timepoint_1_name = VISIT) %>%
    filter(VISITNUM %in% vs_visit_nums)
  
  # ECG measurements
  eg <- pharmaversesdtm::eg %>%
    # Consider measurements made standing/lying down as separate tests, not as timepoints
    mutate(EGTESTCD = paste0(EGTESTCD, "_", EGELTM),
           EGTEST = paste0(EGTEST, ", ", EGTPT)) %>%
    select(USUBJID, EGTESTCD, EGTEST, EGSTRESN, EGBLFL, VISITNUM, VISIT) %>%
    mutate(parameter_category_1 = "EG") %>%
    rename(subject_id = USUBJID, parameter_id = EGTESTCD, parameter_name = EGTEST, 
           result = EGSTRESN, baseline_flag = EGBLFL, timepoint_1_name = VISIT) %>%
    filter(VISITNUM %in% eg_visit_nums)
  
  # Combine all data into one df
  all_data <- bind_rows(lb, vs, eg) %>%
    filter(!is.na(result))
  
  # Retrieve baselines...
  baselines <- all_data %>%
    filter(baseline_flag == "Y") %>%
    select(subject_id, parameter_category_1, parameter_category_2, parameter_id, result) %>%
    rename(baseline = result)
  
  # ...and join them back into the data df.
  all_data <- all_data %>%
    left_join(y=baselines, by=c("subject_id", "parameter_category_1", "parameter_category_2", "parameter_id"))
  
  # Calculate ranks for the timepoints
  all_data <- all_data %>%
    group_by(parameter_id) %>%
    mutate(timepoint_rank = dense_rank(VISITNUM)) %>%
    ungroup()
  
  # Get demographics and prepare the subjects df
  dm <- pharmaversesdtm::dm
  
  subjects <- dm %>%
    select(USUBJID, COUNTRY, SITEID) %>%
    rename(subject_id = USUBJID, country = COUNTRY, site = SITEID) %>%
    mutate(region = "DUMMY")
  
  # Build the parameters df
  parameters <- all_data %>%
    distinct(parameter_id, parameter_category_1, parameter_category_2, parameter_name) %>%
    mutate(parameter_category_3 = NA, time_point_count_min = NA, subject_count_min = NA,
           max_share_missing = NA, generate_change_from_baseline = NA, timeseries_features_to_calculate = NA,
           use_only_custom_timeseries = NA)
  
  # Re-format the clinical data for CTAS
  data <- all_data %>%
    select(subject_id, parameter_id, timepoint_1_name, timepoint_rank, result, baseline) %>%
    mutate(timepoint_2_name = NA)
  
  return(list(data, parameters, subjects))
  
}


create_results_summary_df <- function() {
  
  # An overview df for site scores
  results_summary_df <- site_scores %>%
    left_join(y=timeseries, by="timeseries_id") %>%
    left_join(y=select(parameters, parameter_id, parameter_category_1, parameter_name), by="parameter_id") %>%
    select(site, country, fdr_corrected_pvalue_logp, feature, timeseries_id, parameter_category_1, parameter_name, timepoint_count, baseline) %>%
    group_by(site, timeseries_id) %>%
    mutate(rank_rows_by = max(fdr_corrected_pvalue_logp)) %>%
    ungroup() %>%
    rename(Anomaly_score = fdr_corrected_pvalue_logp, Data_domain = parameter_category_1, Parameter = parameter_name,
           Timeseries_feature = feature) %>%
    pivot_wider(names_from = "Timeseries_feature", values_from = "Anomaly_score") %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    arrange(desc(rank_rows_by)) %>%
    select(-rank_rows_by)
 
  
    return(results_summary_df)
   
}





