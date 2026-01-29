library(tidyverse)
library(sf)
library(arrow)
library(terra)

#############################
# Load and harmonise data 
# (Only work for list_of_potential_parameters.Rdata which do not have all parameters)
#############################

# load parameters data
final_data <- read_parquet('results/merged_parameters_data.parquet')

# load species observation data
sp_obs <- readRDS("data/sp_obs_260112.RDS")

# # load metadata
load("data/Metadata.Rdata")
load('results/list_of_potential_parameters.Rdata')

# Load swiss lake fills

# Swiss_Lakes <- st_read(
#   "data/Swiss_Lakes/Swiss_Lakes.shp")
# 

Swiss_Lakes_bis <- st_read(
  "data/datalake_geom/datalake_geom.shp")

#### Harmonisation of the units in final_data
library(dplyr)
library(stringr)

final_data_harmonised <- final_data %>%
# Remove depth-specific parameters (_15m, _1.5m, etc.)
filter(!str_detect(parseparameter, "_\\d+\\.?\\d*m$")) %>%
# Harmonise pH units
mutate(
  unit = if_else(
    str_detect(tolower(parseparameter), "^ph"),
    NA_character_,
    unit
  )
) %>%
# Harmonise percentage units
mutate(
  unit = if_else(
    unit %in% c("%", "%sat", "dosat", "sat", "OPTO%"),
    "%",
    unit
  )
) %>%
# Chlorophyll A handling
mutate(
  # standardize acetone label
  unit = if_else(unit == "ug/L chl-a in acetone", "ug/L", unit))


# identify chlorophyll rows
chl_mask <- str_detect(final_data_harmonised$parseparameter,
                       regex("chl|chlorophyll", ignore_case = TRUE))

# dominant comparable unit (ug/L or mg m-3 only)
chl_dominant_unit <- final_data_harmonised %>%
  filter(
    chl_mask,
    unit %in% c("ug/L", "mg m-3")
  ) %>%
  count(unit) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(unit)

final_data_harmonised <- final_data_harmonised %>%
  mutate(
    # numerical conversion ONLY when physically identical
    value = case_when(
      chl_mask & unit == "ug/L"   & chl_dominant_unit == "mg m-3" ~ value / 1000,
      chl_mask & unit == "mg m-3" & chl_dominant_unit == "ug/L"   ~ value * 1000,
      TRUE ~ value
    ),
    unit = case_when(
      chl_mask & unit %in% c("ug/L", "mg m-3") ~ chl_dominant_unit,
      TRUE ~ unit
    ),
    parameter_name = case_when(
      chl_mask & unit == "RFU" ~ "Chlorophyll A RFU",
      chl_mask & unit %in% c("ug/L", "mg m-3") ~ "Chlorophyll A",
      TRUE ~ parameter_name
    )
  )


#Turbidity

final_data_harmonised <- final_data_harmonised %>%
  mutate(
    parameter_name = case_when(
      str_detect(parameter_name, regex("turb", ignore_case = TRUE)) & unit == "NTU" ~
        "Turbidity NTU",
      str_detect(parameter_name, regex("turb", ignore_case = TRUE)) & unit == "FTU" ~
        "Turbidity FTU",
      TRUE ~ parameter_name
    )
  ) %>%
  
  distinct()


chl_na <- final_data_harmonised %>%
  filter(
    str_detect(parseparameter, regex("chlorophyll|chl", ignore_case = TRUE)),
    is.na(unit)
  )
nrow(chl_na)
summary(chl_na$value)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -999    -999    -999    -999    -999    -999 

final_data_harmonised <- final_data_harmonised %>%
  filter(!(parameter_name == "Chlorophyll_A" & is.na(unit)))


# Filter out sentinel values
final_data_harmonised <- final_data_harmonised %>%
  filter(
    # Remove common sentinel values for missing data
    !(value %in% c(-999, -1000, -9999, -99999)),
    # Or if sentinels are parameter-specific:
    !(str_detect(parameter_name, "Turbidity|Oxygen_Saturation") & value < 0),
    !(str_detect(parameter_name, "Chlorophyll A|Oxygen_Saturation") & value > 100),
    !(str_detect(parameter_name, "pH") & 0 > value),
    !(str_detect(parameter_name, "pH") & 14 < value),
    !(str_detect(parameter_name, "Nitrate") & 10 < value)
  )

final_data_harmonised <- final_data_harmonised %>%
  mutate(
    parameter_name = case_when(
      str_detect(parseparameter, "_DepthAveraged$") &
        parameter_name != parseparameter ~ parseparameter,
      TRUE ~ parameter_name
    )
  )

final_data_harmonised <- final_data_harmonised %>%
  mutate(
    lon_round = round(longitude, 5),
    lat_round = round(latitude, 5)
  ) %>%
  unite(
    station_id,
    lon_round, lat_round,
    sep = "_",
    remove = FALSE
  )

# ======================================================
# Function: assign species observations to nearest lake
# ======================================================

assign_sp_to_lakes <- function(sp_obs, sp_id, shape_file, doubt_threshold) {
  
  sf_sp <- sp_obs %>%
    
    # keep only species
    filter(
      v_xy_radius < 1001,
      v_presence_status == 2,
      v_accepted_taxon_id == sp_id
    ) %>%
    
    # ✅ keep highest doubt_status per observation
    group_by(
      v_accepted_taxon_id,
      date,
      x,
      y
    )%>% slice_max(v_doubt_status, with_ties = FALSE) %>%
    ungroup() %>%
    
    # apply threshold AFTER deduplication
    filter(v_doubt_status > doubt_threshold) %>%
    
    mutate(date = as.Date(substr(date, 1, 10))) %>%
    st_as_sf(coords = c("x", "y"), crs = 2056)
  
  
  if (nrow(sf_sp) == 0) {
    sf_sp$lake_geom_id <- character(0)
    sf_sp$dist_to_lake <- numeric(0)
    sf_sp$year <- integer(0)
    sf_sp$species_id <- sp_id
    return(sf_sp)
  }
  
  nearest_idx <- st_nearest_feature(sf_sp, shape_file)
  nearest_lakes <- shape_file[nearest_idx, ]
  
  dist_to_lake <- st_distance(sf_sp, nearest_lakes, by_element = TRUE)
  
  sf_sp %>%
    mutate(
      lake_geom_id = nearest_lakes$lake_geom_id,
      dist_to_lake = as.numeric(dist_to_lake),
      year = lubridate::year(date),
      species_id = sp_id
    )
}


# ======================================================
# Main function for single species
# ======================================================

format_sp_data_for_pca <- function(sp_obs, final_data, sp_id, shape_file) {
  
  colnames(datasets_metadata)[colnames(datasets_metadata) == "id"] <- "dataset_id"
  colnames(shape_file)[colnames(shape_file) == "UUID"] <- "lake_geom_id"
  
  # --------------------------------------------------
  # 2. validation comparison (>1 vs >0)
  # --------------------------------------------------
  
  sp_gt1 <- assign_sp_to_lakes(sp_obs, sp_id, shape_file, 1)
  sp_gt0 <- assign_sp_to_lakes(sp_obs, sp_id, shape_file, 0)
  
  if (nrow(sp_gt1) == 0) {
    message(sprintf("  ↳ species %s: no observations with doubt_status > 1", sp_id))
  }
  
  if (nrow(sp_gt0) == 0) {
    message(sprintf("  ↳ species %s: no observations with doubt_status > 0", sp_id))
  }
  
  comparison_summary <- tibble(
    species_id = sp_id,
    doubt_status = c("> 1", "> 0"),
    n_observations = c(nrow(sp_gt1), nrow(sp_gt0)),
    n_lakes = c(
      n_distinct(sp_gt1$lake_geom_id),
      n_distinct(sp_gt0$lake_geom_id)
    )
  ) %>%
    mutate(
      delta_observations = n_observations - first(n_observations),
      delta_lakes = n_lakes - first(n_lakes)
    )
  
  # --------------------------------------------------
  # 3. keep species observations ≤ 200 m from lakes
  # --------------------------------------------------
  
  # For doubt_status > 0 (sp_gt0)
  sp_assigned_gt0 <- sp_gt0 %>%
    filter(dist_to_lake <= 200)
  
  sp_inside_gt0 <- sp_assigned_gt0
  sp_outside_gt0 <- sp_assigned_gt0
  
  # For doubt_status > 1 (sp_gt1)
  sp_assigned_gt1 <- sp_gt1 %>%
    filter(dist_to_lake <= 200)
  
  sp_inside_gt1 <- sp_assigned_gt1
  sp_outside_gt1 <- sp_assigned_gt1
  
  # --------------------------------------------------
  # 4. lake × year presence table 
  # --------------------------------------------------
  all_lakes <- shape_file %>%
    st_drop_geometry() %>%
    distinct(lake_geom_id)
  
  # For sp_gt0 (doubt_status > 0)
  species_lakes_gt0 <- sp_inside_gt0 %>%
    st_drop_geometry() %>%
    distinct(lake_geom_id) %>%
    mutate(species_present = 1)
  
  species_lakes_gt0 <- all_lakes %>%
    left_join(species_lakes_gt0, by = "lake_geom_id") %>%
    mutate(species_present = replace_na(species_present, 0))
  
  # For sp_gt1 (doubt_status > 1)
  species_lakes_gt1 <- sp_inside_gt1 %>%
    st_drop_geometry() %>%
    distinct(lake_geom_id) %>%
    mutate(species_present = 1)
  
  species_lakes_gt1 <- all_lakes %>%
    left_join(species_lakes_gt1, by = "lake_geom_id") %>%
    mutate(species_present = replace_na(species_present, 0))
  
  
  # --------------------------------------------------
  # 5. assign datasets to nearest lake (UNIQUE)
  # --------------------------------------------------  
  
  # For sp_gt0
  species_lakes_complete_gt0 <- shape_file %>%
    st_drop_geometry() %>%
    left_join(
      species_lakes_gt0 %>% select(lake_geom_id, species_present),
      by = "lake_geom_id"
    ) %>%
    mutate(species_present = replace_na(species_present, 0))
  
  # For sp_gt1
  species_lakes_complete_gt1 <- shape_file %>%
    st_drop_geometry() %>%
    left_join(
      species_lakes_gt1 %>% select(lake_geom_id, species_present),
      by = "lake_geom_id"
    ) %>%
    mutate(species_present = replace_na(species_present, 0))
  
  
  # --------------------------------------------------
  # 6. attach lake id to parameter data
  # --------------------------------------------------
  
  # For sp_gt0
  final_data_lake_gt0 <- final_data %>%
    left_join(
      species_lakes_complete_gt0 %>% select(dataset_id, lake_geom_id),
      by = "dataset_id"
    ) %>%
    filter(!is.na(lake_geom_id)) %>%
    mutate(year = lubridate::year(datetime))
  
  # For sp_gt1
  final_data_lake_gt1 <- final_data %>%
    left_join(
      species_lakes_complete_gt1 %>% select(dataset_id, lake_geom_id),
      by = "dataset_id"
    ) %>%
    filter(!is.na(lake_geom_id)) %>%
    mutate(year = lubridate::year(datetime))
  
  
  # --------------------------------------------------
  # 7. yearly parameter means per lake
  # --------------------------------------------------
  
  # For sp_gt0
  params_by_lake_year_gt0 <- final_data_lake_gt0 %>%
    group_by(lake_geom_id,
             station_id,
             year,
             parameter_name) %>%
    summarise(
      mean_value = ifelse(all(is.na(value)), NA, mean(value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = parameter_name,
      values_from = mean_value
    )
  
  # For sp_gt1
  params_by_lake_year_gt1 <- final_data_lake_gt1 %>%
    group_by(lake_geom_id,
             station_id,
             year,
             parameter_name) %>%
    summarise(
      mean_value = ifelse(all(is.na(value)), NA, mean(value, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = parameter_name,
      values_from = mean_value
    )
  
  # --------------------------------------------------
  # 8. FINAL JOIN — lake × year only (FIX: remove duplicates)
  # --------------------------------------------------
  
  # ✅ KEY FIX: Select only lake_geom_id and species_present, then distinct()
  # This prevents duplicate rows when multiple dataset_ids map to the same lake
  
  # For sp_gt0
  final_result_gt0 <- params_by_lake_year_gt0 %>%
    left_join(
      species_lakes_complete_gt0 %>% 
        select(lake_geom_id, species_present) %>% 
        distinct(),
      by = "lake_geom_id"
    ) %>%
    mutate(
      doubt_threshold = "> 0",
      species_id = sp_id
    )
  
  # For sp_gt1
  final_result_gt1 <- params_by_lake_year_gt1 %>%
    left_join(
      species_lakes_complete_gt1 %>% 
        select(lake_geom_id, species_present) %>%
        distinct(),  # Ensures one row per lake_geom_id
      by = "lake_geom_id"
    ) %>%
    mutate(doubt_threshold = "> 1", species_id = sp_id)
  
  # Combine both results
  final_result <- bind_rows(final_result_gt0, final_result_gt1)
  
  
  # --------------------------------------------------
  # output
  # --------------------------------------------------
  
  return(list(
    lake_year_data = final_result,
    lake_year_data_gt0 = final_result_gt0,
    lake_year_data_gt1 = final_result_gt1,
    species_points_inside_gt0 = sp_inside_gt0,
    species_points_outside_gt0 = sp_outside_gt0,
    species_points_inside_gt1 = sp_inside_gt1,
    species_points_outside_gt1 = sp_outside_gt1,
    validation_comparison = comparison_summary
  ))
}


# ======================================================
# Wrapper function to process ALL species
# ======================================================

format_all_sp_data_for_pca <- function(sp_obs, final_data, shape_file, output_dir = "./species_results") {
  
  # Prepare column names
  colnames(datasets_metadata)[colnames(datasets_metadata) == "id"] <- "dataset_id"
  colnames(shape_file)[colnames(shape_file) == "UUID"] <- "lake_geom_id"
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Get unique species IDs from final_data
  all_species <- unique(sp_obs$v_accepted_taxon_id)
  
  cat("Processing", length(all_species), "species...\n")
  
  # Initialize containers for results
  all_results_gt0 <- list()
  all_results_gt1 <- list()
  all_validation <- list()
  
  # Process each species
  for (i in seq_along(all_species)) {
    sp_id <- all_species[i]
    
    cat(sprintf("[%d/%d] Processing species %s... ", i, length(all_species), sp_id))
    
    tryCatch({
      # Run analysis for this species
      result <- format_sp_data_for_pca(sp_obs, final_data, sp_id, shape_file)
      
      # Collect results
      all_results_gt0[[as.character(sp_id)]] <- result$lake_year_data_gt0
      all_results_gt1[[as.character(sp_id)]] <- result$lake_year_data_gt1
      all_validation[[as.character(sp_id)]] <- result$validation_comparison
      
      # Save species-specific files
      species_dir <- file.path(output_dir, paste0("species_", sp_id))
      if (!dir.exists(species_dir)) {
        dir.create(species_dir, recursive = TRUE)
      }
      
      # Save spatial data
      if (nrow(result$species_points_inside_gt0) > 0) {
        sf::st_write(
          result$species_points_inside_gt0,
          file.path(species_dir, "sp_points_inside_gt0.gpkg"),
          delete_layer = TRUE,
          quiet = TRUE
        )
      }
      
      if (nrow(result$species_points_inside_gt1) > 0) {
        sf::st_write(
          result$species_points_inside_gt1,
          file.path(species_dir, "sp_points_inside_gt1.gpkg"),
          delete_layer = TRUE,
          quiet = TRUE
        )
      }
      
      # Save validation comparison
      write.csv(
        result$validation_comparison,
        file.path(species_dir, "validation_comparison.csv"),
        row.names = FALSE
      )
      
      cat("✓\n")
      
    }, error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
    })
  }
  
  # Combine all results
  final_result_gt0 <- bind_rows(all_results_gt0)
  final_result_gt1 <- bind_rows(all_results_gt1)
  all_validations <- bind_rows(all_validation)
  
  # Save combined results
  write.csv(
    final_result_gt0,
    file.path(output_dir, "all_species_lake_year_data_gt0.csv"),
    row.names = FALSE
  )
  
  write.csv(
    final_result_gt1,
    file.path(output_dir, "all_species_lake_year_data_gt1.csv"),
    row.names = FALSE
  )
  
  write.csv(
    all_validations,
    file.path(output_dir, "all_species_validation_comparison.csv"),
    row.names = FALSE
  )
  
  cat("\n✓ All species processed!\n")
  cat("Results saved to:", output_dir, "\n")
  cat("  - all_species_lake_year_data_gt0.csv\n")
  cat("  - all_species_lake_year_data_gt1.csv\n")
  cat("  - all_species_validation_comparison.csv\n")
  cat("  - species_*/sp_points_*.gpkg (spatial data)\n")
  cat("  - species_*/validation_comparison.csv (per-species validation)\n")
  
  return(list(
    lake_year_data_gt0 = final_result_gt0,
    lake_year_data_gt1 = final_result_gt1,
    validation_comparison = all_validations,
    output_directory = output_dir
  ))
}


# ======================================================
# USAGE EXAMPLE
# ======================================================

# Run for all species
results <- format_all_sp_data_for_pca(
  sp_obs = sp_obs[, c(2, 6, 7, 8, 9, 12, 13)],
  final_data = final_data_harmonised,
  shape_file = Swiss_Lakes_bis,
  output_dir = "./species_analysis_results"
)

# Access the main results
final_result_gt0 <- results$lake_year_data_gt0
final_result_gt1 <- results$lake_year_data_gt1
validation_summary <- results$validation_comparison

# Example: Check results for a specific species
sp_id <- 1011540
final_result_gt0 %>% filter(species_id == sp_id)
final_result_gt1 %>% filter(species_id == sp_id)

library(tidyverse)


species_lookup <- sp_obs %>%
  distinct(v_accepted_taxon_id, v_taxon) %>%
  group_by(v_accepted_taxon_id) %>%
  slice(1) %>%   # safety in case of duplicates
  ungroup() %>%
  rename(
    species_id = v_accepted_taxon_id,
    species_name = v_taxon
  )

safe_species_name <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}




plot_species_parameters <- function(species_df,
                                    sp_id,
                                    sp_name,
                                    output_dir) {
  
  plot_data <- species_df %>%
    pivot_longer(
      cols = -c(
        lake_geom_id,
        station_id,
        species_present,
        species_id,
        species_name,
        year,
        doubt_threshold
      ),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    filter(!is.na(value))
  
  
  if (nrow(plot_data) == 0) {
    message("species ", sp_name, ": no data to plot")
    return(NULL)
  }
  
  if (length(unique(plot_data$species_present)) == 1) {
    message("species ", sp_name, ": only one presence class — skipping")
    return(NULL)
  }
  
  p <- ggplot(plot_data, aes(
    x = value,
    y = factor(species_present),
    color = factor(species_present)
  )) +
    geom_jitter(height = 0.15, size = 2) +
    facet_wrap(~ parameter, scales = "free_x") +
    labs(
      title = sp_name,
      subtitle = paste("Species ID:", sp_id),
      y = "Species presence",
      x = "Parameter value"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      text = element_text(size = 16),
      plot.title = element_text(face = "italic")
    )
  
  file_name <- paste0(
    safe_species_name(sp_name),
    "_parameters.png"
  )
  
  ggsave(
    filename = file.path(output_dir, file_name),
    plot = p,
    width = 14,
    height = 8,
    dpi = 300
  )
  
  return(p)
}


final_result <- bind_rows(
  results$lake_year_data_gt0,
  results$lake_year_data_gt1
)

final_result <- final_result %>%
  left_join(species_lookup, by = "species_id")




all_species <- unique(final_result$species_id)

all_plots <- list()

for (sp_id in all_species) {
  
  species_df <- final_result %>%
    filter(species_id == sp_id)
  
  sp_name <- unique(species_df$species_name)
  
  if (length(sp_name) == 0 || is.na(sp_name)) {
    sp_name <- paste("species", sp_id)
  }
  
  species_dir <- file.path(
    results$output_directory,
    paste0("species_", sp_id)
  )
  
  if (!dir.exists(species_dir)) {
    dir.create(species_dir, recursive = TRUE)
  }
  
  cat("Plotting:", sp_name, "\n")
  
  all_plots[[as.character(sp_id)]] <-
    plot_species_parameters(
      species_df = species_df,
      sp_id = sp_id,
      sp_name = sp_name,
      output_dir = species_dir
    )
}
