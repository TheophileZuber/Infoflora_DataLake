# Loading packages
library(tidyverse)
library(ggplot2)

#load metadata
load("data/Metadata.Rdata")

load("data/presence_absence.RData")
lake_presence_absence_parameters <- parameter_presence_absence
rm(presence_absence,parameterID_presence_absence,parameter_presence_absence)

load("data/Unk_datasets_presence_absence.RData")
load("results/lake_parameters_metadata.RData")
### FOR EACH PARAMETERS, WHICH ARE THE MOST PRESENT ACCROSS LAKES

Lake_parameter_coverage <- lake_presence_absence_parameters %>%
  summarise(across(-lake, sum)) %>%     # sum presence across lakes
  pivot_longer(
    everything(),
    names_to = "parameter",
    values_to = "n_lakes"
  ) %>%
  arrange(desc(n_lakes))%>%
  filter(parameter != "Time")

n_lakes_total <- n_distinct(lake_presence_absence_parameters$lake)

parameter_coverage <- Lake_parameter_coverage %>%
  mutate(
    pct_lakes = 100 * n_lakes / n_lakes_total
  )

parameter_coverage %>%
  slice_max(n_lakes, n = 50) %>%
  ggplot(aes(x = reorder(parameter, n_lakes), y = n_lakes)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Parameter",
    y = "Number of lakes sampled",
    title = "Number of parameters across lakes"
  )

### num of files and datasets

datasets_unique <- datasets_metadata %>%
  distinct(id, .keep_all = TRUE)
# Look at how many datasets per lakes
dataset_lake_map <- datasets_unique %>%
  filter(id %in% datasets_id) %>%
  left_join(lakes %>% select(id, name), by = c("lakes_id" = "id")) %>%
  select(dataset_id = id, lakes_id, lake_name = name) %>%
  filter(!is.na(lake_name))

# Count datasets per lake
Ndatasets_per_lake <- dataset_lake_map %>% 
  count(lake_name, sort = TRUE)

# # Look at how many files per lakes
# files_lake_map <- files_metadata %>%
#   filter(id %in% datasets_id) %>%
#   left_join(lakes %>% select(id, name), by = c("lakes_id" = "id")) %>%
#   select(dataset_id = id, lakes_id, lake_name = name) %>%
#   filter(!is.na(lake_name))
# 
# # Count files per lake
# Nfiles_per_lake <- files_lake_map %>% 
#   count(lake_name, sort = TRUE)

### Indice de dispersion des parametres
library(sf)

# Get coordonnee for datasets and change into right crs
# unique(datasets_metadata$latitude) # -9999 bizarre
# unique(datasets_metadata$longitude) # -9999 bizarre
# unique(datasets_metadata$id)

coordonnees_sf <- datasets_metadata[, c(9, 11, 14)] %>%
  rename(datasets_id = 1, latitude = 2, longitude = 3) %>%
  filter(latitude != -9999) %>%
  distinct() %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2056)

# Get coordonnee per parameters
coordonnees_parameters <- left_join(pa_summary_fixed, coordonnees_sf, by = "datasets_id")
coordonnees_parameters <- coordonnees_parameters[,c(4,15)] 
# %>% distinct(parameters_id, geometry, .keep_all = TRUE)

# calculate the centroid for each parameter

centroids <- coordonnees_parameters %>%
  group_by(parameters_id) %>%
  summarise(centroid = st_centroid(st_union(geometry)))

coord_with_centroid <- coordonnees_parameters %>%
  left_join(centroids, by = "parameters_id")

# The distance of each point to the centroid
coord_with_centroid <- coord_with_centroid %>%
  mutate(dist_to_centroid = st_distance(geometry, centroid, by_element = TRUE))

coord_with_centroid <- na.omit(coord_with_centroid)

# The median for each parameters
median_distances <- coord_with_centroid %>%
  group_by(parameters_id) %>%
  summarise(
    median_distance = median(as.numeric(dist_to_centroid)),
    n_points = n()
  )

# Select rows where at least one of the 4 columns (cfnames,description,characteristic,unit) is NOT NA
parameters <- selectiontables_metadata %>%
  filter(if_any(c(3:6), ~!is.na(.)))

# add parameters'name

# Extract ALL rows involved in duplication (including first occurrence)
test <- parameters[parameters$parameters_id %in% parameters$parameters_id[duplicated(parameters$parameters_id)], ]

# remove duplicates of ids that are linked to license and other
parameters_2 <- parameters %>% filter(!grepl("license", name, ignore.case = TRUE))
parameters_2 <- parameters_2 %>% filter(!grepl("GNU", name, ignore.case = TRUE))
colnames(parameters_2)[1] <- "parameters_id"
Dispersion_ind <- median_distances %>%
  left_join(parameters_2[,1:2], by = "parameters_id")

### Diverging bar charts

# Prepare the data - normalize/scale the median_distance for better visualization
Dispersion_ind_plot <- Dispersion_ind %>%
  mutate(
    # Scale median_distance to make it comparable with n_points
    median_distance_scaled = scale(median_distance)[,1],
    # Make one side negative for diverging effect
    n_points_neg = -n_points,
    # Reorder names by one of the variables for better readability
    name = reorder(name, median_distance)
  )

# Prepare the data
Dispersion_ind_plot <- Dispersion_ind %>%
  # Join with parameter_coverage to get n_lakes
  left_join(parameter_coverage, by = c("name" = "parameter")) %>%
  # Scale median_distance for better visualization
  mutate(
    median_distance_scaled = scale(median_distance)[,1],
    # Make n_lakes negative for left side
    n_lakes_neg = -n_lakes,
    # Reorder by n_lakes or median_distance
    name = reorder(name, n_lakes)
  ) %>%
  # Optional: take top 30 parameters
  slice_max(n_lakes, n = 30)

# Create diverging bar chart
ggplot(Dispersion_ind_plot, aes(y = name)) +
  # Left side - number of lakes (negative)
  geom_col(aes(x = n_lakes_neg), fill = "#2166ac", alpha = 0.8) +
  # Right side - median distance (scaled)
  geom_col(aes(x = median_distance_scaled), fill = "#b2182b", alpha = 0.8) +
  # Add vertical line at zero
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  # Labels and theme
  labs(
    x = "← Number of Lakes | Dispersion Index (scaled) →",
    y = "Parameter",
    title = "Parameter Coverage vs Spatial Dispersion"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8)
  )
##############
library(ggplot2)
library(dplyr)

# Create bar plot for dispersion only
Dispersion_ind_plot %>%
  slice_max(median_distance, n = 30) %>%  # Optional: top 50
  ggplot(aes(x = reorder(name, median_distance), y = median_distance)) +
  geom_col(fill = "#b2182b", alpha = 0.8) +
  coord_flip() +
  labs(
    x = "Parameter",
    y = "Median Distance (Dispersion Index)",
    title = "Spatial Dispersion of Parameters"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8)
  )
### SAVE IMPORTANT FILE TO SEND
# load("results/lake_parameters_metadata.RData")
# load("results/Parameters_metadata_per_lake_table.Rdata")
Parameters_metadata_per_lake_table <- pa_final
save(Parameters_metadata_per_lake_table, file = "results/Parameters_metadata_per_lake_table.Rdata")
