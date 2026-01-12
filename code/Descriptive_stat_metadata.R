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

#################################
### Lakes parameters coverage ###
#################################

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

#################################
### num of files and datasets ###
#################################

# datasets_unique <- datasets_metadata %>%
#   distinct(id, .keep_all = TRUE)
# # Look at how many datasets per lakes
# dataset_lake_map <- datasets_unique %>%
#   filter(id %in% datasets_id) %>%
#   left_join(lakes %>% select(id, name), by = c("lakes_id" = "id")) %>%
#   select(dataset_id = id, lakes_id, lake_name = name) %>%
#   filter(!is.na(lake_name))
# 
# # Count datasets per lake
# Ndatasets_per_lake <- dataset_lake_map %>%
#   count(lake_name, sort = TRUE)

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

###########################################
### Indice de dispersion des parametres ###
###########################################

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

# Extract ALL rows involved in duplication (including first occurrence)
test <- parameters[parameters$parameters_id %in% parameters$parameters_id[duplicated(parameters$parameters_id)], ]

# remove duplicates of ids that are linked to license and other
parameters_2 <- parameters %>% filter(!grepl("license", name, ignore.case = TRUE))
parameters_2 <- parameters_2 %>% filter(!grepl("GNU", name, ignore.case = TRUE))
colnames(parameters_2)[1] <- "parameters_id"
Dispersion_ind <- median_distances %>%
  left_join(parameters_2[,1:2], by = "parameters_id")


# Parameters that are at least in 5 lakes 

parameters_list <- subset(Lake_parameter_coverage,n_lakes > 5)

#make graph

parameters_list %>%
  # slice_max(n_lakes, n = 50) %>%
  ggplot(aes(x = reorder(parameter, n_lakes), y = n_lakes)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Parameter",
    y = "Number of lakes sampled",
    title = "Number of parameters across lakes"
  )

#####################################################################################
### make dictionary of parameters ids and the different parameters under this ids ###
#####################################################################################

# get the name of the parse parameters per parameter

parseparam_by_id <- lake_parameters_summary %>%
  group_by(parameters_id) %>%
  summarise(
    parseparameters = sort(unique(parseparameter)),
    .groups = "drop"
  )

parseparam_by_id <- lake_parameters_summary %>%
  group_by(parameters_id) %>%
  summarise(
    parseparameters = paste(sort(unique(parseparameter)), collapse = ", "),
    n_parseparameters = n_distinct(parseparameter),
    .groups = "drop"
  )

#' make list with description of the parameters

Potential_parameters <- left_join(parameters_list,parseparam_by_id, by = "parameters_id")
Potential_parameters <- left_join(Potential_parameters,parameters_2[,c(1,4)], by = "parameters_id")
Potential_parameters <- Potential_parameters[,-5]
Potential_parameters <- subset(Potential_parameters, parameters_id != 145) # duplicate

# plot the num of lakes, parse parameters and the index of dispersion

Potential_parameters %>%
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

Potential_parameters %>%
  ggplot(aes(x = reorder(name, n_lakes), y = n_lakes)) +
  geom_col(fill = "darkblue", alpha = 0.8) +
  coord_flip() +
  labs(
    x = "Parameter",
    y = "num of lakes",
    title = "Lakes' presence of Parameters"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8)
  )

Potential_parameters %>%
  ggplot(aes(x = reorder(name, n_parseparameters), y = n_parseparameters)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  coord_flip() +
  labs(
    x = "Parameter",
    y = "nombre d'appelations",
    title = "nombre d'appelations"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8)
  )

# Heatmap avec valeurs normalisées par colonne

Potential_parameters_normalized <- Potential_parameters %>%
  mutate(
    # Normaliser entre 0 et 1 pour chaque critère indépendamment
    norm_distance = (median_distance - min(median_distance)) / 
      (max(median_distance) - min(median_distance)),
    norm_lakes = (n_lakes - min(n_lakes)) / 
      (max(n_lakes) - min(n_lakes)),
    norm_appelations = 1 - ((n_parseparameters - min(n_parseparameters)) / 
                              (max(n_parseparameters) - min(n_parseparameters))),
    score_moyen = (norm_distance + norm_lakes + norm_appelations) / 3
  ) %>%
  arrange(desc(score_moyen))

# Créer une version long format avec valeurs réelles et normalisées
heatmap_data <- Potential_parameters_normalized %>%
  select(name, median_distance, n_lakes, n_parseparameters, 
         norm_distance, norm_lakes, norm_appelations) %>%
  tidyr::pivot_longer(cols = c(median_distance, n_lakes, n_parseparameters), 
                      names_to = "critere", 
                      values_to = "valeur_reelle") %>%
  # Ajouter les valeurs normalisées
  mutate(
    valeur_norm = case_when(
      critere == "median_distance" ~ Potential_parameters_normalized$norm_distance[match(name, Potential_parameters_normalized$name)],
      critere == "n_lakes" ~ Potential_parameters_normalized$norm_lakes[match(name, Potential_parameters_normalized$name)],
      critere == "n_parseparameters" ~ Potential_parameters_normalized$norm_appelations[match(name, Potential_parameters_normalized$name)]
    ),
    critere = recode(critere,
                     "median_distance" = "Dispersion\n(médiane)",
                     "n_lakes" = "Nombre\nde lacs",
                     "n_parseparameters" = "Nombre\nd'appelations"
    )
  )

# Ordonner selon le score moyen
order_params <- Potential_parameters_normalized %>%
  arrange(desc(score_moyen)) %>%
  pull(name)

heatmap_data %>%
  mutate(name = factor(name, levels = order_params)) %>%
  ggplot(aes(x = critere, y = name, fill = valeur_norm)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(valeur_reelle, 0)), color = "black", size = 2) +
  scale_fill_gradient2(
    low = "#d73027", 
    mid = "#ffffbf", 
    high = "#1a9850",
    midpoint = 0.5,
    name = "Score\nnormalisé",
    limits = c(0, 1)
  ) +
  labs(
    x = "",
    y = "Paramètre",
    title = "Tous les paramètres (n=47) - Valeurs réelles avec couleurs normalisées",
    subtitle = "Couleurs basées sur normalisation 0-1 par colonne. Valeurs = chiffres réels"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right"
  )

### SAVE IMPORTANT FILE TO SEND
# load("results/lake_parameters_metadata.RData")
# load("results/Parameters_metadata_per_lake_table.Rdata")
# Parameters_metadata_per_lake_table <- pa_final
# save(Parameters_metadata_per_lake_table, file = "results/Parameters_metadata_per_lake_table.Rdata")

library(openxlsx)

# for writing a data.frame or list of data.frames to an xlsx file
write.xlsx(Potential_parameters, 'results/list_of_potential_parameters.xlsx')
