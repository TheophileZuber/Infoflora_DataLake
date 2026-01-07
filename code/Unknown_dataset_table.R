# Loading packages
library(tidyverse)
library(httr2)
library(jsonlite)


### Define base URL
Base_URL <- "https://api.datalakes-eawag.ch"

### load metadata
load("data/Metadata.Rdata")

# Get unique IDs from each dataset
file_ids <- unique(files_metadata$datasets_id)
dataset_ids <- unique(datasets_metadata$id)

### Find IDs in files_metadata but NOT in datasets_metadata 
ids_only_in_files <- setdiff(file_ids, dataset_ids)
ids_only_in_files
# [1]   21   22  822   19   20   24   25   NA  807 1376 1375 1236 1237 1352 1353  929 1354  930  967  920 1268  922  923
# [24] 1276  924  925  926  927  928 1240 1430 1378 1362 1429  645    9    5    8    7    6    3    4    2   10  675   15
# [47]   11   14 1261  601

#subset to look at these specific IDs'rows
SUB_ids_only_in_files <- files_metadata %>% filter(datasets_id %in% ids_only_in_files)

# keep only the ids that are not found in the datasets metadata
datasets_id <- unique(SUB_ids_only_in_files$datasets_id)
datasets_id <- na.omit(datasets_id)
# Select rows where at least one of the 4 columns (cfnames,description,characteristic,unit) is NOT NA
parameters <- selectiontables_metadata %>%
  filter(if_any(c(3:6), ~!is.na(.)))
###' Create a table for unkown datasets (no lake ids in metadata) 
###' with presence and absence of parameters

# Create empty list to store results
data_list <- list()

for (n in datasets_id) {
  
  ##Get request for parameters metadata
  
  #endpoints
  endpoints_URL <- paste0("/datasetparameters/",n)
  
  #Define request
  req_metadata <- request(paste0(Base_URL, endpoints_URL))
  
  #GET request and parse
  response <- req_metadata %>% 
    req_perform() %>%
    resp_body_json()
  
  #transform in dataframe
  response <- bind_rows(response)
  
  #store the dataframe to the right unk_datasets_id
  id <- as.character(n)
  if (id %in% names(data_list)) {
    data_list[[id]] <- bind_rows(data_list[[id]], response)
  } else {
    data_list[[id]] <- response
  }
}
#rm unecessary variables
rm(response,req_metadata)

# Combine all parameters by unk_datasets_id
Unkown_datasets_parameters_summary <- map_df(names(data_list), function(Unk_datasets) {
  data_list[[Unk_datasets]] %>%
    mutate(datasets_id = Unk_datasets) %>%
    select(datasets_id, everything())
})



### presence/absence table

#presence absence with combination of parameters'shortname and their parameter's id
presence_absence <- Unkown_datasets_parameters_summary %>%
  distinct(datasets_id, parseparameter, parameters_id) %>%  # unique combinations
  mutate(present = 1) %>%
  unite(var, parseparameter, parameters_id, sep = "_") %>%  # combine into one column name
  pivot_wider(
    names_from  = var,
    values_from = present,
    values_fill = 0
  )

## presence absence with only parameter's id and with parameter's id replaced by their name

parameterID_presence_absence <- Unkown_datasets_parameters_summary %>%
  distinct(datasets_id, parameters_id) %>%  # unique combinations
  mutate(present = 1) %>%
  unite(var, parameters_id, sep = "_") %>%  # combine into one column name
  pivot_wider(
    names_from  = var,
    values_from = present,
    values_fill = 0
  )

# Create a named vector for mapping
name_mapping <- setNames(selectiontables_metadata$name, selectiontables_metadata$id)
# Only rename numeric columns
Unk_datasets_parameter_presence_absence <- parameterID_presence_absence %>%
  rename_with(
    ~name_mapping[.x],
    .cols = matches("^\\d+$")  # Only columns that are purely numeric
  )

setwd("~/MasterBec/InfoFlora_stage/Infoflora_DataLake")
save(Unk_datasets_parameter_presence_absence, file = "data/Unk_datasets_presence_absence.RData")
#load("data/presence_absence.RData")


###' Create a table with instead of presence and absence, the metadata for each
###' files and parameters are stored in a list. Therefore we have a table with 
###' unkown datasets'ids as columns, parameters as rows and metadata in cells 
###' with the format: c(starting date of sampling, end date, estimated 
###' frequency of sampling).
###' One datasets has several files assigned that have each a starting and
###' ending date of sampling.
###' There is one dataset for each combination of lake + parameter. 
###' the frequency of sampling of a datasets is estimated as: 
###' (end date - start date) / number of files

## build the table with metadata 

# Add the timestamps
pa_long <- Unk_datasets_parameter_presence_absence %>%
  pivot_longer(
    -datasets_id,
    names_to = "parameter",
    values_to = "present"
  ) %>%
  filter(present == 1)

# add parameter_id

parameters2 <- parameters[, 1:2]
colnames(parameters2) <- c("parameters_id", "parameter")
pa_long <- pa_long %>%
  left_join(parameters2, by = "parameter")

# estimate the frequency of sampling
dataset_time_summary <- files_metadata %>%
  filter(!is.na(mindatetime), !is.na(maxdatetime)) %>%
  mutate(
    mindatetime = as.POSIXct(mindatetime, tz = "UTC"),
    maxdatetime = as.POSIXct(maxdatetime, tz = "UTC")
  ) %>%
  group_by(datasets_id) %>%
  summarise(
    start_date = min(mindatetime),
    end_date   = max(maxdatetime),
    n_files    = n_distinct(id),
    duration_days = as.numeric(difftime(end_date, start_date, units = "days")),
    .groups = "drop"
  ) %>%
  mutate(
    avg_days_per_file = duration_days / n_files,
    frequency_estimated = case_when(
      avg_days_per_file < 1/24 ~ "minute-scale",
      avg_days_per_file < 1    ~ "hourly",
      avg_days_per_file < 7    ~ "daily",
      avg_days_per_file < 30   ~ "weekly",
      avg_days_per_file < 365   ~ "monthly",
      avg_days_per_file < 730   ~ "yearly",
      TRUE                     ~ "sporadic"
    )
  )

pa_long$datasets_id <- as.numeric(pa_long$datasets_id)
dataset_time_summary$datasets_id <- as.numeric(dataset_time_summary$datasets_id)
# add it to the futur table
pa_summary_fixed <- pa_long %>%
  left_join(dataset_time_summary, by = "datasets_id") %>%
  mutate(
    has_time = !is.na(start_date)
  ) %>%
  filter(parameter != "Time")

# create the list of metadata that will be present in every cell
pa_nested <- pa_summary_fixed %>%
  mutate(
    dataset_metadata = purrr::pmap(
      list(start_date, end_date, frequency_estimated),
      ~ list(
        start = ..1,
        end   = ..2,
        frequency = ..3
      )
    )
  ) %>%
  group_by(datasets_id, parameter) %>%
  summarise(
    metadata = list(dataset_metadata),
    .groups = "drop"
  )

# put the data in wide format
pa_final <- pa_nested %>%
  pivot_wider(
    names_from = parameter,
    values_from = metadata
  )
save(pa_final, pa_nested,pa_summary_fixed,file = "results/Unkown_lakes_parameters_metadata.RData" )

