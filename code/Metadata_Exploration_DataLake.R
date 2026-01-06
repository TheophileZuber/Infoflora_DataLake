# Loading packages
library(tidyverse)
library(httr2)
library(jsonlite)


### Define base URL
Base_URL <- "https://api.datalakes-eawag.ch"

### Exploration metadata for the categories: datasets in Datalakes, 
#' files metadata for given dataset,
#' Datalakes look up tables

Endpoints <- c("/datasets","/files/","/selectiontables")

for (i in Endpoints) {
  
  #Define request
  req_metadata <- request(paste0(Base_URL,i))
  
  # GET request and parse
  response <- req_metadata %>% 
    req_perform() %>%
    resp_body_json()
  
  # Create variable name from endpoint (remove slashes)
  var_name <- str_remove_all(i, "/")
  var_name <- paste0(var_name,"_metadata")
  
  # Convert to dataframe
  df <- bind_rows(response)
  
  # Assign to global environment
  assign(var_name, df, envir = .GlobalEnv)
  
  #remove unecessary variables
  rm(df, var_name, req_metadata, response,i)
}

### Access to metadata on the parameters for each dataset and creat summary table

##' "id" in datasets_metadata seems to correspond to "datasets_id" in files_metadata.
##' First get shared datasets'id then look at lake_id in datasets_metadata 
##' to have the lake name in selectiontables_metadata. 
##' Then extract parameters for each lake and make a table. (with parameters id, name, units, link?)
##' No time, frequency of sampling, etc in metadata => in datasets?


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
SUB_ids_only_in_files <- files_metadata %>% filter(datasets_id == ids_only_in_files)
setdiff(unique(SUB_ids_only_in_files$datasets_id),ids_only_in_files)
#' datasets without metadata, lots of JSON files URL to download from web not through API

### Find IDs in datasets_metadata but NOT in files_metadata
ids_only_in_datasets <- setdiff(dataset_ids, file_ids)
ids_only_in_datasets
# [1] 1

#subset to look at this specific ID's row
SUB_ids_only_in_datasets <- datasets_metadata %>% filter(id == 1) #' Information on morphology of several lakes
 
# Take only the IDs present in both files and datasets
datasets_id <- unique(files_metadata$datasets_id)
datasets_id <- datasets_id[!(datasets_id %in% ids_only_in_files)]

### subset of lakes and parameters

lakes <- selectiontables_metadata[!is.na(selectiontables_metadata$elevation),]
#add back Rhone and Rhein with NA elevation 
lakes <- rbind(lakes,selectiontables_metadata[selectiontables_metadata$name %in% c("Rhone","Rhein"),])

# Select rows where at least one of the 4 columns (cfnames,description,characteristic,unit) is NOT NA
parameters <- selectiontables_metadata %>%
filter(if_any(c(3:6), ~!is.na(.)))

### Create table with lake names and parameters metadata

# Look at how many datasets per lakes
dataset_lake_map <- datasets_metadata %>%
  filter(id %in% datasets_id) %>%
  left_join(lakes %>% select(id, name), by = c("lakes_id" = "id")) %>%
  select(dataset_id = id, lakes_id, lake_name = name) %>%
  filter(!is.na(lake_name))

# Count datasets per lake
Ndata_per_lake <- dataset_lake_map %>% 
  count(lake_name, sort = TRUE)
# ONLY 44 LAKE WITH DATA?


# Create empty list to store results
data_list <- list()

for (n in datasets_id) {
  #get lake id
  rows <- datasets_metadata[datasets_metadata$id == n,]
  lake_id <- unique(rows$lakes_id)
  
  #get lake name
  rows <- lakes[lakes$id == lake_id,]
  
  lake_name <- rows$name
  
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
  
  #store the dataframe to the right lake
  
  if (lake_name %in% names(data_list)) {
    data_list[[lake_name]] <- bind_rows(data_list[[lake_name]], response)
  } else {
    data_list[[lake_name]] <- response
  }
}
#rm unecessary variables
rm(rows,response,req_metadata,endpoints_URL,lake_name,lake_id)

# Combine all parameters by lake
lake_parameters_summary <- map_df(names(data_list), function(lake_name) {
  data_list[[lake_name]] %>%
    mutate(lake = lake_name) %>%
    select(lake, everything())
})



### presence/absence table

# #check na
# lake_parameters_summary[is.na(lake_parameters_summary$parseparameter),] no na
# lake_parameters_summary[is.na(lake_parameters_summary$parameters_id),] no na

#presence absence with combination of parameters'shortname and there parameter's id
presence_absence <- lake_parameters_summary %>%
  distinct(lake, parseparameter, parameters_id) %>%  # unique combinations
  mutate(present = 1) %>%
  unite(var, parseparameter, parameters_id, sep = "_") %>%  # combine into one column name
  pivot_wider(
    names_from  = var,
    values_from = present,
    values_fill = 0
  )

# presence absence with only parameter's id and with parameter's id replaced by their name

parameterID_presence_absence <- lake_parameters_summary %>%
  distinct(lake, parameters_id) %>%  # unique combinations
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
parameter_presence_absence <- parameterID_presence_absence %>%
  rename_with(
    ~name_mapping[.x],
    .cols = matches("^\\d+$")  # Only columns that are purely numeric
  )

setwd("~/MasterBec/InfoFlora_stage/Infoflora_DataLake")
save(lake_parameters_summary,presence_absence,parameterID_presence_absence,parameter_presence_absence, file = "data/presence_absence.RData")
# load("data/presence_absence.RData")

#' Get the start and end date of sampling as well as estimating the frequency 
#' via files metadata then look at doing that more accuretly by going through files?
#' Then try to do the same table for files with no link to lake id
