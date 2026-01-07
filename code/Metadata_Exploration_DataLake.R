# Loading packages
library(tidyverse)
library(httr2)
library(jsonlite)


### Define base URL
Base_URL <- "https://api.datalakes-eawag.ch"

### Exploration metadata for the categories: datasets in Datalakes, 
#' The data has this structure: One lake has several datasets, and one dataset
#' has several files. 

### request the metadata for files, datasets and the parameters available in files 
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
}

#remove unecessary variables
rm(df, var_name, req_metadata, response,i)

### Access to metadata of the parameters for each files and create summary table


# Get unique IDs from each dataset
#' datasets_id is the datasets'ids in files_metadata but 
#' id is the datasets'ids in datasets_metadata

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
setdiff(unique(SUB_ids_only_in_files$datasets_id),ids_only_in_files)

### Find IDs in datasets_metadata but NOT in files_metadata
ids_only_in_datasets <- setdiff(dataset_ids, file_ids)
ids_only_in_datasets
# [1] 1

#subset to look at this specific ID's row
SUB_ids_only_in_datasets <- datasets_metadata %>% filter(id == 1) #' Information on morphology of several lakes

#' In Files_metadata we have the files'ids and the dataset's id from which they are.
#' In datasets_metadata we have datasets'ids and the lake's id to which they are linked.
#' In selectiontables_metadata we have the lakes' ids and the lake's name attached to it.
#' Thus the datasets IDs that are present in Files_metadata but not in datasets_metadata
#' are unkown lakes. 


# Take only the IDs present in both files and datasets
datasets_id <- unique(files_metadata$datasets_id)
datasets_id <- datasets_id[!(datasets_id %in% ids_only_in_files)]

### Create subset of lakes and parameters from selectiontables_metadata for later use

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
# Only 44 lakes, which correspond to the number of choice of downloadable data on the website


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
#load("data/presence_absence.RData")

###' Create a table with instead of presence and absence, where the metadata for each
###' files and parameters are stored in a list. Therefore we have a table with 
###' lakes as columns, parameters as rows and metadata in cells.
###' with the format: c(dataset's id, starting date of sampling, end date, estimated 
###' frequency of sampling).
###' One datasets has several files assigned that have each a starting and
###' ending date of sampling.
###' There is one dataset for each combination of lake + parameter. 
###' the frequency of sampling of a datasets is estimated as: 
###' (end date - start date) / number of files

## build the table with metadata 

### Add the timestamps

pa_long <- parameter_presence_absence %>%
  pivot_longer(
    -lake,
    names_to = "parameter",
    values_to = "present"
  ) %>%
  filter(present == 1)

# add parameter_id

colnames(parameters)[2] <- "parameter"
pa_long <- pa_long %>%
  left_join(parameters[,1:2], by = "parameter")
colnames(pa_long)[4] <- "parameters_id"

# add datasets id 

pa_long <- pa_long %>%
  left_join(
    dataset_lake_map %>% select(dataset_id, lake_name),
    by = c("lake" = "lake_name")
  ) %>%
  rename(datasets_id = dataset_id)

# estimate the frequency of sampling

dataset_time_summary <- files_metadata %>%
  filter(!is.na(mindatetime), !is.na(maxdatetime)) %>%
  mutate(
    mindatetime = as.POSIXct(mindatetime,tz = "UTC"),
    maxdatetime = as.POSIXct(maxdatetime,tz = "UTC")
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
    ),
start_date_fmt = format(start_date, "%Y-%m-%d"),
end_date_fmt   = format(end_date,   "%Y-%m-%d")
)
# add it to the futur table

pa_summary_fixed <- pa_long %>%
  left_join(dataset_time_summary, by = "datasets_id") %>%
  mutate(
    has_time = !is.na(start_date)
  ) %>%
  filter(parameter != "Time")

# create the list of metadata that will be present in every cell

pa_with_metadata <- pa_summary_fixed %>%
  mutate(
    dataset_metadata = purrr::pmap(
      list(datasets_id, start_date_fmt, end_date_fmt, frequency_estimated),
      ~ list(
        dataset_id = ..1,
        start = ..2,
        end = ..3,
        frequency = ..4
      )
    )
  )


pa_nested <- pa_with_metadata %>%
  group_by(lake, parameter) %>%
  summarise(
    metadata = list(dataset_metadata),
    .groups = "drop"
  )


pa_final <- pa_nested %>%
  pivot_wider(
    names_from = parameter,
    values_from = metadata
  )

save(pa_final, pa_nested,pa_summary_fixed,file = "results/lake_parameters_metadata.RData" )
