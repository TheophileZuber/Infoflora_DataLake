# Loading packages
library(tidyverse)
library(httr2)
library(jsonlite)


#load metadata
load("data/Metadata.Rdata")
load('results/list_of_potential_parameters.Rdata')

# load("data/presence_absence.RData")
# lake_presence_absence_parameters <- parameter_presence_absence
# rm(presence_absence,parameterID_presence_absence,parameter_presence_absence)
# load("data/Unk_datasets_presence_absence.RData")
# load("results/lake_parameters_metadata.RData")

### Define base URL
Base_URL <- "https://api.datalakes-eawag.ch/download/csv/"

# define the parameter to downlaod
par_id_to_dl <- 

# get the datasets where the parameter is
datasets_id_of_par <- lake_parameters_summary %>% 
  filter(parameters_id %in% par_id_to_dl) %>%
  unique(datasets_id)

# get the files in the datasets where the parameter is
Endpoints <- files_metadata %>% 
  filter(datasets_id_of_par %in% datasets_id) %>%
  unique(id)

for (i in Endpoints) {
  
  #Define request
  req_file <- request(paste0(Base_URL,i))
  
  # GET request and parse
  response <- req_file %>% 
    req_perform() %>%
    resp_body_json()
  
  # Convert to dataframe
  df <- bind_rows(response)
  
  # get the dataset where the file comes from
  datasets_of_current_par <- files_metadata %>% 
    filter(Endpoints %in% id) %>%
    unique(datasets_id)
  
  # get the different names of the columns of the files downloaded
  parseparameters <- files_metadata %>% 
    filter(datasets_of_current_par %in% datasets_id) %>%
    filter(par_id_to_dl %in% parameters_id)
  
  # get the time column and the column of the parameter wanted and change the column name
  
  # add the metadata of "files id", "dataset id", "lake name", "parse parameters name"
  
  # add up all the files downloaded together to have a final dataframe with all data from one parameter
    
}

######################################################################


get_filetype <- function(file_id, files_metadata) {
  files_metadata$filetype[files_metadata$id == file_id][1]
}

download_and_read_file <- function(file_id, files_metadata, base_url) {
  
  filetype <- get_filetype(file_id, files_metadata)
  if (is.na(filetype)) stop("Unknown filetype")
  
  message("  ├─ File ", file_id, " (", filetype, ")")
  
  tryCatch({
    
    resp <- request(paste0(base_url, file_id)) %>% req_perform()
    
    if (filetype == "nc") {
      tmp <- tempfile(fileext = ".nc")
      writeBin(resp$body, tmp)
      nc <- nc_open(tmp)
      return(list(success = TRUE, type = "nc", nc = nc, tmp = tmp))
    }
    
    stop("Unsupported filetype")
    
  }, error = function(e) {
    message("  │   ✗ FAILED")
    list(success = FALSE)
  })
}

extract_nc_parameter <- function(nc, dataset_id, par_id, lake_parameters_summary) {
  
  time_axis <- lake_parameters_summary %>%
    filter(
      datasets_id == dataset_id,
      parameters_id == 1,
      parseparameter == "time"
    ) %>%
    pull(axis)
  
  value_axes <- lake_parameters_summary %>%
    filter(
      datasets_id == dataset_id,
      parameters_id == par_id
    )
  
  time_var <- time_axis[time_axis %in% names(nc$var)][1]
  value_vars <- intersect(value_axes$axis, names(nc$var))
  
  if (is.na(time_var) || length(value_vars) == 0) return(NULL)
  
  time <- ncvar_get(nc, time_var)
  
  time_units <- ncatt_get(nc, time_var, "units")$value
  origin <- sub(".*since ", "", time_units)
  multiplier <- ifelse(grepl("days", time_units), 86400,
                       ifelse(grepl("hours", time_units), 3600, 1))
  
  datetime <- as.POSIXct(time * multiplier, origin = origin, tz = "UTC")
  
  map_dfr(value_vars, function(v) {
    tibble(
      datetime = datetime,
      value = ncvar_get(nc, v),
      axis = v
    )
  }) %>%
    left_join(value_axes, by = "axis")
}

download_parameter_data <- function(
    par_id,
    base_url = "https://api.datalakes-eawag.ch/download/",
    files_metadata,
    lake_parameters_summary
) {
  
  datasets_id <- unique(
    lake_parameters_summary$datasets_id[
      lake_parameters_summary$parameters_id == par_id
    ]
  )
  
  failed_files <- integer()
  out <- list()
  
  message("Starting download for parameter ", par_id)
  message("Found ", length(datasets_id), " datasets\n")
  
  for (d in datasets_id) {
    
    message("▶ Dataset ", d)
    
    files <- files_metadata$id[files_metadata$datasets_id == d]
    message("  Files: ", length(files))
    
    for (f in files) {
      
      res <- download_and_read_file(
        file_id = f,
        files_metadata = files_metadata,
        base_url = base_url
      )
      
      if (!res$success) {
        failed_files <- c(failed_files, f)
        next
      }
      
      if (res$type != "nc") next
      
      df <- extract_nc_parameter(
        nc = res$nc,
        dataset_id = d,
        par_id = par_id,
        lake_parameters_summary = lake_parameters_summary
      )
      
      nc_close(res$nc)
      unlink(res$tmp)
      
      if (!is.null(df)) {
        df$file_id <- f
        df$dataset_id <- d
        out[[length(out) + 1]] <- df
      }
    }
    
    message("✔ Finished dataset ", d, "\n")
  }
  
  list(
    data = bind_rows(out),
    failed_files = unique(failed_files)
  )
}


download_and_read_file(
  file_id = 14,
  files_metadata = files_metadata,
  base_url = "https://api.datalakes-eawag.ch/download/"
)

