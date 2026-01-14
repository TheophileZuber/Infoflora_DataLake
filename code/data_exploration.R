# Loading packages
library(tidyverse)
library(httr2)
library(jsonlite)
library(ncdf4)
library(purrr)
library(tibble)

#load metadata
load("data/Metadata.Rdata")
load('results/list_of_potential_parameters.Rdata')

# load("data/presence_absence.RData")
# lake_presence_absence_parameters <- parameter_presence_absence
# rm(presence_absence,parameterID_presence_absence,parameter_presence_absence)
# load("data/Unk_datasets_presence_absence.RData")
# load("results/lake_parameters_metadata.RData")

# ### Define base URL
# # Base_URL <- "https://api.datalakes-eawag.ch/download/csv/"
# base_url <- "https://api.datalakes-eawag.ch/download/"
# remove duplicates of ids that are linked to license and other
parameters <- parameters %>% filter(!grepl("license", name, ignore.case = TRUE))
parameters <- parameters %>% filter(!grepl("GNU", name, ignore.case = TRUE))
anyDuplicated(parameters$name)
######################################################################
extract_nc_parameter <- function(
    nc,
    dataset_id,
    par_id,
    lake_parameters_summary,
    parameters
) {
  
  ## ---- 1. Parameter name ----
  param_name <- parameters$name[parameters$id == par_id]
  
  if (length(param_name) == 0 || is.na(param_name)) return(NULL)
  if (!param_name %in% names(nc$var)) return(NULL)
  
  ## ---- 2. Time dimension ----
  if ("time" %in% names(nc$dim)) {
    time_dim <- "time"
  } else {
    time_dim <- names(nc$dim)[
      sapply(nc$dim, function(d) grepl("since", d$units))
    ][1]
  }
  
  if (is.na(time_dim)) return(NULL)
  
  time_vals  <- nc$dim[[time_dim]]$vals
  origin <- sub(".*since ", "", nc$dim[[time_dim]]$units)
  datetime <- as.POSIXct(time_vals, origin = origin, tz = "UTC")
  
  ## ---- 3. Extract variable & dimensions ----
  vals <- ncvar_get(nc, param_name)
  var_dims <- sapply(nc$var[[param_name]]$dim, `[[`, "name")
  
  ## ---- 4. Build dataframe ----
  
  # TIME ONLY
  if (length(var_dims) == 1 && var_dims[1] == time_dim) {
    
    df <- tibble(
      datetime = datetime,
      value    = vals
    )
    
    # DEPTH x TIME
  } else if (time_dim %in% var_dims && length(var_dims) == 2) {
    
    depth_dim <- setdiff(var_dims, time_dim)
    depth_vals <- nc$dim[[depth_dim]]$vals
    
    df <- expand.grid(
      depth    = depth_vals,
      datetime = datetime
    )
    
    df$value <- as.vector(vals)
    
  } else {
    return(NULL)
  }
  
  ## ---- 5. Attach axis (SAFE) ----
  meta <- lake_parameters_summary %>%
    filter(
      datasets_id == dataset_id,
      parameters_id == par_id
    ) %>%
    select(axis, unit, parseparameter)
  
  if (nrow(meta) > 0) {
    df$axis <- meta$axis[1]
  } else {
    df$axis <- param_name
    df$unit <- NA_character_
    df$parseparameter <- NA_character_
    return(df)
  }
  
  ## ---- 6. Attach metadata ----
  df <- df %>%
    left_join(meta, by = "axis")
  
  ## ---- 7. Final column order ----
  final_cols <- c("datetime", "value", "axis", "unit", "parseparameter")
  
  if ("depth" %in% names(df)) {
    final_cols <- c("datetime", "depth", "value", "axis", "unit", "parseparameter")
  }
  
  df <- df %>% select(all_of(final_cols))
  
  return(df)
}



download_parameter_data <- function(
    par_id,
    base_url = "https://api.datalakes-eawag.ch/download/",
    files_metadata,
    lake_parameters_summary,
    parameters
) {
  
  datasets_id <- unique(
    lake_parameters_summary$datasets_id[
      lake_parameters_summary$parameters_id == par_id
    ]
  )
  
  out <- list()
  failed_files <- integer()
  skipped_files <- integer()
  
  message("Starting download for parameter ", par_id)
  message("Found ", length(datasets_id), " datasets\n")
  
  for (d in datasets_id) {
    message("▶ Dataset ", d)
    
    files <- files_metadata %>%
      filter(datasets_id == d) %>%
      pull(id) %>%
      unique() %>%
      na.omit()
    message("  Files: ", length(files))
    
    for (f in files) {
      filetype <- files_metadata %>%
        filter(id == f) %>%
        pull(filetype)
      message("  ├─ File ", f, " (", filetype, ")")
      
      res <- tryCatch({
        resp <- request(paste0(base_url, f)) %>% req_perform()
        list(resp = resp, type = filetype)
      }, error = function(e) {
        failed_files <<- c(failed_files, f)
        message("  │   ✗ FAILED")
        NULL
      })
      if (is.null(res)) next
      
      df <- NULL
      if (filetype == "json") {
        raw <- res$resp %>% resp_body_json()
        df <- extract_json_parameter(raw, d, par_id, lake_parameters_summary)
      }
      
      if (filetype == "nc") {
        tmp <- tempfile(fileext = ".nc")
        writeBin(res$resp$body, tmp)
        nc <- ncdf4::nc_open(tmp)
        df <- extract_nc_parameter(
          nc,
          d,
          par_id,
          lake_parameters_summary,
          parameters
        )
        ncdf4::nc_close(nc)
        unlink(tmp)
      }
      
      if (is.null(df)) {
        message("  │   ↷ skipped (parameter not present in file)")
        skipped_files <- c(skipped_files, f)
        next
      }
      
      df$file_id <- f
      df$dataset_id <- d
      df$filetype <- filetype
      out[[length(out) + 1]] <- df
    }
    
    message("✔ Finished dataset ", d, "\n")
  }
  
  list(
    data = if(length(out) > 0) bind_rows(out) else tibble(),
    failed_files = unique(failed_files),
    skipped_files = unique(skipped_files)
  )
}


res_test <- download_parameter_data(
  par_id = 14,
  files_metadata = files_metadata,
  lake_parameters_summary = lake_parameters_summary,
  parameters = parameters
)
