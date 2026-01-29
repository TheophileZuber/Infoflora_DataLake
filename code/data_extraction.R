# Loading packages
library(data.table)
library(httr2)
library(jsonlite)
library(ncdf4)
library(future)
library(furrr)
library(tidyverse)
# Load metadata
load("data/Metadata.Rdata")
load('results/list_of_potential_parameters.Rdata')

dir.create("chunks", showWarnings = FALSE)

# ==========================
# HELPER FUNCTION - NEW
# ==========================

identify_depth_case <- function(json, dataset_id, file_id, 
                                lake_parameters_summary, 
                                files_metadata, 
                                datasets_metadata) {
  
  # Ensure file_id is a scalar
  file_id <- as.integer(file_id[1])
  
  # Convert to data.table if needed
  if (!is.data.table(files_metadata)) {
    files_metadata <- as.data.table(files_metadata)
  }
  if (!is.data.table(datasets_metadata)) {
    datasets_metadata <- as.data.table(datasets_metadata)
  }
  if (!is.data.table(lake_parameters_summary)) {
    lake_parameters_summary <- as.data.table(lake_parameters_summary)
  }
  
  # Check for depth parameter in metadata
  depth_meta <- lake_parameters_summary[
    datasets_id == dataset_id & parseparameter == "depth"
  ]
  
  # CASE 4 or CASE 2: depth parameter exists
  if (nrow(depth_meta) > 0) {
    depth_axis <- depth_meta$axis[1]
    
    if (depth_axis %in% names(json)) {
      depth_list <- json[[depth_axis]]
      
      # CASE 4: depth_list is a simple vector (for 2D data structure)
      if (is.list(depth_list) && !is.list(depth_list[[1]])) {
        depth_values <- as.numeric(unlist(depth_list))
        return(list(
          case = 4,
          depth_axis = depth_axis,
          depth_values = depth_values,
          single_depth = NA
        ))
      }
      
      # CASE 2: depth as parallel list to other parameters
      return(list(
        case = 2,
        depth_axis = depth_axis,
        depth_values = as.numeric(unlist(depth_list)),
        single_depth = NA
      ))
    }
  }
  
  # CASE 1: Check if depth encoded in parseparameter names
  param_meta <- lake_parameters_summary[datasets_id == dataset_id]
  if (any(grepl("\\d+\\.?\\d*m$", param_meta$parseparameter))) {
    return(list(
      case = 1,
      depth_axis = NA,
      depth_values = NULL,
      single_depth = NA
    ))
  }
  
  # CASE 3: Check if single depth in BOTH metadata sources
  # Get metadata from files_metadata
  file_meta <- files_metadata[id == file_id]
  file_has_depth <- FALSE
  file_mindepth <- NA
  file_maxdepth <- NA
  
  if (nrow(file_meta) > 0 && !is.na(file_meta$mindepth[1]) && !is.na(file_meta$maxdepth[1])) {
    file_has_depth <- TRUE
    file_mindepth <- as.numeric(file_meta$mindepth[1])
    file_maxdepth <- as.numeric(file_meta$maxdepth[1])
  }
  
  # Get metadata from datasets_metadata
  dataset_meta <- datasets_metadata[id == dataset_id]
  dataset_has_depth <- FALSE
  dataset_mindepth <- NA
  dataset_maxdepth <- NA
  
  if (nrow(dataset_meta) > 0 && !is.na(dataset_meta$mindepth[1]) && !is.na(dataset_meta$maxdepth[1])) {
    dataset_has_depth <- TRUE
    dataset_mindepth <- as.numeric(dataset_meta$mindepth[1])
    dataset_maxdepth <- as.numeric(dataset_meta$maxdepth[1])
  }
  
  # CASE 3: Both sources have depth AND mindepth == maxdepth in BOTH
  if (file_has_depth && dataset_has_depth) {
    if (file_mindepth == file_maxdepth && dataset_mindepth == dataset_maxdepth) {
      # Use the file metadata depth (more specific)
      return(list(
        case = 3,
        depth_axis = NA,
        depth_values = NULL,
        single_depth = file_mindepth
      ))
    }
  }
  
  # CASE 3 variant: Only file_metadata has depth and mindepth == maxdepth
  # (datasets_metadata might be NA or have different min/max)
  if (file_has_depth && file_mindepth == file_maxdepth && !dataset_has_depth) {
    return(list(
      case = 3,
      depth_axis = NA,
      depth_values = NULL,
      single_depth = file_mindepth
    ))
  }
  
  # CASE 5: No depth information available OR mindepth != maxdepth
  # This includes:
  # - Both metadata sources have NA for depth
  # - file_metadata has mindepth != maxdepth
  # - datasets_metadata has mindepth != maxdepth
  # - Any other case where we can't determine a single depth
  return(list(
    case = 5,
    depth_axis = NA,
    depth_values = NULL,
    single_depth = -999
  ))
}

# ==========================
# FIXED extract_json_parameter - with data.table conversion
# ==========================

extract_json_parameter <- function(json, dataset_id, file_id, par_id, 
                                   lake_parameters_summary,
                                   files_metadata,
                                   datasets_metadata) {
  
  if (!is.list(json)) return(NULL)
  
  # Convert to data.table if needed
  if (!is.data.table(lake_parameters_summary)) {
    lake_parameters_summary <- as.data.table(lake_parameters_summary)
  }
  if (!is.data.table(files_metadata)) {
    files_metadata <- as.data.table(files_metadata)
  }
  if (!is.data.table(datasets_metadata)) {
    datasets_metadata <- as.data.table(datasets_metadata)
  }
  
  # Ensure file_id is scalar
  file_id <- as.integer(file_id[1])
  
  ## ---- 1. Time axis ---- (UNCHANGED)
  time_meta <- lake_parameters_summary[
    datasets_id == dataset_id & parameters_id == 1 & parseparameter == "time"
  ]
  
  time_axes <- time_meta$axis
  time_axis <- intersect(time_axes, names(json))[1]
  if (is.na(time_axis)) return(NULL)
  
  datetime <- as.POSIXct(unlist(json[[time_axis]]), origin = "1970-01-01", tz = "UTC")
  n_time <- length(datetime)
  
  ## ---- 2. Get parameter metadata ---- (UNCHANGED)
  meta_rows <- lake_parameters_summary[
    datasets_id == dataset_id & parameters_id == par_id,
    .(axis, unit, parseparameter)
  ]
  
  if (nrow(meta_rows) == 0) return(NULL)
  
  ## ---- 2.5. Identify depth case ---- (NEW)
  depth_info <- identify_depth_case(
    json, dataset_id, file_id,
    lake_parameters_summary,
    files_metadata,
    datasets_metadata
  )
  
  ## ---- 3. Extract all parameters ----
  result_list <- vector("list", nrow(meta_rows) * 100)
  result_count <- 0
  
  for (i in seq_len(nrow(meta_rows))) {
    
    current_axis <- meta_rows$axis[i]
    current_parseparam <- meta_rows$parseparameter[i]
    current_unit <- meta_rows$unit[i]
    
    if (!current_axis %in% names(json)) next
    
    mat <- json[[current_axis]]
    if (!is.list(mat) && !is.numeric(mat)) next
    
    # CASE 1: 1D vector (time-only)
    if (length(mat) == n_time && !is.list(mat[[1]])) {
      vals <- as.numeric(unlist(lapply(mat, function(z) if (is.null(z)) NA else z)))
      
      if (length(vals) != n_time) next
      
      valid_idx <- which(!is.na(vals))
      if (length(valid_idx) == 0) next
      
      # MODIFIED: Add depth column based on case
      depth_col <- if (depth_info$case == 1) {
        # Extract depth from parseparameter (e.g., "0.5m" -> 0.5)
        depth_str <- gsub("^.*?(\\d+\\.?\\d*)m$", "\\1", current_parseparam)
        if (grepl("^\\d+\\.?\\d*$", depth_str)) {
          as.numeric(depth_str)
        } else {
          depth_info$single_depth
        }
      } else if (depth_info$case == 2) {
        # Use parallel depth list
        depth_info$depth_values[valid_idx]
      } else if (depth_info$case == 3) {
        # Single depth from metadata
        depth_info$single_depth
      } else {
        # Case 5: no depth info
        depth_info$single_depth
      }
      
      result_count <- result_count + 1
      result_list[[result_count]] <- data.table(
        datetime = datetime[valid_idx],
        value = vals[valid_idx],
        depth = depth_col,
        axis = current_axis,
        unit = current_unit,
        parseparameter = current_parseparam
      )
      next
    }
    
    # CASE 2: 2D (depth × time)
    if (!is.list(mat)) next
    
    n_levels <- length(mat)
    
    # MODIFIED: Loop through ALL levels instead of just min_level
    for (level_idx in seq_len(n_levels)) {
      
      level_data <- mat[[level_idx]]
      
      if (!is.list(level_data)) next
      
      vals <- sapply(level_data, function(x) if (is.null(x)) NA_real_ else as.numeric(x))
      
      if (length(vals) != n_time) next
      
      # Apply quality mask if present (UNCHANGED)
      qual_name <- paste0(current_parseparam, "_qual")
      
      if (qual_name %in% names(json)) {
        qual_data <- json[[qual_name]]
        
        if (is.list(qual_data) && length(qual_data) >= level_idx) {
          qual_level <- qual_data[[level_idx]]
          
          if (is.list(qual_level) && length(qual_level) == length(vals)) {
            qual_vals <- as.integer(unlist(qual_level))
            vals[qual_vals != 0] <- NA_real_
          }
        } else if (!is.list(qual_data) && length(qual_data) == length(vals)) {
          qual_vals <- as.integer(unlist(qual_data))
          vals[qual_vals != 0] <- NA_real_
        }
      }
      
      valid_idx <- which(!is.na(vals))
      if (length(valid_idx) == 0) next
      
      # MODIFIED: Get depth for this level
      depth_col <- if (depth_info$case == 4 && !is.null(depth_info$depth_values)) {
        # Use depth values from depth axis
        depth_info$depth_values[level_idx]
      } else if (depth_info$case == 3) {
        depth_info$single_depth
      } else {
        depth_info$single_depth  # Will be -999 for case 5
      }
      
      result_count <- result_count + 1
      result_list[[result_count]] <- data.table(
        datetime = datetime[valid_idx],
        value = vals[valid_idx],
        depth = depth_col,
        axis = current_axis,
        unit = current_unit,
        parseparameter = current_parseparam
      )
    }
  }
  
  ## ---- 4. Combine results ---- (UNCHANGED)
  if (result_count == 0) return(NULL)
  
  rbindlist(result_list[1:result_count], use.names = TRUE)
}

# ===============================
# MODIFIED EXTRACT_NC_PARAMETER
# ===============================

extract_nc_parameter <- function(nc, dataset_id, file_id, par_id, 
                                 lake_parameters_summary,
                                 files_metadata,
                                 datasets_metadata) {
  
  ## ---- 1. Get parameter metadata ---- (UNCHANGED)
  meta_rows <- lake_parameters_summary[
    datasets_id == dataset_id & parameters_id == par_id,
    .(axis, unit, parseparameter)
  ]
  
  if (nrow(meta_rows) == 0) return(NULL)
  
  ## ---- 2. Identify time dimension ---- (UNCHANGED)
  if ("time" %in% names(nc$dim)) {
    time_dim <- "time"
  } else {
    time_dim <- names(nc$dim)[sapply(nc$dim, function(d) grepl("since", d$units))][1]
  }
  
  if (is.na(time_dim)) return(NULL)
  
  time_vals <- nc$dim[[time_dim]]$vals
  origin <- sub(".*since ", "", nc$dim[[time_dim]]$units)
  datetime <- as.POSIXct(time_vals, origin = origin, tz = "UTC")
  n_time <- length(datetime)
  
  ## ---- 2.5. Identify depth dimension ---- (NEW)
  depth_dim <- NULL
  depth_vals <- NULL
  
  # Look for depth dimension
  possible_depth_names <- c("depth", "z", "lev", "level")
  for (dim_name in possible_depth_names) {
    if (dim_name %in% names(nc$dim)) {
      depth_dim <- dim_name
      depth_vals <- nc$dim[[depth_dim]]$vals
      break
    }
  }
  
  # Fallback: check metadata for single depth
  single_depth <- NA
  file_meta <- files_metadata[id == file_id]
  if (nrow(file_meta) > 0 && !is.na(file_meta$mindepth[1]) && !is.na(file_meta$maxdepth[1])) {
    if (file_meta$mindepth[1] == file_meta$maxdepth[1]) {
      single_depth <- as.numeric(file_meta$mindepth[1])
    }
  }
  
  ## ---- 3. Extract all parameters ----
  result_list <- vector("list", nrow(meta_rows) * 100)  # MODIFIED: increased size
  result_count <- 0
  
  for (i in seq_len(nrow(meta_rows))) {
    
    param_name <- meta_rows$parseparameter[i]
    current_axis <- meta_rows$axis[i]
    current_unit <- meta_rows$unit[i]
    
    if (!param_name %in% names(nc$var)) next
    
    vals <- tryCatch(ncvar_get(nc, param_name), error = function(e) NULL)
    if (is.null(vals)) next
    
    var_dims <- sapply(nc$var[[param_name]]$dim, `[[`, "name")
    
    # Case 1: time-only (MODIFIED: add depth column)
    if (length(var_dims) == 1 && var_dims[1] == time_dim) {
      valid_idx <- which(!is.na(vals))
      if (length(valid_idx) == 0) next
      
      # Determine depth value
      depth_col <- if (!is.na(single_depth)) {
        single_depth
      } else {
        -999  # No depth information
      }
      
      result_count <- result_count + 1
      result_list[[result_count]] <- data.table(
        datetime = datetime[valid_idx],
        value = vals[valid_idx],
        depth = depth_col,  # NEW COLUMN
        axis = current_axis,
        unit = current_unit,
        parseparameter = param_name
      )
      next
    }
    
    # Case 2: depth × time (MODIFIED: extract ALL depths)
    if (time_dim %in% var_dims && length(var_dims) == 2) {
      depth_dim_var <- setdiff(var_dims, time_dim)
      
      # Get depth values for this dimension
      if (!is.null(depth_vals) && depth_dim == depth_dim_var) {
        n_depths <- length(depth_vals)
      } else {
        # If no depth values available, use index
        n_depths <- nc$dim[[depth_dim_var]]$len
        depth_vals_fallback <- seq_len(n_depths)
      }
      
      # MODIFIED: Loop through ALL depths
      for (depth_idx in seq_len(n_depths)) {
        
        # Extract values for this depth
        vals_at_depth <- if (var_dims[1] == depth_dim_var) {
          vals[depth_idx, ]
        } else {
          vals[, depth_idx]
        }
        
        valid_idx <- which(!is.na(vals_at_depth))
        if (length(valid_idx) == 0) next
        
        # Get depth value
        depth_value <- if (!is.null(depth_vals)) {
          depth_vals[depth_idx]
        } else if (exists("depth_vals_fallback")) {
          depth_vals_fallback[depth_idx]
        } else if (!is.na(single_depth)) {
          single_depth
        } else {
          -999
        }
        
        result_count <- result_count + 1
        result_list[[result_count]] <- data.table(
          datetime = datetime[valid_idx],
          value = vals_at_depth[valid_idx],
          depth = depth_value,  # NEW COLUMN
          axis = current_axis,
          unit = current_unit,
          parseparameter = param_name
        )
      }
    }
  }
  
  ## ---- 4. Combine results ---- (UNCHANGED)
  if (result_count == 0) return(NULL)
  
  rbindlist(result_list[1:result_count], use.names = TRUE)
}
# ===============================
# Configure parallel execution
# ===============================

plan(multisession, workers = 8)

# ===============================
# MODIFIED PROCESS_ONE_DATASET
# ===============================

process_one_dataset <- function(d, par_id, base_url, files_metadata, 
                                lake_parameters_summary,
                                datasets_metadata) {  # ADDED datasets_metadata
  
  # Identify files (UNCHANGED)
  files_df <- files_metadata[
    datasets_id == d & !is.na(maxdatetime) & as.POSIXct(maxdatetime) >= as.POSIXct("2000-01-01"),
    .(id, filetype)
  ]
  
  if (nrow(files_df) == 0) return(NULL)
  
  files_df <- unique(files_df)
  
  n_json <- sum(files_df$filetype == "json")
  n_nc <- sum(files_df$filetype == "nc")
  
  chosen_type <- if (n_json == n_nc) "json" else if (n_json > n_nc) "json" else "nc"
  
  files <- files_df[filetype == chosen_type, id]
  
  # Collect all data for this dataset in a list
  dataset_results <- vector("list", length(files))
  
  for (idx in seq_along(files)) {
    f <- files[idx]
    
    filetype <- files_metadata[id == f, filetype][1]
    
    # Download with error handling (UNCHANGED)
    res <- tryCatch({
      resp <- request(paste0(base_url, f)) %>% req_perform()
      list(resp = resp, type = filetype)
    }, error = function(e) NULL)
    
    if (is.null(res)) next
    
    df <- NULL
    
    # Extract based on type (MODIFIED: pass new parameters)
    if (filetype == "json") {
      raw <- resp_body_json(res$resp)
      df <- extract_json_parameter(
        raw, d, f, par_id,  # ADDED file_id (f)
        lake_parameters_summary,
        files_metadata,
        datasets_metadata
      )
    } else if (filetype == "nc") {
      tmp <- tempfile(fileext = ".nc")
      writeBin(res$resp$body, tmp)
      nc <- nc_open(tmp)
      df <- extract_nc_parameter(
        nc, d, f, par_id,  # ADDED file_id (f)
        lake_parameters_summary,
        files_metadata,
        datasets_metadata
      )
      nc_close(nc)
      unlink(tmp)
    }
    
    if (!is.null(df) && nrow(df) > 0) {
      # Daily aggregation using data.table (MODIFIED: group by depth too)
      df[, date := as.Date(datetime)]
      df <- df[, .(
        value = mean(value, na.rm = TRUE),
        unit = first(unit)
      ), by = .(date, depth, axis, parseparameter)]  # ADDED depth to grouping
      
      setnames(df, "date", "datetime")
      df <- df[!is.na(value)]
      
      # Add identifiers (UNCHANGED)
      df[, `:=`(file_id = f, dataset_id = d, filetype = filetype)]
      
      dataset_results[[idx]] <- df
    }
  }
  
  # Combine all files from this dataset (UNCHANGED)
  dataset_data <- rbindlist(dataset_results, use.names = TRUE, fill = TRUE)
  
  # Save ONE file per dataset instead of per file (UNCHANGED)
  if (nrow(dataset_data) > 0) {
    chunk_file <- file.path("chunks", paste0("par_", par_id, "_dataset_", d, ".rds"))
    saveRDS(dataset_data, chunk_file)
  }
  
  return(NULL)
}

# ===============================
# MODIFIED DOWNLOAD_PARAMETER_DATA
# ===============================

download_parameter_data <- function(
    par_id,
    base_url = "https://api.datalakes-eawag.ch/download/",
    files_metadata,
    lake_parameters_summary,
    datasets_metadata  # NEW PARAMETER
) {
  
  # Convert to data.table if not already (UNCHANGED)
  if (!is.data.table(lake_parameters_summary)) {
    setDT(lake_parameters_summary)
  }
  if (!is.data.table(files_metadata)) {
    setDT(files_metadata)
  }
  if (!is.data.table(datasets_metadata)) {  # NEW
    setDT(datasets_metadata)
  }
  
  datasets_id <- unique(lake_parameters_summary[parameters_id == par_id, datasets_id])
  
  message("Starting download for parameter ", par_id)
  message("Found ", length(datasets_id), " datasets\n")
  
  # Parallel execution (MODIFIED: pass datasets_metadata)
  future_walk(
    datasets_id,
    process_one_dataset,
    par_id = par_id,
    base_url = base_url,
    files_metadata = files_metadata,
    lake_parameters_summary = lake_parameters_summary,
    datasets_metadata = datasets_metadata,  # NEW
    .progress = TRUE
  )
  
  # Combine all chunks (UNCHANGED)
  files <- list.files("chunks", pattern = paste0("par_", par_id), full.names = TRUE)
  
  if (length(files) == 0) {
    message("No data extracted")
    return(data.table())
  }
  
  final_data <- rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
  
  # Clean up chunks (UNCHANGED)
  file.remove(files)
  
  return(final_data)
}

# ===============================
# Run extraction
# ===============================

# t0 <- Sys.time()
# 
# result_dataframe <- download_parameter_data(
#   par_id = 144,
#   files_metadata = files_metadata,
#   lake_parameters_summary = lake_parameters_summary
# )
# 
# print(Sys.time() - t0)


###################### test out put

# files_metadata %>%
#   filter(
#     datasets_id %in% unique(lake_parameters_summary$datasets_id[
#       lake_parameters_summary$parameters_id == 14
#     ]),
#     !is.na(maxdatetime),
#     as.POSIXct(maxdatetime) >= as.POSIXct("2000-01-01")
#   ) %>%
#   count(datasets_id)

#' remove unnecessary tracking (optimiser,eg: write a line in a txt file), add metadata after?, save locally during parallel work. 
#' add selection of time period
#' list de dataframe with index that is added, stock dataframe with the index lst[[i]] <- df
#' try catch write error in a txt file
#' look if depth is present in the json datasets or in metadata
#' remove fichier na 
#' look if info depth, keep with no depth, sort in function of depth, always take min(depth)
#' several values for the same date (day) average 
#' 2000
#' unit,dataset id, file id, parse parameters
#' check levels in Json files if they correspond to water depth
#' appliquer le filter
#' datatable, no dataframe, no tibble
#' look at how many json file, how many nc files,
#' if equal get the quickest, if only one type take it,
#' if one sup than other get the one with most 
#' (and save the datasets id to go check later on the dataset to see if issues)


# ===============================
# Function: Save extracted parameter with proper name
# ===============================
# Create directory for saved parameters
dir.create("extracted_parameters", showWarnings = FALSE)

save_parameter_by_name <- function(result_dataframe, par_id, parameters_metadata) {
  
  # Convert to data.table if not already
  if (!is.data.table(result_dataframe)) {
    setDT(result_dataframe)
  }
  if (!is.data.table(parameters_metadata)) {
    setDT(parameters_metadata)
  }
  
  # Get parameter name
  param_name <- parameters_metadata[id == par_id, name]
  
  if (length(param_name) == 0) {
    warning(paste("Parameter ID", par_id, "not found in metadata"))
    param_name <- paste0("parameter_", par_id)
  } else {
    param_name <- param_name[1]  # Take first if multiple matches
    # Clean name for filename (remove special characters)
    param_name <- gsub("[^a-zA-Z0-9_-]", "_", param_name)
  }
  
  # Add parameter ID and name to the dataframe
  result_dataframe[, `:=`(
    parameter_id = par_id,
    parameter_name = param_name
  )]
  
  # Save with parameter name
  output_file <- file.path("extracted_parameters", paste0(param_name, ".rds"))
  saveRDS(result_dataframe, output_file)
  
  message("Saved parameter ", par_id, " as '", param_name, "' (", nrow(result_dataframe), " rows)")
  
  return(output_file)
}

# ===============================
# Function: Merge all extracted parameters
# ===============================

merge_all_parameters <- function(
    parameters_dir = "extracted_parameters",
    output_file = "merged_all_parameters.rds",
    merge_by = c("datetime", "dataset_id")
) {
  
  # Get all .rds files in the directory
  param_files <- list.files(
    parameters_dir, 
    pattern = "\\.rds$", 
    full.names = TRUE
  )
  
  if (length(param_files) == 0) {
    stop("No parameter files found in ", parameters_dir)
  }
  
  message("Found ", length(param_files), " parameter files to merge")
  
  # Read all files
  all_params <- lapply(param_files, function(f) {
    message("Reading ", basename(f))
    readRDS(f)
  })
  
  # Combine all into one long table
  combined <- rbindlist(all_params, use.names = TRUE, fill = TRUE)
  
  message("\nTotal rows before merging: ", nrow(combined))
  message("Unique parameters: ", uniqueN(combined$parameter_name))
  message("Unique datasets: ", uniqueN(combined$dataset_id))
  message("Date range: ", min(combined$datetime), " to ", max(combined$datetime))
  
  # Pivot to wide format (one column per parameter)
  # This creates a table where each row is a unique datetime-dataset combination
  # and each parameter gets its own column
  
  setcolorder(
    combined,
    c(
      "datetime",
      "dataset_id",
      "file_id",
      "parameter_id",
      "parameter_name",
      "parseparameter",
      "value",
      "unit"
    )
  )
  
  message("\nMerged data dimensions: ", nrow(combined), " rows × ", ncol(combined), " columns")
  
  # Save merged result
  saveRDS(combined, output_file)
  message("Saved merged data to: ", output_file)
  
  return(combined)
}

# ===============================
# Function: Get summary of extracted parameters
# ===============================

summarize_extracted_parameters <- function(parameters_dir = "extracted_parameters") {
  
  param_files <- list.files(
    parameters_dir, 
    pattern = "\\.rds$", 
    full.names = TRUE
  )
  
  if (length(param_files) == 0) {
    message("No parameter files found")
    return(NULL)
  }
  
  # Get info about each file
  summary_list <- lapply(param_files, function(f) {
    dt <- readRDS(f)
    
    data.table(
      file = basename(f),
      parameter_name = unique(dt$parameter_name)[1],
      parameter_id = unique(dt$parameter_id)[1],
      n_rows = nrow(dt),
      n_datasets = uniqueN(dt$dataset_id),
      n_files = uniqueN(dt$file_id),
      date_min = min(dt$datetime),
      date_max = max(dt$datetime),
      value_min = min(dt$value, na.rm = TRUE),
      value_max = max(dt$value, na.rm = TRUE),
      unit = paste(unique(dt$unit), collapse = ", ")
    )
  })
  
  summary_dt <- rbindlist(summary_list)
  
  return(summary_dt)
}

# ===============================
# USAGE EXAMPLE
# ===============================

# After running the extraction script, save the result:
# save_parameter_by_name(
#   result_dataframe = result_dataframe,
#   par_id = 139,
#   parameters_metadata = selectiontables_metadata
# )

# To check what's been extracted so far:
# summary <- summarize_extracted_parameters()
# print(summary)

# When all parameters are extracted, merge them:
# merged_data <- merge_all_parameters()

# ===============================
# WORKFLOW FOR EXTRACTING MULTIPLE PARAMETERS
# ===============================

extract_and_save_parameter <- function(
    par_id,
    base_url = "https://api.datalakes-eawag.ch/download/",
    files_metadata,
    lake_parameters_summary,
    parameters_metadata
) {
  
  # Source the extraction function from the previous script
  # (Assumes download_parameter_data function is available)
  
  message("\n========================================")
  message("Extracting parameter ID: ", par_id)
  message("========================================\n")
  
  t0 <- Sys.time()
  
  # Extract data
  result_dataframe <- download_parameter_data(
    par_id = par_id,
    base_url = base_url,
    files_metadata = files_metadata,
    lake_parameters_summary = lake_parameters_summary
  )
  
  elapsed <- Sys.time() - t0
  
  if (is.null(result_dataframe) || nrow(result_dataframe) == 0) {
    message("No data extracted for parameter ", par_id)
    return(NULL)
  }
  
  # Save with proper name
  output_file <- save_parameter_by_name(
    result_dataframe = result_dataframe,
    par_id = par_id,
    parameters_metadata = parameters_metadata
  )
  
  message("Extraction time: ", round(elapsed, 2), " ", attr(elapsed, "units"))
  
  return(output_file)
}

# ===============================
# Extract multiple parameters in sequence
# ===============================

extract_multiple_parameters <- function(
    par_ids,
    base_url = "https://api.datalakes-eawag.ch/download/",
    files_metadata,
    lake_parameters_summary,
    parameters_metadata
) {
  
  results <- list()
  
  for (par_id in par_ids) {
    
    result <- tryCatch({
      extract_and_save_parameter(
        par_id = par_id,
        base_url = base_url,
        files_metadata = files_metadata,
        lake_parameters_summary = lake_parameters_summary,
        parameters_metadata = parameters_metadata
      )
    }, error = function(e) {
      message("ERROR extracting parameter ", par_id, ": ", e$message)
      NULL
    })
    
    results[[as.character(par_id)]] <- result
    
    # Clean up memory between extractions
    gc()
  }
  
  # Summary of what was extracted
  message("\n========================================")
  message("EXTRACTION COMPLETE")
  message("========================================")
  summary <- summarize_extracted_parameters()
  print(summary)
  
  return(results)
}

# ===============================
# EXAMPLE USAGE
# ===============================

# Extract a single parameter:
# extract_and_save_parameter(
#   par_id = 139,
#   files_metadata = files_metadata,
#   lake_parameters_summary = lake_parameters_summary,
#   parameters_metadata = selectiontables_metadata
# )

#Extract multiple parameters:
param_list <- c(104,22)  # Add your parameter IDs here
extract_multiple_parameters(
  par_ids = param_list,
  files_metadata = files_metadata,
  lake_parameters_summary = lake_parameters_summary,
  parameters_metadata = selectiontablesparameters_metadata
)

# Check progress:
# summarize_extracted_parameters()

# When all done, merge everything:
final_dataset <- merge_all_parameters(
  parameters_dir = "extracted_parameters",
  output_file = "results/merged_all_parameters.rds"
)



#'
#'
#'NOTE: SI JSON FILE NE FONCTIONNE PAS CHECK NC FILE, OU INVERSEMENT!
#'NE PAS OUBLIER PAS PAR LAKE ID MAIS PAR COORDONNE
