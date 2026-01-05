library(tidyverse)
library(httr2)
library(jsonlite)

#Define access Token 
Token <- "123456"

#Define base URL
Base_URL <- "https://api.datalakes-eawag.ch"

#Define end points
metadata_endpoint <- "/files/"
Temperature_monitoring_Lenkerseeli_endpoint <- "/download/csv/1"
#Define request
req_metadata <- request(paste0(Base_URL,metadata_endpoint))
req_csv <- request(paste0(Base_URL,Temperature_monitoring_Lenkerseeli_endpoint))
#Provide access token via header

Header <- req_metadata %>% req_headers(
  api_key = Token 
)


Header_csv <- req_csv %>% req_headers(
  api_key = Token 
)

# GET request
perform_req_get <- Header %>% req_perform()
req_csv_data <- Header_csv %>% req_perform()

# results of the request
results_metadata <- perform_req_get %>% resp_body_json()
# results_metadata[[1]]$citation
# str(results_metadata)
# str(results_metadata[[1]])

?resp_body_raw
?purrr
?map

results_csv <- req_csv_data %>% resp_body_json()

# List of lists into dataframe

# df_1 <- map(results_metadata[[1]],as.data.frame)
# str(df_1)
# df <- as.data.frame(results_metadata[[1]])
# test <- do.call(cbind,results_metadata[[1]])

df_1 <- do.call(cbind,results_metadata[[1]])
df_2 <- do.call(cbind,results_metadata[[2]])
df_3 <- do.call(cbind,results_metadata[[3]])
bdf <- do.call(cbind,results_metadata)
df_1 <- cbind(results_metadata[[1]])
test <- fromJSON(results_metadata)
