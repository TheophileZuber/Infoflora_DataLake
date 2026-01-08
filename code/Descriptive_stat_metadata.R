# Loading packages
library(tidyverse)
library(ggplot2)

#load metadata
load("data/Metadata.Rdata")

load("data/presence_absence.RData")
lake_presence_absence_parameters <- parameter_presence_absence
rm(presence_absence,parameterID_presence_absence,parameter_presence_absence)

load("data/Unk_datasets_presence_absence.RData")

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
  slice_max(n_lakes, n = 20) %>%
  ggplot(aes(x = reorder(parameter, n_lakes), y = n_lakes)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Parameter",
    y = "Number of lakes sampled",
    title = "Number of parameters across lakes"
  )


### SAVE IMPORTANT FILE TO SEND
load("results/lake_parameters_metadata.RData")
load("results/Parameters_metadata_per_lake_table.Rdata")
Parameters_metadata_per_lake_table <- pa_final
save(Parameters_metadata_per_lake_table, file = "results/Parameters_metadata_per_lake_table.Rdata")
