

# loading the dataset stored in github
base_address <- "https://raw.githubusercontent.com/SG540/data_science_portfolio"
specific_address <- "/main/GAM_handedness/data_handedness.csv"
data <- read.csv(paste0(base_address, specific_address))

data_at_OSF <- read.csv(here::here("Paper_2",
                                   "original_materials",
                                   "osfstorage-archive",
                                   "data_handedness.csv"))

library(tidyverse)

data %>%
    anti_join(data_at_OSF,
              by = colnames(data))

data_at_OSF %>%
    anti_join(data,
              by = colnames(data_at_OSF))
