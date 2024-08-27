########################################
### 05. 2022 species response matrix ###
########################################

#### The purpose of this script is to create species response matricies for
#### each NBP count circle in 2022 so they may be ordinated against traffic flow
#### data

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(readxl)

# load tidied nbp data
dat <- read_xlsx("data/b_intermediate_data/nbp_tidy_jan_24.xlsx")

# load circle code lookup table
circ.lu <- read_csv("data/c_analysis_data/nbp_circ_codes.csv")

# Filter and structure data to create 2022 species response matricies

dsrm <- dat %>%
  ## filter out hybirds and sp. reports and filter for just 2022
  filter(year == 2022, !str_detect(species, pattern = " sp."), !str_detect(species, pattern = " x ")) %>%
  
  ## remove spaces from species names
  group_by(park, loop, station, species) %>%
  
  ## sum seen + fly + heard observations
  reframe(obs = sum(seen, fly, heard)) %>%
  
  ## replace spaces in species names so they don't behave strangely later
  mutate(species = str_replace_all(species, pattern = " ", replacement = ".")) %>%
  
  ## pivot wider with each species as a column and observed numbers as the value
  pivot_wider(names_from = species, values_from = obs) %>%
  
  ## replace nas with zero (keeping in mind that zero doesn't necessarily mean zero...)
  replace(is.na(.), 0)

## inspect data frame
view(dsrm)  ## looks good

## Join with cicle codes, keep just codes
srm <- left_join(dsrm, circ.lu) %>%
  select(code, everything(), -park, -loop, -station)

## ready for export
write.csv(srm, "data/c_analysis_data/nbp_srm_2022.csv", row.names = FALSE)
