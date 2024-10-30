#################################################
### 08. community response to land use change ###
#################################################


# Question: Do sites that have experienced measurable land use change differ
# in species composition pre/post change?

# objective: export species response matrix for sites with complete data for years 2019 and 2023. 
# data will be used for 

# libraries
library(tidyverse)
library(readxl)
library(xlsx)
library(openxlsx)

# load data
d <- read_xlsx("data/b_intermediate_data/nbp_tidy_jan_24.xlsx") 
circs <- read_csv("data/c_analysis_data/nbp_circ_codes.csv")
hrcd <- read_xls("data/zz_miscellaneous_data/NBP_countCircles_Points_withNearHRCD.xls")
traits <- read_xlsx("data/zz_miscellaneous_data/NBP_species_list+functional_traits.xlsx")

# add station codes to nbp data
d <- left_join(d, circs) %>% select(obs_id, year, month, code, everything())

# add alpha code to nbp data
d <- left_join(d, select(traits, com.name, alpha.code), join_by("species" == "com.name"))

# create a sorting order for codes
sort.order <- data.frame(code = circs$code,
                       sort.ord = seq(1:length(circs$code))) %>% na.exclude()


dat <- d %>%
  filter(year %in% c(2019, 2023), 
         code %in% hrcd$Station,
         !str_detect(species, pattern = " sp.| x "))

completeness <- dat %>%
  group_by(code, year) %>%
  reframe(nsurv = n_distinct(survey_id)) %>%
  pivot_wider(names_from = year, values_from = nsurv) %>%
  filter(`2019` == 12 & `2023` == 12)

view(completeness)

## not sufficient. Maybe look at just spring when birds are most vocal?

dat2 <- d %>%
  filter(year %in% c(2019, 2023),
         month %in% c(4, 5, 6),
         code %in% hrcd$Station,
         !str_detect(species, pattern = " sp.| x "))

completeness2 <- dat2 %>%
  group_by(code, year) %>%
  reframe(nsurv = n_distinct(survey_id)) %>%
  pivot_wider(names_from = year, values_from = nsurv) %>%
  filter(`2019` == 3 & `2023` == 3)

view(completeness2)

view(filter(hrcd, Station %in% completeness2$code))

# this is a better list

df <- d %>%
  filter(year %in% c(2019, 2023),
         month %in% c(4, 5, 6),
         !str_detect(species, pattern = " sp.| x "),
         code %in% completeness2$code,
         code != "LFM1") ## lake forest park is an oddball, exclude


spp <- sort(unique(df$alpha.code))

srm <- df %>%
  group_by(code, year, alpha.code) %>%
  summarise(count = sum(seen, heard)) %>%
  pivot_wider(names_from = alpha.code, values_from = count) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  left_join(., sort.order) %>%
  arrange(year, sort.ord) %>%
  mutate(site = paste(code, year, sep = "-")) %>%
  select(site, all_of(spp))

view(srm)
 
# ok I think this looks good.

# now filter hrcd data for export

hrcd.d <- hrcd %>%
  filter(Station %in% completeness2$code,
         Station != "LFM1") %>%
  select(Station, Park, DeltaCanopy, NEAR_DIST, NEAR_CHANGE)

hrcd.df <- hrcd.d %>%
  rbind(., hrcd.d) %>%
  mutate(year = c(rep(2019, dim(hrcd.d)[1]), rep(2023, dim(hrcd.d)[1])),
         site = paste(Station, year, sep = "-")) %>%
  select(site, NEAR_CHANGE, NEAR_DIST, DeltaCanopy)

view(hrcd.df)

## good--export files
write.xlsx(srm, "data/c_analysis_data/srm_2019&2023_hrcd.xlsx")
write.xlsx(hrcd.df, "data/c_analysis_data/covs_2019&2023_hrcd.xlsx")


