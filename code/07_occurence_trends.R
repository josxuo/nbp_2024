#############################
####### 00. occupancy #######
#############################

# clear environment

rm(list = ls())

# libraries
library(tidyverse)
library(readxl)
library(tidyverse)

# load data

nbp <- read_xlsx("data/b_intermediate_data/nbp_tidy_jan_24.xlsx")
circ <- read_csv("data/c_analysis_data/nbp_circ_codes.csv")
covs <- read_csv("data/c_analysis_data/circ_no_overlap_covariates.csv")

### ANALYSIS OBJECTIVE: IDENTIFY SPECIES EXPERIENCING FASTEST LOCAL DECLINES ###
# Seek: 1) Declines in reported abundance; 2) Declines in proportion of surveys reporting;
# 3) Declines in occupancy.

## t1 = 2004
## t2 = 2023
## exclude 2020

## aggregate data across all parks

# functions
## extract overall p value from linear models
overall_p <- function(my_model){
  f <- summary(my_model)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  attributes(p) <- NULL
  return(p)
}

# function for returning count circle codes with a complete set of survey data for a given set of years and months
complete <- function(years, months, data) {
  int <- data %>%
    filter(year %in% years, month %in% months, exclusions == "none")
  
  comp <- int %>%
    group_by(station.code, year) %>%
    reframe(nsurv = n_distinct(survey_id)) %>%
    pivot_wider(names_from = year, values_from = nsurv) %>%
    replace(is.na(.), 0) %>%
    mutate(row_sum = apply(.[, -1], 1, sum)) %>%
    filter(row_sum == length(years) * length(months)) %>%
    pull(station.code)
}


# will want to exclude introduced species
intros <- c("European Starling", "House Sparrow", "Rock Pigeon", "Eurasian Collared-Dove",
            "California Quail")


d <- nbp %>%
  # filter for years after 2003, excluding incomplete years 2020 and 2024.
  filter(year > 2003, year != 2020, year != 2024, 
         # filter out sp. records and hybrid records
         !str_detect(species, pattern = " sp\\.| x "),
         # filter out introduced species
         !species %in% intros,
         # filter for just non-overlapping count circles
         station.code %in% covs$Station)

# inspect data
## sort(unique(d$species))
## sort(unique(d$year))
## sort(unique(d$code))

## Select circles with complete data for a given set of years, months to analyze
sus <- complete(years = c(2004:2019, 2022, 2023), months = c(4), data = d)

# Set up tables/calculations useful later
## number of surveys conducted each year
nsurvPerYear <- d %>%
  filter(station.code %in% sus) %>%
  group_by(year) %>%
  summarise(nsurv = n_distinct(survey_id))
nsurvPerYear



## species response matrix -- mean abundance per survey by year
srmAbund <- d %>%
  filter(station.code %in% sus) %>%
  group_by(year, species) %>%
  summarise(count = sum(seen, heard, fly)) %>%
  ungroup() %>%
  left_join(., nsurvPerYear) %>%
  mutate(maps = count / nsurv) %>%
  select(-count) %>%
  pivot_wider(names_from = species, values_from = maps) %>%
  replace(is.na(.), 0)

## species response matrix -- proportion of surveys reporting detections per year
srmDet <- d %>%
  filter(station.code %in% sus) %>%
  group_by(year, species) %>%
  summarise(det = n_distinct(survey_id)) %>%
  ungroup() %>%
  left_join(., nsurvPerYear) %>%
  mutate(detProp = det / nsurv) %>%
  select(-det) %>%
  pivot_wider(names_from = species, values_from = detProp) %>%
  replace(is.na(.), 0)

## Species mean abundance per survey trends
d.maps <- srmAbund %>% pivot_longer(3:length(srmAbund), names_to = "species", values_to = "maps")

spp <- sort(unique(d.maps$species)) # list of unique species in maps data set to use for indexing
rate <- numeric(length = length(spp))
pval <- numeric(length = length(spp))

for(i in 1:length(spp)){
  df <- d.maps[d.maps$species == spp[i], ]
  mod <- lm(maps ~ year, data = df)
  rate[i] <- coef(mod)[2]
  pval[i] <- overall_p(mod)
}

## combine species, rates, and pvalues into data table
maps.rates <- data.frame(species = spp,
                         maps.rate = rate,
                         maps.pval = pval)

## identify statistically significant and top 10 fastest declining species by mean abund per suv
maps.decline.20 <- maps.rates %>%
  filter(maps.pval < 0.05) %>%
  slice_min(., maps.rate, n = 20)

## Species proportion of surveys reporting trends
d.prop <- srmDet %>% pivot_longer(3:length(srmDet), names_to = "species", values_to = "prop")

spp <- sort(unique(d.prop$species)) # list of unique species in maps data set to use for indexing
rate <- numeric(length = length(spp))
pval <- numeric(length = length(spp))

for(i in 1:length(spp)){
  df <- d.prop[d.prop$species == spp[i], ]
  mod <- lm(prop ~ year, data = df)
  rate[i] <- coef(mod)[2]
  pval[i] <- overall_p(mod)
}

prop.rates <- data.frame(species = spp,
                         prop.rate = rate,
                         prop.pval = pval)

prop.decline.20 <- prop.rates %>%
  filter(prop.pval < 0.05) %>%
  slice_min(., prop.rate, n = 20)

topDecliners <- inner_join(maps.decline.20, prop.decline.20)
sort(unique(prop.decline.20$species))
sort(unique(maps.decline.20$species))

# we now have our declining species list
concern <- sort(unique(topDecliners$species)) 

ggplot(filter(d.maps, species %in% concern), aes(x = year, y = maps)) +
  geom_smooth(method = "loess", se = FALSE, aes(color = species)) #+
  #theme_bcs()

ggplot(filter(d.prop, species %in% concern), aes(x = year, y = prop)) +
  geom_smooth(method = "loess", se = FALSE, aes(color = species))# +
  #theme_bcs()


ggplot(filter(d.maps, species == "American Coot"), aes(x = year, y = maps)) +
  geom_point() +
  geom_smooth(method = lm)


amro <- lm(maps ~ year, data = filter(d.maps, species == "American Robin"))
summary(amro)
overall_p(coot)
coef(coot)[2]

## Maybe we could also look at maximum flock size per year?

maxFlock <- d %>%
  mutate(count = seen + fly + heard) %>%
  group_by(year, species) %>%
  summarise(max = max(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = species, values_from = max) %>%
  replace(is.na(.), 0)

d.max <- maxFlock %>%
  pivot_longer(2:length(maxFlock), names_to = "species", values_to = "max")

spp <- sort(unique(d.max$species)) # list of unique species in maps data set to use for indexing
rate <- numeric(length = length(spp))
pval <- numeric(length = length(spp))

for(i in 1:length(spp)){
  df <- d.max[d.max$species == spp[i], ]
  mod <- lm(max ~ year, data = df)
  rate[i] <- coef(mod)[2]
  pval[i] <- overall_p(mod)
}

max.rates <- data.frame(species = spp,
                         max.rate = rate,
                         max.pval = pval)

max.decline.20 <- max.rates %>%
  filter(max.pval < 0.05) %>%
  slice_min(., max.rate, n = 20)

max.decline.20

ggplot(filter(d.max, species %in% max.decline.20$species), aes(x = year, y = max)) +
  geom_smooth(method = "loess", se = FALSE) +#, color = bcs_colors['dg']) + 
  facet_wrap(~species, scales = "free") #+
#  theme_bcs()

