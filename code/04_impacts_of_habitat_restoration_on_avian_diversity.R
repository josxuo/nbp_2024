#######################################
### 04. habitat restoration impacts ###
#######################################


# Battey and Ross selected NBP point count areas within 50 meters of 
# habitat restoration zones at Golden Gardens, Discovery, Carkeek, and Magnuson Parks.

## In ArcGIS, used GSP reference map and select by location tool to identify NBP circles
## adjacent to GSP restoration sites.

# load packages
library(tidyverse)
library(readxl)
library(lme4)

# data
## tidied NPB data
dat <- read_xlsx("data/b_intermediate_data/nbp_tidy_jan_24.xlsx")

## count circles near GSP restoration sites by phase data
gsp <- read_csv("data/c_analysis_data/circ_gsp_phases.csv")

## proportion of count circle overlapping with GSP restoration site by phase
gsp.prop <- read_csv("data/c_analysis_data/circ_gsp_phases_prop_area.csv")

## count circle codes to help join to main data
cod <- read_csv("data/c_analysis_data/nbp_circ_codes.csv")

## The restoration analysis in 2014 asked if there are differences in mean abundance per survey and
## mean annual species diversity, the number of species reported annually per circle.

## Step 1: calculate mean abundance per survey and mean diversity per station per year

#### filter out sp. records and hybrids, filter out incomplete years 1996, 2020, and 2024
dres <- dat %>%
  filter(!str_detect(species, pattern = " sp."), !str_detect(species, pattern = " sp."),
         !year %in% c(1996, 2020, 2024))

## create a diversity / abundance data frame for each station for each year

ddiv <- dres %>%
  group_by(year, park, loop, station) %>%
  summarise(S = n_distinct(species), nsurv = n_distinct(survey_id), maps = sum(seen, heard, fly) / nsurv)


## join diversity data frame with gsp phase table

ddiv$lu <- paste0(ddiv$park, ddiv$loop, ddiv$station)
gsp$lu <- paste0(gsp$park, gsp$loop, gsp$station)

ddiv <- left_join(ddiv, gsp)


## One challenge is that many or most stations are within 50 meters of several GSP phases. How
## many are just near one phase?

### first, replace phase data with 0, 1, 2, 3, 4, and . to make easier to read. Will want to concatenate
### all phase information for each station into a single string "phase_sum"

ddiv$near_phase_0 <- replace(ddiv$near_phase_0, ddiv$near_phase_0 == "y", "0")
ddiv$near_phase_0 <- replace(ddiv$near_phase_0, ddiv$near_phase_0 == "n", ".")

ddiv$near_phase_1 <- replace(ddiv$near_phase_1, ddiv$near_phase_1 == "y", "1")
ddiv$near_phase_1 <- replace(ddiv$near_phase_1, ddiv$near_phase_1 == "n", ".")

ddiv$near_phase_2 <- replace(ddiv$near_phase_2, ddiv$near_phase_2 == "y", "2")
ddiv$near_phase_2 <- replace(ddiv$near_phase_2, ddiv$near_phase_2 == "n", ".")

ddiv$near_phase_3 <- replace(ddiv$near_phase_3, ddiv$near_phase_3 == "y", "3")
ddiv$near_phase_3 <- replace(ddiv$near_phase_3, ddiv$near_phase_3 == "n", ".")

ddiv$near_phase_4 <- replace(ddiv$near_phase_4, ddiv$near_phase_4 == "y", "4")
ddiv$near_phase_4 <- replace(ddiv$near_phase_4, ddiv$near_phase_4 == "n", ".")

ddiv$phase_sum <- paste(ddiv$near_phase_0, ddiv$near_phase_1, ddiv$near_phase_2, ddiv$near_phase_3, ddiv$near_phase_4, sep = "")

cphase <- c("0....", ".1...", "..2..", "...3.", "....4")
focal.parks <- c("Discovery Park", "Magnuson Park", "Carkeek Park", "Golden Gardens Park")

### AOV and Tukey test

d <- filter(ddiv, park %in% focal.parks, phase_sum %in% cphase, year == 2023)

phase_aov <- aov(S ~ phase_sum, data = d) 
TukeyHSD(phase_aov)

summary(d)

ggplot((d %>% group_by(phase_sum) %>% summarise(mean = mean(S))), aes(x = phase_sum, y = mean)) + 
  geom_col()

ggplot(d, aes(x = phase_sum, y = S)) +
  geom_boxplot(fill = bcs_colors["dark green"], color = bcs_colors["bright green"]) + geom_jitter(color = bcs_colors["bright green"])+
  theme_bcs()

### I don't know. I don't feel very convinced by this analysis approach. 

### I wonder if it would be better to calculate the overlap of each count circle with area of GSP phase?

ddiv <- dres %>%
  group_by(year, park, loop, station) %>%
  summarise(S = n_distinct(species), nsurv = n_distinct(survey_id), maps = sum(seen, heard, fly) / nsurv)


## join diversity data frame with gsp phase table

ddiv$lu <- paste0(ddiv$park, ddiv$loop, ddiv$station)
cod$lu <- paste0(cod$park, cod$loop, cod$station)
codlu <- select(cod, lu, code)

ddiv <- left_join(ddiv, codlu, join_by("lu" == "lu"))
ddiv <- left_join(ddiv, gsp.prop, join_by("code" == "code"))

view(ddiv)

## so let's try a mixed effect model for 2022 where there are fixed effects for prop gsp and rand effect for park

d <- filter(ddiv, park %in% focal.parks)

## I've been playing around with values for year, so far only found significant effects for 2022
prop.mixed <- lmer(S ~ prop_0 + prop_1 + prop_2 + prop_3 + prop_4 + (1 | park), data = filter(d, year == 2022))

summary(prop.mixed)

prop.mixed <- lmer(S ~ prop_4 + (1 | park), data = filter(ddiv, year == 2019))
summary(prop.mixed)


ggplot(filter(d, year == 2022), aes(x = prop_4, y = S)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)


