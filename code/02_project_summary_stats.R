#############################
### 02. nbp summary stats ###
#############################

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(readxl)

# Custom bcs plotting theme at "code/00_bcs_theme.R"----

bcs_colors <- c(
  "dark green" = "#0A3C23",
  "cream" = "#FAF5F0",
  "yellow green" = "#E6FF55",
  "peach" = "#FFB98C",
  "bright green" = "#36BA3A"
)

theme_bcs <- function() {
  theme(
    # Backgrounds
    panel.background = element_rect(fill = bcs_colors["cream"], color = NA),
    plot.background = element_rect(fill = bcs_colors["cream"], color = NA),
    panel.grid.major = element_line(color = bcs_colors["dark green"], linewidth = 0.5, linetype = "dotted"),
    panel.grid.minor = ggplot2::element_blank(),
    
    # Text
    text = element_text(color = bcs_colors["dark green"]),
    axis.text = element_text(color = bcs_colors["dark green"]),
    axis.title = element_text(color = bcs_colors["dark green"]),
    plot.title = element_text(color = bcs_colors["dark green"], face = "bold", size = 28),
    plot.subtitle = element_text(color = bcs_colors["dark green"], face = "italic", size = 22),
    plot.caption = element_text(color = bcs_colors["dark green"], size = 8),
    
    # Lines and borders
    axis.line = element_line(color = bcs_colors["dark green"]),
    axis.ticks = element_line(color = bcs_colors["dark green"]),
    panel.border = element_rect(color = bcs_colors["dark green"], fill = NA),
    
    # Legends
    legend.background = element_rect(fill = bcs_colors["cream"]),
    legend.key = element_rect(fill = bcs_colors["cream"]),
    legend.text = element_text(color = bcs_colors["dark green"]),
    legend.title = element_text(color = bcs_colors["dark green"]),
    
    # Facets
    strip.background = element_rect(fill = bcs_colors["yellow green"]),
    strip.text = element_text(color = bcs_colors["dark green"], face = "bold")
  )
}

# load tidied data (see script "01_tidy_raw_nbp.R" for details)
dat <- read_excel("data/b_intermediate_data/nbp_tidy_jan_24.xlsx")

# inspect data if desired
## str(dat)
## head(dat)


#############################
##### data completeness #####
#############################

# How complete is the data? do we have surveys for each loop for each month that we expect we would?
## What time range does the dataset cover?
range(dat$survey_date) # 1996-04-13 to 2024-01-20, so the datset should include 8 months for 1996;
                       # 12 months for 1997-2019; in 2020 we shut down NBP in march and 1 month for 2024 (noting that we shut down NBP in 2020)


## Create dataframe with year and number of months NBP was active in that year
dmths <- data.frame(year = sort(unique(dat$year)),
                    Nmonth = c(8, rep(12, 27), 1))

## Join months data frame with a summary table of how many loops each park had per year, and how many surveys
## were done for each loop at each park each year, then create a "completeness" column
dcmp <- dat %>%
  group_by(year, park) %>%
  summarise(Nloop = n_distinct(loop), nloop = n_distinct(paste0(survey_date, loop))) %>%
  ungroup() %>%
  left_join(., dmths) %>%
  mutate(completeness = nloop / Nloop / Nmonth)

## visualize with a heat map
pcmp <- ggplot(dcmp, aes(x = year, y = park, fill= completeness)) + 
  geom_tile() + 
  scale_fill_gradient(low = bcs_colors["yellow green"], high = bcs_colors["dark green"]) + 
  labs(x = "", y = "") +
  ggtitle("NBP data completeness") + 
  theme_bcs() 

pcmp

## print plot if desired
# pdf("figures/02_data_completeness.pdf", width = 11)
# print(pcmp)
# dev.off()


 #############################
 ####### survey summary ######
 #############################
 
## HOW MANHY SURVEYS IN DATASET?

length(unique(dat$survey_id))  # 39174 surveys BUT this includes the odd Magnuson surveys with non-existent station IDs
dat %>% filter(!is.na(station.code)) %>% summarise(nsurv = n_distinct(survey_id))  ## 38,771 surveys

## SURVEYS BY YEAR
### create dataframe with count of distinct surveys per year, then plot results
dyr <- dat %>%
  filter(!is.na(station.code)) %>%
  group_by(year) %>%
  summarise(nsurv = n_distinct(survey_id))

pyr <- ggplot(dyr, aes(x = year, y = nsurv)) +
  geom_col(fill = bcs_colors["dark green"]) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  xlab("") + ylab("Number of NBP point counts in dataset") + ggtitle("NBP point counts by year") +
  theme_bcs() +
  theme(panel.grid.major.x = element_blank())

pyr

### print plot to pdf if desired
# pdf("figures/02_nsurv_yr.pdf")
# print(pyr)
# dev.off()

## SURVEYS BY LOCATION
### create dataframe with count of distinct surveys by park, then plot results
dprk <- dat %>%
  filter(!is.na(station.code)) %>%
  group_by(park) %>%
  summarise(nsurv = n_distinct(survey_id)) %>%
  ungroup() %>%
  mutate(park = fct_reorder(park, nsurv))

pprk <- ggplot(dprk, aes(x = park, y = nsurv)) +
  geom_col(fill = bcs_colors["dark green"]) +
  coord_flip() +
  xlab("") + ylab("Number of NBP point counts in dataset") + ggtitle("NBP point counts by park") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bcs() +
  theme(panel.grid.major.y = element_blank())

pprk

#### print chart to pdf if desired
# pdf("figures/02_nsurv_park.pdf", width = 11, height = 7)
# print(pprk)
# dev.off()

#############################
###### species summary ######
#############################

# SPECIES TOTAL
## remove observations not resolved to species level and remove hybrids
dsp <- dat %>% filter(!str_detect(species, pattern = " sp.| x "))
sort(unique(dsp$species)) # 212 species

## FREQUENTLY REPORTED SPECIES
### step 1, create species response matrix for each unique survey
dfreq <- dat %>% 
  mutate(observed = seen + heard + fly) %>%
  select(survey_id, species, observed) %>%
  pivot_wider(names_from = species, values_from = observed)

### step 2: create proportion data
d <- colnames(dfreq[, -1]) ## vector of the species names
times <- unname(apply(X = !is.na(dfreq[, -1]), MARGIN = 2, sum)) ## vector of the number of times each species has been reported

#### add names and number of reports to a data frame, then create a new column dividing number of surveys reporting by total surveys
dprop <- data.frame(species = d, 
                    n_reported = times)
dprop$prop_reported <- dprop$n_reported / length(unique(dfreq$survey_id))

#### reorder species factor levels to make plot more appealing
dprop$species <- fct_reorder(dprop$species, dprop$prop_reported)

#### plot top 20 most frequenlty reported species
pfreq <- ggplot(slice_max(dprop, prop_reported, n = 20), aes(x = species, y = prop_reported)) +
  geom_col(fill = bcs_colors["dark green"]) +
  coord_flip() + 
  xlab("") + ylab("Propotion of surveys reporting") + ggtitle("Most frequently reported species on NBP counts") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bcs() +
  theme(panel.grid.major.y = element_blank())

pfreq

#### print plot if desired
# pdf("figures/02_prop_top_20.pdf", width = 11, height = 7)
# print(pfreq)
# dev.off()

# NOT ID'D TO SPECIES LEVEL
## create dataframe with just unresolved species, then count how many times each sp. occurs in the dataset
dunr <- dat %>% filter(str_detect(species, pattern = " sp.")) %>%
  group_by(species) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(species = fct_reorder(species, n))

punr <- ggplot(dunr, aes(x = species, y = n)) + 
  geom_col(fill = bcs_colors["dark green"]) +
  coord_flip() +
  xlab("") + ylab("Number of times reported") + ggtitle("NBP observations not identified to species level") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     breaks = c(250, 500, 750, 1000, 1250, 1500)) +
  theme_bcs() +
  theme(panel.grid.major.y = element_blank())

punr

### print plot
# pdf("figures/02_sp._reports.pdf", width = 11, height = 7)
# print(punr)
# dev.off()

# ABUNDANCE

## Which species have the highest mean abundance per survey (maps)
dabund <- dat %>%
  group_by(species) %>%
  summarise(nobs = sum(seen, heard, fly), nsurv = n_distinct(survey_id), maps_n = nobs / nsurv,
            maps_N = nobs / length(unique(dat$survey_id))) %>%
  ungroup() %>%
  mutate(species_n = fct_reorder(species, maps_n), species_N = fct_reorder(species, maps_N))

pmaps_n <- ggplot(slice_max(dabund, maps_n, n = 20), aes(x = species_n, y = maps_n)) +
  geom_col(fill = bcs_colors["dark green"]) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "Mean abundance per survey when observed", title = "Mean abundance per survey when observed") +
  theme_bcs() +
  theme(panel.grid.major.y = element_blank())

pmaps_n  ## this stat is driven by bird group size, but species may be very infrequently observed (e.g., western bluebird)

pmaps_N <- ggplot(slice_max(dabund, maps_N, n = 20), aes(x = species_N, y = maps_N)) +
  geom_col(fill = bcs_colors["dark green"]) +
  coord_flip() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "Mean abundance per survey", title = "Mean abundance per survey") +
  theme_bcs() +
  theme(panel.grid.major.y = element_blank())

pmaps_N  ## this stat is largely driven by frequency of reports

# Visualizing mean abundance per survey by species across years
## use mean abundance per survey N, stat mostly driven by observation frequency rather than group size
### new dataframe calculating number of surveys conducted each year
dNsurvyr <- dat %>%
  group_by(year) %>%
  summarise(Nsurv = n_distinct(survey_id))

### new data frame with mean abundance per survey per year
dmapsN <- dat %>%
  group_by(year, species) %>%
  summarise(nobs = sum(seen, fly, heard)) %>%
  ungroup() %>%
  left_join(., dNsurvyr) %>%
  mutate(maps_N = nobs / Nsurv) %>%
  select(year, species, maps_N) %>%
  pivot_wider(names_from = species, values_from = maps_N) %>%
  replace(is.na(.), 0) %>% 
  pivot_longer(-1, names_to = "species", values_to = "maps_N")

### Visualize on heat map with sets of random species or by key word

spp <- d[sample(1:length(d), 20)]  # this will return 20 random
# spp <- d[str_detect(d, "Warbler")]  # choose a species or group of birds that share a common work (e.g, warbler, thrush, swallow)
# spp <- c("CHOOSE YOUR OWN")

ggplot(filter(dmapsN, species %in% spp), aes(x = year, y = species, fill = maps_N)) +
  geom_tile() +
  scale_fill_gradient(low = bcs_colors["yellow green"], high = bcs_colors["dark green"]) + 
  labs(x = "", y = "") +
  ggtitle("NBP observations") + 
  theme_bcs()

# SPECIES TOTALS BY LOCATION
dS <- dat %>%
  filter(!str_detect(species, pattern = " sp.| x ",),  ## filter out sp and hybrids
         species != "Spotted Owl") %>%  ## truely don't think we saw the spotted owl at Magnuson
  group_by(park) %>% 
  summarise(S = n_distinct(species)) %>%
  ungroup() %>%
#  left_join(., active) %>%    ## could create a join to label parks by their current NBP status (active vs inactive)
#  replace(is.na(.), "Not Active") %>% 
  mutate(park = fct_reorder(park, S))

meanS <- mean(dS$S)  # average species reported by park

pmeanS <- ggplot(dS, aes(x = park, y = S)) +
  geom_col(fill = bcs_colors["dark green"]) + 
  ylab("Total species reported") + xlab("") + ggtitle("NBP total species reported") +
  geom_hline(yintercept = mean(dS$S), color = bcs_colors["bright green"], lty = 2) +
  geom_text(aes(label = S), color = bcs_colors["cream"], hjust = 1.4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     breaks = c(75, 150)) + 
  coord_flip() + 
  annotate("text", x = 1, y = 107, label = paste("Avg =", round(meanS, 0), "species"), color = bcs_colors["dark green"]) +
  theme_bcs() +
  theme(panel.grid.major.y = element_blank())

pmeanS

## print plot if desired
# pdf("figures/02_mean_S_by_park.pdf", width = 11)
# print(pmeanS)
# dev.off()


########## END ###########