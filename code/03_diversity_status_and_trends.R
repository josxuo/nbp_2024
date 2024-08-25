#######################################
### 03. diversity status and trends ###
#######################################

##################
### BACKGROUND ###
##################

## in 2014, C.J. Battey and Toby Ross published "Impacts of Habitat Restoration
## and the Status of Avian Communities in Seattle City Parks" using data from 
## Birds Connect Seattle's (formerly Seattle Audubon) Neighborhood Bird Project.
## the report is available at  https://birdsconnectsea.org/wp-content/uploads/2021/02/Seattle_Audubon_NBP_Report_FINAL.pdf
## Now in 2024, 10 years later, we are running similar analyses.

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(readxl)

# Custom bcs plotting theme at "code/00_bcs_theme.R"

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

# define bird groups
## invasive species
invas <- c("European Starling", "Eurasian Collared-Dove", "House Sparrow")

## human-associated species
humasc <- c("American Crow", "Rock Pigeon", "European Starling", "House Sparrow")

## riparian species
ripar <- c("Orange-crowned Warbler", "Wilson's Warbler", "Song Sparrow", "Belted Kingfisher",
           "Yellow Warbler", "Common Yellowthroat")

## warblers
warb <- c("Orange-crowned Warbler", "Wilson's Warbler", "Yellow Warbler", 
          "Common Yellowthroat", "Black-throated Gray Warbler", "MacGillivray's Warbler",
          "Hermit Warbler", "Townsend's Warbler", "Yellow-rumped Warbler")

## woodpeckers
wood <- c("Hairy Woodpecker", "Downy Woodpecker", "Pileated Woodpecker", "Northern Flicker")

# Define active NBP sites
status <- data.frame(park = sort(unique(dat$park)), 
                     status = c("inactive", "active", "active", "inactive", "active",
                                "active", "active", "inactive", "active", "active",
                                "active", "active", "inactive", "inactive", "inactive",
                                "active"))
## check you got it right
# status  ## looks good. Let's join it to data.
dat <- left_join(dat, status)

############################################
### STATUS AND TRENDS IN AVIAN DIVERSITY ### 
############################################

# Battey and Ross report 232 species over a 17 year timespan (1996-2013?). What do we have?

## Filtering just for year less than 2014
# filter(dat, year < 2014) %>% summarise(S = n_distinct(species)) ## 238 distinct, but this includes sp. records and hybirds

## Filtering out sp. records takes us down to 207 species
# filter(dat, year < 2014, !str_detect(species, " sp.")) %>% summarise(S = n_distinct(species))

## leaving in sp. records but removing hybrids? Gives 236
# filter(dat, year < 2014, !str_detect(species, " x ")) %>% summarise(S = n_distinct(species))

## leaving in sp. records but removing hybrids and year less than 2013 Gives 232
# filter(dat, year < 2013, !str_detect(species, " x ")) %>% summarise(S = n_distinct(species))

### let's check the other stats to see if these filters track. 
### they report 207 at Discovery, 169 at Magnuson, and 123 at Golden Gardens
# filter(dat, year < 2013 & !str_detect(species, " x ")) %>% 
  # group_by(park) %>%
  # summarise(S = n_distinct(species))  ## nope, different numbers

### let's upload the raw data--I realize the taxonomy has changed for a few species.
# raw <- read_excel("data/a_raw_data/nbp_raw_jan_24.xlsx")
# raw <- left_join(raw, status, join_by("Park" == "park"))

# filter(raw, Year < 2013, status == "active", !str_detect(Species, " x ")) %>% summarise(S = n_distinct(Species))

# filter(raw, Year < 2015, status == "active") %>% group_by(Park) %>% summarise(S = n_distinct(Species)) ## still can't replicate the numbers.

### it is unclear how our data sets differ. I suspect Battey and Ross may have inlcuded sp. records and hybrids.
### it is my preference to exclude them from the current analysis.

## remove raw data from environment
# rm(raw)

## remove sp. records and hybrids from data frame, remove incomplete years (1996, 2020, 2024)

dres <- dat %>% filter(!str_detect(species, pattern = " sp."), !str_detect(species, pattern = " x "),
                       !year %in% c(1996, 2020, 2024))

# Total species
paste("NBP surveys recorded", n_distinct(dres$species), "species in Seattle City Parks over a", 
      max(dres$year) - min(dres$year), "year timespan.") # 212 species

### Diversity by parks
dSprk <- dres %>% group_by(park) %>% summarise(S = n_distinct(species)) %>% ungroup() %>% 
  mutate(park = fct_reorder(park, S)) %>%
  arrange(desc(S))

pSprk <- ggplot(slice_max(dSprk, S, n = 4), aes(x = park, y = S)) +
  geom_col(fill = bcs_colors["dark green"]) +
  labs(x = "", y = "Species richness", title = "Species richness by park") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bcs() + 
  theme(panel.grid.major.x = element_blank())

pSprk

#### NOTE the figure above replicates the chart from Battey and Ross, however, we
#### should report on all active NBP sites.

dSprk <- left_join(dSprk, status) %>%
  mutate(park = fct_reorder(park, S))

pSprks <- ggplot(filter(dSprk, status == "active"), aes(x = park, y = S)) +
  geom_col(fill = bcs_colors["dark green"]) +
  coord_flip() + 
  labs(x = "", y = "Species richness", title = "Species richness by park") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bcs() + 
  theme(panel.grid.major.y = element_blank())

pSprks  # version of this plot already made in 02_project_summary_stats.R (figure/02_mean_S_by_park)

## print plot if desired
# pdf("figures/03_total_S_by_Park.pdf")
# print(pSprks)
# dev.off()

## Battey and Ross report on the mean annuall species diversity accross all parks (130)
## and by individual parks (high of 87 at Magnuson and low of 48 at Golden Gardens)

dSyr <- filter(dres, status == "active") %>% group_by(year) %>%
  summarise(S = n_distinct(species))

mean(dSyr$S) # Mean annual species diversity = 125

dSyrprk <- dres %>% group_by(year, park) %>%
  summarise(S = n_distinct(species)) %>%
  ungroup() %>%
  group_by(park) %>%
  summarise(meanS = mean(S)) %>%
  arrange(desc(meanS))

dSyrprk  ## Discovery 105, Mag 86, Seward 65

### Trends in mean annual species diversity
#### All parks. Note, that Discovery park was added to the project in 2003, which
#### significantly increased the diversity observed. Seward data appears in 2004.
#### Arboretum added 2005. Lincoln Parks added in 2017
#### and Cheasty Greenspace in 2021. Many early locations were dropped after 1999 or 2002.
#### for the purpose of this analysis, it makes sense to focus just on active parks
#### that have data from 2005 to 2023. 

####### Find the year range of data present for each NBP location
parks <- data.frame(park = sort(unique(dres$park)),
                    fstyr = numeric(length = 16),
                    lstyr = numeric(length = 16))

for(i in 1:length(parks$park)) {
  parks[i, 2] <- min(dres[dres$park == parks[i, 1], ]$year)
  parks[i, 3] <- max(dres[dres$park == parks[i, 1], ]$year)
}

parks %>% arrange(fstyr)
####### focal parks just those with data from 2005 through 2023
focal_parks <- parks[parks$fstyr <= 2005 & parks$lstyr == 2023, ]$park

####### Redo species richness calculation for this subset

dSyr <- filter(dres, park %in% focal_parks) %>% 
  group_by(year) %>%
  summarise(S = n_distinct(species))

mean(dSyr$S) # Mean annual species diversity = 125 -- basically same as before

pSyr <- ggplot(filter(dSyr, year >= 2005), aes(x = year, y = S)) + 
  geom_point(fill = bcs_colors["dark green"]) +
  geom_smooth(method = "lm", se = FALSE, color = bcs_colors["bright green"]) +
  labs(x = "", y = "Total species reported", title = "Species reported",
       subtitle = "from eight NBP locations") +
  theme_bcs()

pSyr

Strend <- lm(S ~ year, data = filter(dSyr, year >= 2005))
summary(Strend)  ## statistically significant negative trend with simple linear model, R2 = .50

## Trends by park
dSyrprk <- dres %>% 
  filter(park %in% focal_parks, year >= 2005) %>%
  group_by(year, park) %>%
  summarise(S = n_distinct(species))

pSyrprk <- ggplot(dSyrprk, aes(x = year, y = S)) +
  geom_point(fill = bcs_colors["dark green"]) +
  geom_smooth(method = "lm", color = bcs_colors["bright green"], se = FALSE) +
  labs(x = "", y = "Total species reported") +
  facet_wrap(~ park, scales = "free") + 
  theme_bcs()

pSyrprk

## Calculate values for suggestive trends using simple linear models
Strends <- data.frame(park = focal_parks,
                      trend = numeric(length = length(focal_parks)),
                      pval = numeric(length = length(focal_parks)),
                      r2 = numeric(length = length(focal_parks)))

for(i in 1:length(focal_parks)) {
  mod <- summary(lm(S ~ year, data = dSyrprk[dSyrprk$park == focal_parks[i], ]))
  Strends[i, 2] <- round(mod$coefficients[2, 1], 4)
  Strends[i, 3] <- round(mod$coefficients[2, 4], 4)
  Strends[i, 4] <- round(mod$r.squared, 4)
}

Strends ## suggestive trends stat significant for Disco, Genesee, Golden Gardens
        ## Lake Forest Park, and Magnuson



### mean per-survey abundance

##### data frame with total observations of each species for each year, 
##### also add 0 values where species not observed in a given year
dabundyr <- dres %>% group_by(year, species) %>% 
  summarise(abund = sum(seen, heard, fly)) %>%
  pivot_wider(names_from = species, values_from = abund) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(-1, names_to = "species", values_to = "abund")

##### data frame with total number of surveys done for each year
dnsurvyr <- dres %>% group_by(year) %>%
  summarise(nsurv = n_distinct(survey_id))

##### joining abundance and survey totals data and calculating mean abundance per survey "maps" column
dabundyr <- left_join(dabundyr, dnsurvyr) %>%
  mutate(maps = abund / nsurv)

sp <- sort(unique(dabundyr$species))
coeff <- numeric(length = length(sp))

for(i in 1:length(sp)) {
  # Subset the data for the current species
  d <- dabundyr[dabundyr$species == sp[i], ]
  
  # Check if there are enough data points to fit a model
  if (nrow(d) > 1) {  # Need at least two points to fit a linear model
    # Fit the linear model (assuming you want to predict 'y' from 'x')
    model <- lm(maps ~ year, data = d)
    
    # Extract the coefficient for 'x'
    coeff[i] <- coef(model)["year"]
  } else {
    # If not enough data, store NA (or another indicator of missing value)
    coeff[i] <- NA
  }
  
  # Optionally print progress
  print(paste("Processed species", sp[i], "with coefficient", coeff[i]))
}

maps <- data.frame(species = sp,
                   coef = coeff) %>%
  mutate(species = fct_reorder(species, coef))

sp[sample(1:212, 25)]

ggplot(filter(maps, species %in% sp[sample(1:212, 25)]), aes(x = species, y = coef)) + 
  geom_col(aes(fill = coef)) +
  scale_fill_gradient(low = bcs_colors["yellow green"], high = bcs_colors["dark green"]) +
  labs(title = "Average rate of change", x = "", y = "Rate", subtitle = "in mean abundance per survey") +
  theme_bcs() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## Battey and Ross limit to species with mean annual detection > 10 and mean rates
## greater than 0.0075

dspsurv <- dres %>% group_by(year, species, survey_id) %>%
  summarise(obs = sum(seen, heard, fly)) %>%
  pivot_wider(names_from = species, values_from = obs) %>%
  replace(is.na(.), 0)

ddet <- data.frame(species = colnames(dspsurv[-c(1, 2, 3)]), 
                   detperyr = (unname(apply(dspsurv[, -c(1, 2, 3)], MARGIN = 2, 
                                           FUN = sum))/26))
ddet$detperyr > 10
sum(abs(dmapsdet$coef) > .0075)

dmapsdet <- left_join(maps, ddet)

short <- filter(dmapsdet, detperyr > 10, abs(coef) > 0.005) %>%
  mutate(species = fct_reorder(species, -coef))

pdeltmaps <- ggplot(short, aes(x = species, y = coef)) + 
  geom_col(aes(fill = coef)) +
  scale_fill_gradient(low = bcs_colors["yellow green"], high = bcs_colors["dark green"]) +
  labs(title = "Average rate of change", x = "", y = "Rate", subtitle = "in mean abundance per survey") +
  theme_bcs() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1),
        legend.position = "none")

pdeltmaps

## I changed the filtering parameters relative to Battey and Ross--used rate of .005 rather
## than .0075. 

## print plot if desired
# pdf("figures/03_delta_maps.pdf", width = 8)
# print(pdeltmaps)
# dev.off()

## 

#### END #### 