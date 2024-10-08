#######################################
### 03. diversity status and trends ###
#######################################

##################
### BACKGROUND ###-----
##################

## in 2014, C.J. Battey and Toby Ross published "Impacts of Habitat Restoration
## and the Status of Avian Communities in Seattle City Parks" using data from 
## Birds Connect Seattle's (formerly Seattle Audubon) Neighborhood Bird Project.
## the report is available at  https://birdsconnectsea.org/wp-content/uploads/2021/02/Seattle_Audubon_NBP_Report_FINAL.pdf
## Now in 2024, 10 years later, we are running similar analyses.

# clear environment
rm(list = ls())

###################################
# Packages, themes, and functions #----
###################################

#load packages

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

# function for extracting overall p-value from model
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
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

#########################
# data and focal groups #----
#########################

# load tidied data (see script "01_tidy_raw_nbp.R" for details)
dat <- read_excel("data/b_intermediate_data/nbp_tidy_jan_24.xlsx")

# Define active NBP sites
status <- data.frame(park = c("Bliner Property", "Carkeek Park", "Cheasty Greenspace", "Clark Lake Park", "Discovery Park", "Genesee Park",
                              "Golden Gardens Park", "Jenkin's Creek Park", "Lake Forest Park", "Lincoln Park", "Magnsuon Park", "Seward Park",
                              "Shadow Lake Bog", "Soos Creek", "Walsh Property", "Washington Park Arboretum"), 
                     status = c("inactive", "active", "active", "inactive", "active",
                                "active", "active", "inactive", "active", "active",
                                "active", "active", "inactive", "inactive", "inactive",
                                "active"))
## check you got it right
status  ## looks good. Let's join it to data.
dat <- left_join(dat, status)

# inspect data if desired
## str(dat)
## head(dat)

# define focal bird groups
## invasive species
invas <- c("European Starling", "Eurasian Collared-Dove", "House Sparrow", "Rock Pigeon")

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


############################################
### STATUS AND TRENDS IN AVIAN DIVERSITY ### ----
############################################

# ATTEMPTING TO REPLICATE SPECIES RICHNESS REPORTED IN BATTY AND ROSS

# Battey and Ross report 232 species over a 17 year time span (1996-2013?). What do we have?

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
### it is my preference to exclude sp. records from the current analysis.

## remove raw data from environment
# rm(raw)

# MOVING ON
## filter data for observations "resolved" to species level (dres)
dres <- dat %>% filter(!str_detect(species, pattern = " sp."),  ## this leaves in hybrids
                       #!year %in% c(1996, 2020, 2021, 2024),   ## will need to remove years for which there are very incomplete data later
                       !is.na(station.code), ## remove the odd stations like MF56, whatever that is
                       species != "Spotted Owl") %>%  ## there's no way we saw a spotted owl at Magnuson
  mutate(obs = seen + heard + fly)  ## combine counts of seen heard fly



# Total species
paste("NBP surveys recorded", n_distinct(dres$species), "species in Seattle City Parks over a", 
      max(dres$year) - min(dres$year), "year timespan.") # 213 including hybrids excluding spotted owl

# write species list to csv file for reporting, include the number of years the species was recorded and the number of sites
# the species was recorded at
species.list <- dres %>% group_by(species) %>%
 reframe(count = sum(seen, heard, fly), yrs = n_distinct(year), sites = n_distinct(station.code))

write.csv(species.list, "results/species_list.csv", row.names = FALSE)

## This is a summary of the entire dataset. For trend analysis, we need to compare sites that have been consistently monitored over the same time frame

## select years
years <- c(2005:2019, 2022, 2023) ## this excludes years with large data gaps
months <- c(2)
units <- complete(years, months, data = dres)  # previously found may had the greatest number of surveys across years and sites

## create data frame aggregating total species observed each year & total count of birds each year for given month(s)
S <- dres %>% 
  # filter just for those circles with consistent monitoring over the years/months selected
  filter(station.code %in% units, month %in% months, year %in% years) %>%
  
  # aggregate number of species reported and bird counts by count circle and year
  group_by(station.code, year) %>%
  reframe(yearS = n_distinct(species), count = sum(obs)) %>%
  
  # scale year so it starts at 1 for ease in use with models later
  mutate(scaleyr = year - min(year) + 1)

## create data frame with average species detected and mean abundance per survey
meanS <- dres %>% 
  # filter just for those circles with consistent monitoring over the years/months selected
  filter(station.code %in% units, month %in% months, year %in% years) %>%
  
  # aggregate number of species and individuals reported during each individual survey (keep code and year for use later)
  group_by(station.code, year, survey_id) %>%
  reframe(nspecies = n_distinct(species), count = sum(obs)) %>%
  
  # group by count circle and year and find mean number of species reported by survey and mean abundance per survey
  group_by(station.code, year) %>%
  reframe(meanS = mean(nspecies), maps = mean(count))

## join data frames
S <- left_join(S, meanS)

## Plot richness over time
ggplot(S, aes(x = year, y = yearS)) + 
  geom_point() +
  labs(x = "Year", y = "Species richness", title = "Total species reported", 
       subtitle = paste("at sites consistently monitored during month(s)", paste(months, collapse = ", "))) +
  geom_smooth(method = "lm", se = FALSE, color = bcs_colors["bright green"]) +
  theme_bcs()

## Summary of model of log of species richness vs scaled year
summary(lm(log(yearS) ~ scaleyr, data = S))


## Plot mean abundance per survey over time
ggplot(S, aes(x = year, y = log(maps))) +
  geom_point() +
  labs(x = "Year", y = "Log of mean abundance per survey", title = "Mean abundance per survey", 
       subtitle = paste("at sites consistently monitored during month(s)", paste(months, collapse = ", "))) +
  geom_smooth(method = "lm", se = FALSE, color = bcs_colors["bright green"]) +
  theme_bcs()

## Summary of model of log of mean abund per survey vs scaled year
summary(lm(log(maps) ~ scaleyr, data = S))


## No significant trend in winter richness or abundance
## Spring richness and abundance are both trending down
## Possible decline in richness and abundance in summer
## No significant trend in fall richness / abundance

## January: no trend
## February: no trend
## March: march abundance and richness declining
## April: possibly richness declining (p < .1), abundance declining
## May: possibly richness declining (p < .1), abundance declining
## June: abundance and richness declining
## July: abundance declining; no trend richness
## August: richness declining; no trend abundance
## September: abundance declining; no trend richness 
## October: no trends
## November: abundance possibly declining (p < .1); no trend richness
## December: no trends


### Trend mean abundance per survey for each species

## select years
years <- c(2004:2019,2022, 2023) 
months <- c(7)
spp <- c("Song Sparrow", "American Robin", "Northern Flicker", "Bufflehead", "Mallard", "Dark-eyed Junco")

units <- complete(years, months, data = dres)  # previously found may had the greatest number of surveys across years and sites

## create data frame aggregating total species observed each year & total count of birds each year for given month(s)

sp.trends <- dres %>% filter(code %in% units, month %in% months, year %in% years, species %in% spp) %>%
  group_by(year, species) %>%
  reframe(count = sum(obs)) %>%
  mutate(scaleyr = year - min(year) + 1)

sp.trends
view(sp.trends)

## create data frame with average species detected and mean abundance per survey
mean.sp.trends <- dres %>% filter(code %in% units, month %in% months, year %in% years) %>%
  group_by(year, species) %>%
  reframe(maps = mean(obs)) %>%
  filter(species %in% spp)

mean.sp.trends

## join data frames
sp.trends <- left_join(sp.trends, mean.sp.trends) %>% select(year, species, count, maps, scaleyr)

ggplot(sp.trends, aes(x = year, y = log(count))) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = bcs_colors["bright green"]) +
  facet_wrap(~species, scales = "free") +
  theme_bcs()

## Create a data frame that assesses the trend in abundance for each species at sites consistently
## monitored across a given set of years. This assesses trends in each month separately. 

# create the empty data frame to hold values
res <- data.frame(species = character(),
                  month = numeric(),
                  trend = numeric(), 
                  pval = numeric())

# Create list of species to assess
spp <- sort(unique(dres$species))

## this filters for a particular set of units in a particular month, and then sums all the observations for that year. 
## There are the same number of surveys at the same sites each year, so values are comparable year over year 


## NOTE: Running this loop will take a few mins
for(i in 1:length(spp)){
  for(j in 1:12){
    units <- complete(years = years, months = j, data = dres)
    d <- dres %>% filter(year %in% years, month == j, species == spp[i], station.code %in% units) %>%
      group_by(year, species) %>% reframe(count = sum(obs)) %>% mutate(scaleyr = year - min(year) + 1)
    
    if(dim(d)[1] <= 1){
      res[(j + (i-1) * 12), 1] <- spp[i]
      res[(j + (i-1) * 12), 2] <- j
      res[(j + (i-1) * 12), 3] <- NA
      res[(j + (i-1) * 12), 4] <- NA
    }
    else{mod <- lm(log(count) ~ scaleyr, data = d)
      res[(j + (i-1) * 12), 1] <- spp[i]
      res[(j + (i-1) * 12), 2] <- j
      res[(j + (i-1) * 12), 3] <- ifelse(is.numeric(coef(mod)[2]), unname(coef(mod)[2], NA))
      res[(j + (i-1) * 12), 4] <- ifelse(is.numeric(overall_p(mod)), overall_p(mod), NA)
    }
  }
}

# this will plot all statistically significant monthly trends for each species. 1 on y axis = no change. 


ylims <- exp(range(na.omit(res[res$pval < 0.05, ]$trend)))

par(mfrow = c(4, 4))

for(i in 1:length(spp)){
  pd <- res[res$species == spp[i] & res$pval < 0.1, ]
  plot(pd$month, exp(pd$trend), xlim = c(1, 12), ylim = ylims, main = spp[i], xlab = "", ylab = "")
  abline(h = 1, col = "red")
}

## Looks like we might have something to say about American Goldfinch, American Robin, Anna's Hummingbird,
## Dark-eyed Junco, European Starling, Glaucous-winged Gull, Gadwall, House Sparrow, House Finch, Killdeer,
## Northern Flicker, Red-winged Blackbird, Savannah Sparrow...

## Diversity by sites

## table with richness and number of years surveyed and number of surveys by site

dSsite <- dres %>% group_by(code) %>% reframe(S = n_distinct(species), nyears = n_distinct(year), nsurv = n_distinct(survey_id)) %>%
  mutate(code = fct_reorder(code, S)) %>%
  arrange(desc(S))

## table with mean richness per survey
dmeanSsite <- dres %>% group_by(code, year) %>% reframe(S = n_distinct(species)) %>%
  group_by(code) %>% reframe(meanS = mean(S)) %>%
  mutate(code= fct_reorder(code, meanS)) %>%
  arrange(desc(meanS))

## join tables
dSsite <- left_join(dSsite, dmeanSsite) %>% select(code, S, meanS, everything())

## Write site-level diversity results to csv
# write.csv(dSsite, "results/site-level_richness.csv", row.names = FALSE)

## Map with mean richness per survey
library(leaflet)
utms <- read_xlsx("data/c_analysis_data/circ_traffic_flow.xlsx")
utms <- left_join(utms, dSsite, join_by("Station" == "code"))

range(na.omit(utms$meanS))

pal <- colorNumeric(palette = "Reds", domain = range(utms$meanS), reverse = FALSE)
map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = utms, lng = ~Longitude, lat = ~ Latitude, color = ~pal(meanS), popup = ~paste("Station:", Station, "<br>", "Mean number of species reported per survey:", round(meanS, 0)), radius = 3)

map

## most species on a single survey
dres %>%
  group_by(survey_id) %>%
  reframe(S = n_distinct(species)) %>%
  mutate(survey_id2 = fct_reorder(survey_id, S)) %>%
  arrange(desc(S))

# MWL3 25 species on a single five minute survey, 2x!

## Seasonality. This will create relative abundance charts for each species by month
abun.mon <- dres %>%
  group_by(month, species) %>%
  reframe(abun = sum(obs))

abun.mon

nsurv.mon <- dres %>%
  group_by(month) %>%
  reframe(nsurv = n_distinct(survey_id))

maps.mon <- left_join(abun.mon, nsurv.mon) %>% mutate(maps = abun / nsurv)

maps.mon

sp.list <- sort(unique(maps.mon$species))

par(mfrow = c(4, 4))
for(i in 1:length(sp.list)){
  plot(maps.mon[maps.mon$species == sp.list[i], ]$month, maps.mon[maps.mon$species == sp.list[i], ]$maps, 
       xlab = "", ylab = "", main = sp.list[i], type = "h", lwd = 5, col = bcs_colors["dark green"],
       xlim = c(1, 12))
}


### Diversity by parks.
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

## Battey and Ross report on the mean annual species diversity across all parks (130)
## and by individual parks (high of 87 at Magnuson and low of 48 at Golden Gardens)

dSyr <- filter(dres, status == "active") %>% group_by(year) %>%
  summarise(S = n_distinct(species))

mean(dSyr$S) # mean annual diversity at active parks

dSyrprk <- dres %>% group_by(year, park) %>%
  summarise(S = n_distinct(species)) %>%
  ungroup() %>%
  group_by(park) %>%
  summarise(meanS = mean(S)) %>%
  arrange(desc(meanS))

dSyrprk  

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
focal_parks <- parks[parks$fstyr <= 2005 & parks$lstyr >= 2023, ]$park

####### Redo species richness calculation for this subset

dSyr <- filter(dres, park %in% focal_parks) %>% 
  group_by(year) %>%
  summarise(S = n_distinct(species)) %>%
  mutate(scaleyr = year - min(year) + 1)

mean(dSyr$S) #

pSyr <- ggplot(filter(dSyr, year %in% c(2005:2019, 2022, 2023)), aes(x = year, y = log(S))) + 
  geom_point(fill = bcs_colors["dark green"]) +
  geom_smooth(method = "lm", se = FALSE, color = bcs_colors["bright green"]) +
  labs(x = "", y = "Log of total species reported", title = "Trend in species reported over time",
       subtitle = "from eight NBP locations") +
  theme_bcs()

pSyr

# pdf("figures/03_species_richness_trend.pdf", width = 8)
# print(pSyr)
# dev.off()

dSyr

Strend <- lm(log(S) ~ scaleyr, data = filter(dSyr, year %in% c(2005:2019, 2022, 2023)))
summary(Strend)  ## Statistically significant p < 0.01

## Trends by park
dSyrprk <- dres %>% 
  filter(park %in% focal_parks, year %in% c(2005:2019, 2022, 2023)) %>%
  group_by(year, park) %>%
  reframe(S = n_distinct(species)) %>%
  mutate(scaleyr = year - min(year) + 1)
  

pSyrprk <- ggplot(dSyrprk, aes(x = year, y = log(S))) +
  geom_point(fill = bcs_colors["dark green"]) +
  geom_smooth(method = "lm", color = bcs_colors["bright green"], se = FALSE) +
  labs(x = "", y = "Log total species reported") +
  facet_wrap(~ park, scales = "free") + 
  theme_bcs()

pSyrprk

## Calculate values for suggestive trends using simple linear models
Strends <- data.frame(park = character(),
                      trend = numeric(),
                      pval = numeric(),
                      r2 = numeric())

for(i in 1:length(focal_parks)) {
  mod <- summary(lm(log(S) ~ scaleyr, data = dSyrprk[dSyrprk$park == focal_parks[i], ]))
  Strends[i, 1] <- focal_parks[i]
  Strends[i, 2] <- round(mod$coefficients[2, 1], 4)
  Strends[i, 3] <- round(mod$coefficients[2, 4], 4)
  Strends[i, 4] <- round(mod$r.squared, 4)
}

Strends ## suggestive trends stat significant for Disco, possibly Genesee, Golden Gardens
        ## possibly Lake Forest Park, possibly Magnuson, possibly Seward


### mean per-survey abundance. NOTE THIS ANALYSIS DOESN'T CONTROL FOR CONSISTENT SITE MONITORING ACROSS YEARS. 
### Preference would be to use analysis of species trends by month earlier in this script.

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
  labs(title = "Average rate of change", x = "", y = "Rate", subtitle = "in mean abundance per survey (2005-2023)") +
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

## focal groups
dabundyr <- dres %>% filter(year >= 2005) %>%
  mutate(focal.group = case_when(species %in% humasc ~ "human associated",
                                      species %in% wood ~ "woodpecker",
                                      TRUE ~ "other")) %>%
  group_by(year, focal.group) %>% 
  summarise(abund = sum(seen, heard, fly)) %>%
  pivot_wider(names_from = focal.group, values_from = abund) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(-1, names_to = "focal.group", values_to = "abund")

##### data frame with total number of surveys done for each year
dnsurvyr <- dres %>% group_by(year) %>%
  summarise(nsurv = n_distinct(survey_id))

##### joining abundance and survey totals data and calculating mean abundance per survey "maps" column
dabundyr <- left_join(dabundyr, dnsurvyr) %>%
  mutate(maps = abund / nsurv)

pfocal <- ggplot(filter(dabundyr, focal.group != "other"), aes(x = year, y = maps)) +
  geom_point(fill = bcs_colors["dark green"]) +
  geom_smooth(method = "lm", se = FALSE, color = bcs_colors["bright green"]) +
  facet_wrap(~ focal.group, scales = "free") + 
  theme_bcs()

pfocal

group <- sort(unique(dabundyr$focal.group))
coeff <- numeric(length = length(group))
rsq <- numeric(length = length(group))
pval <- numeric(length = length(group))

for(i in 1:length(group)) {
  # Subset the data for the current species
  d <- dabundyr[dabundyr$focal.group == group[i], ]
  model <- lm(maps ~ year, data = d)
  coeff[i] <- coef(model)["year"]
  rsq[i] <- summary(model)$r.squared
  pval[i] <- overall_p(model)

  print(paste("Processed group", group[i], "with values beta =", coeff[i], "rsq =", rsq[i], "and p-val =", pval[i]))
}

## focal species
dabundyr <- dres %>%
  filter(year >= 2005,
         species == "Savannah Sparrow",
         park %in% c("Discovery Park", "Magnuson Park")) %>%
  group_by(year, park) %>% 
  summarise(abund = sum(seen, heard, fly)) %>%
  pivot_wider(names_from = park, values_from = abund) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(-1, names_to = "park", values_to = "abund")

##### data frame with total number of surveys done for each year
dnsurvyr <- dres %>% group_by(year, park) %>%
  summarise(nsurv = n_distinct(survey_id))

##### joining abundance and survey totals data and calculating mean abundance per survey "maps" column
dabundyr <- left_join(dabundyr, dnsurvyr) %>%
  mutate(maps = abund / nsurv)

pspecies <- ggplot(dabundyr, aes(x = year, y = maps)) +
  geom_point(fill = bcs_colors["dark green"]) +
  geom_smooth(method = "lm", se = FALSE, color = bcs_colors["bright green"]) +
  facet_wrap(~ park, scales = "free") + 
  theme_bcs()

pspecies


#### END #### 


