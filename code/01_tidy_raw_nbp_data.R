#############################
### 01. tidy raw nbp data ###
#############################

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(readxl)
library(openxlsx)

# raw data
## load raw data
raw <- read_excel("data/a_raw_data/nbp_raw_jan_24.xlsx")

## inspect raw data
# str(raw)

# tidy data
## rename columns to all lowercase
names(raw) <- tolower(names(raw))

## replaces spaces in column names with underscore
names(raw) <- str_replace_all(names(raw), pattern = " ", replacement = "_")

## assign each observation a row number a unique identifier
int <- raw %>% mutate(obs_id = row_number()) %>% select(obs_id, everything())

## assign each unique survey an identifier
int$survey_id <- paste(int$survey_date, int$park, int$loop, int$station, sep = "-")

## convert NA / NULL observation fields to zeros
int[is.na(int$seen),]$seen <- 0
int[is.na(int$heard), ]$heard <- 0
int[is.na(int$fly), ]$fly <- 0

## convert date/time fields to appropriate data classes
## NOTE HOLD OFF FOR NOW, IT'S MAKING THE DEDUP RUN FUNNY
##int$survey_date <- as.Date(int$survey_date, format = "%m/%d/%Y")
##int$start_time <- hm(int$start_time)
##int$end_time <- hm(int$end_time)


## check for duplicates
#dup <- sum(duplicated(raw)) # 1032 fully duplicated records
#sum(duplicated(select(raw, -notes))) - dup # 85 additional duplicates when ignoring note field 
#sum(duplicated(select(raw, survey_date, park, loop, station, species, seen, heard, fly))) - dup - 85 # 85 additional duplicates at the species observations level
#sum(duplicated(select(raw, survey_date, park, loop, station, species))) - dup - 85 -85 # 204 additional duplicates at species level, for total of 1406 duplicates (0.7% of records)

### dedup dataset
#### remove fully duplicated records
int <- int[!duplicated(select(int, -obs_id)), ]

#### for records that are duplicated except for note field, keep records with more information (i.e., longer notes field)
##### step 1: find all relevant duplicated observations
xnote_dup <- int[duplicated(select(int, -obs_id, -notes)) | duplicated(select(int, -obs_id, -notes), fromLast = TRUE), ]

##### step 2: calculate length of string in notes column
xnote_dup$note_len <- str_length(xnote_dup$notes)  
xnote_dup$ind <- paste(xnote_dup$survey_id, xnote_dup$species)

##### step 3: return the observation ID of the shorter string
inds <- unique(xnote_dup$ind)
rem <- numeric(length = length(inds))

##### step 4: among duplicated observation pairs, find the one with the shorter note length and assign it's obs_id to a vector

for(i in 1:length(inds)) {
  dups <- xnote_dup[xnote_dup$ind == inds[i], ]
  rem[i] <- dups[which.min(dups$note_len), ]$obs_id
}

##### step 5: filter out the unwanted duplicated observations
int <- int %>% filter(!obs_id %in% rem)

#### Next layer of duplicates are those that are duplicated to the level of species counts
##### create vector with desired column names
columns <- c("survey_id", "species", "seen", "heard", "fly")

##### create data frame with just duplicate values of interest
xcnt_dup <- int[duplicated(select(int, all_of(columns))) | duplicated(select(int, all_of(columns)), fromLast = TRUE), ]
# view(xcnt_dup %>% arrange(survey_id, species))

###### looks like there are up to three entries per species per survey. In many cases the difference is
###### that weather / dog data wasn't captured in weather fields but left in notes. In other cases, start/end times are different.

####### create new fields that will be useful
xcnt_dup$ind <- paste(xcnt_dup$survey_id, xcnt_dup$species, sep = "-") # creates identifier for observations of same species on same counts
xcnt_dup$na_cnt <- apply(X = is.na(xcnt_dup), MARGIN = 1, FUN = sum) # counts how many NAs are associated with each observation

###### for records where NAs in weather / dog fields are the difference, remove the duplicates with a greater number of NAs.
####### step 1: filter for just relevant records
columns <- c("survey_id", "species", "seen", "heard", "fly", "nest", "start_time", "end_time")
xcnt_dup_sub1 <- xcnt_dup[duplicated(select(xcnt_dup, all_of(columns))) | duplicated(select(xcnt_dup, all_of(columns)), fromLast = TRUE),  ]


###### step 2: create loop that returns obs_id for duplicated observation with greatest number of NAs
####### first create indices/parameters for use in loop
inds <- unique(xcnt_dup_sub1$ind) 
rem <- numeric(length = length(inds))

####### set up and run for loop
for(i in 1:length(inds)) {
  dups <- xcnt_dup_sub1[xcnt_dup_sub1$ind == inds[i], ]
  rem[i] <- dups[which.max(dups$na_cnt), ]$obs_id
}

###### step 3: Filter out rem obs_ids from intermediate data set
int <- int %>% filter(!obs_id %in% rem) 

###### for records where start/end times are different, keep records with fewest NAs and earliest start times
####### step 1: filter for relevant records
xcnt_dup_sub2 <- xcnt_dup[!xcnt_dup$obs_id %in% xcnt_dup_sub1$obs_id, ]

####### step 2: create loops that returns obs_id for duplicated observation with most NAs
inds <- unique(xcnt_dup_sub2$ind)
rem <- numeric(length = length(inds)*2)

for(i in 1:length(inds)){
  dups <- xcnt_dup_sub2[xcnt_dup_sub2$ind == inds[i], ]
  rem[i] <- dups[which.max(dups$na_cnt), ]$obs_id
}

###### step 3: create loop that returns obs_id for duplicated obs with latest start time (note: choosing to remove
###### latest start time was arbitrary)

xcnt_dup_sub3 <- filter(xcnt_dup_sub2, !obs_id %in% rem)
xcnt_dup_sub3$start_time <- hm(xcnt_dup_sub3$start_time)

inds <- unique(xcnt_dup_sub3$ind)

for(i in 1:length(inds)) {
  dups <- xcnt_dup_sub3[xcnt_dup_sub3$ind == inds[i], ]
  rem[i + 15] <- dups[which.max(as.numeric(dups$start_time)), ]$obs_id
}

###### step 4: filter out observations from intermediate dataset
int <- filter(int, !obs_id %in% rem)

### The next set of possible duplicates is at the species level, but counts are different
columns <- c("survey_id", "species")
xsp_dup <- int[duplicated(select(int, all_of(columns))) | duplicated(select(int, all_of(columns)), fromLast = TRUE), ]

# view(xsp_dup %>% arrange(survey_id, species))

## I think what I'd like to do in this case, where counts of seen / heard / fly are different between the same species report for a given count circle survey
## that I would like to merge the observations such that I keep the observation with the most data (i.e., fewest NAs) and then take the average value 
## for each of seen, heard, fly, rounded to the nearest integer

###### step 1: create useful indices / columns
xsp_dup$ind <- paste(xsp_dup$survey_id, xsp_dup$species, sep = "-")
xsp_dup$na_cnt <- apply(X = is.na(xsp_dup), MARGIN = 1, FUN = sum)

###### step 2: create a for loop to return obs_id for dup with fewest NAs, or if same number of NAs, the first one?
inds <- unique(xsp_dup$ind)
rem <- numeric(length = length(inds)) # there is one survey/species with a triple duplicate

for(i in 1:length(inds)) {
  dups <- xsp_dup[xsp_dup$ind == inds[i], ]
  rem[i] <- dups[which.max(dups$na_cnt), ]$obs_id
}

###### step 3: filter out from intermediate dataset
int <- int[!int$obs_id %in% rem, ]

##### step 4: merge observations
inds <- unique(xsp_dup$ind) # create index / lookup values
seen <- numeric(length = length(inds)) # empty vector for average of seen
heard <- numeric(length = length(inds)) # empty vector for average of heard
fly <- numeric(length = length(inds)) # empty vector for average of fly

for(i in 1:length(inds)) {
  dups <- xsp_dup[xsp_dup$ind == inds[i], ]
  seen[i] <- round(mean(dups$seen), 0)
  heard[i] <- round(mean(dups$heard), 0)
  fly[i] <- round(mean(dups$fly), 0)
}

# create data frame of observations that will accept the new averaged observation data
xsp_DE_dup1 <- xsp_dup[!xsp_dup$obs_id %in% rem, ] %>% arrange(survey_id, species)
xsp_DE_dup2 <- xsp_DE_dup1[!duplicated(select(xsp_DE_dup1, -obs_id, -seen, -heard, -fly)), ]

df <- data.frame(ind = inds, 
                 seen = seen,
                 heard = heard, 
                 fly = fly)

lu <- xsp_DE_dup2[,c("obs_id", "ind")]

df <- left_join(df, lu) %>%
  arrange(obs_id)

# overwrite existing observation with averaged values
int[int$obs_id %in% df$obs_id, ]$seen <- df$seen
int[int$obs_id %in% df$obs_id, ]$heard <- df$heard
int[int$obs_id %in% df$obs_id, ]$fly <- df$fly

# inspect results to ensure worked
# view(left_join(int %>% filter(obs_id %in% df$obs_id) %>% select(obs_id, seen:fly), df, join_by("obs_id" == "obs_id")))
## looks good

## OK so I've merged the data, now I think I still need to filter out the one remaining duplicates from Bliner property obs_id = 51864
int <- filter(int, !obs_id == 51864)

##I am expecting 1406 difference in observations between raw and intermediate dataset
length(raw$year) - length(int$year)  ## Great.

## set survey_date to date class
int$survey_date <- as.Date(int$survey_date, format = "%m/%d/%Y")

## Update taxonomy / nomenclature
### check what species are recorded in raw data
## write.csv(sort(unique(raw$species)), "data/a_raw_data/nbp_raw_species_list.csv", row.names = FALSE)

### update three species based on eBird Taxonomy v2023
int[int$species == "Northwestern Crow", ]$species <- "American Crow"
int[int$species == "Thayer's Gull", ]$species <- "Iceland Gull"
int[int$species == "Pacific-slope Flycatcher", ]$species <- "Western Flycatcher"

# Write intermediate data file
# write.csv(int, "data/b_intermediate_data/nbp_tidy_jan_24.csv", row.names = FALSE) # .csv too large

write.xlsx(int, "data/b_intermediate_data/nbp_tidy_jan_24.xlsx")
############## END ###############