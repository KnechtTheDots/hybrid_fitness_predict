# the first three lines create the log file for the snakemake
# workflow
log <- file(snakemake@log[['log']], open = "wt")
sink(log, type = "output")
sink(log, type = "message")

# load in packages
library(dplyr)
library(tidyr)



# Seed Size ---------------------------------------------------------------


# initials of the researchers that measured the seeds
initials <- c("jk", "js", "lb", "rp")

# there were some notes etc. beyond the 4th column, so only take the first 4 columns
tmp <- read.csv(paste0("data/raw/seed_areas_", initials[1], ".csv"))[,1:4]

tmp$researcher <- initials[1]

d <- tmp

for(i in 2:length(initials)){
  tmp <- read.csv(paste0("data/raw/seed_areas_", initials[i], ".csv"))[,1:4]
  
  tmp$researcher <- initials[i]
  
  d <- rbind(d, tmp)
}


# calculate the mean of the four measurements
seed_size <- d %>% 
  group_by(tray, block, ind) %>% 
  summarise(seed_area = round(mean(area),3))


ids <- read.csv("data/raw/planting_ids.csv")

# tray format is different in the two files, so need to change that to merge
ids <- ids %>% 
  mutate(tray = gsub("LC", "lc_", tray)) %>% 
  select(tray, block, cross, mother, father)

# join the ids with seed size and write to csv
seed_size <- seed_size %>% 
  left_join(ids) %>% 
  mutate(cross = tolower(cross))



# Germination Timing ------------------------------------------------------


germ <- read.csv("data/raw/germination_timing.csv")

germ <- germ %>% 
    pivot_longer(4:ncol(germ), names_to = "date", values_to = "germ") %>% 
    # get rid of X in front of date, separate and get rid of the time
    # and make date numeric (also get rid of 24 since they are all
    # 2024)
    mutate(date = gsub("X24", "", date)) %>% 
    separate(date, into = c("date", NA), sep = "_") %>% 
    mutate(date = as.numeric(date)) %>% 
    # now drop all the NAs when a germ wasn't observed and keep only the 
    # earliest observed date, i.e., when it germinated
    drop_na(germ) %>% 
    group_by(tray, block, ind) %>% 
    filter(date == min(date))


# since some of the germination might have been caught first in the seedling
# images, find the earliest image for each seedling and make that the germ time
# if it is less than above

areas <- read.csv("data/raw/seedling_areas.csv")


areas <- areas %>% 
    # remove the 24 and hours
    separate(time, into = c("day", NA), sep = "_") %>% 
    mutate(day = gsub("24","", day),
           day = as.numeric(day)) %>% 
    # make area numeric and drop NAs
    mutate(area = as.numeric(area)) %>% 
    drop_na(area) %>% 
    # now get the earliest day for each individual
    group_by(tray, block, ind) %>% 
    filter(day == min(day)) %>% 
    # since multiple researchers measured each day/seedling combo, there
    # are multiple instances of each day, so filter down to just one
    ungroup() %>% 
    filter(!duplicated(paste0(tray, "_", block, "_", ind)))

germ_day <- areas %>% 
    mutate(tray = paste0("lc_", tray)) %>% 
    left_join(germ, by = join_by(tray, block, ind)) %>%
    pivot_longer(c(day, date), values_to = "germ_day") %>% 
    drop_na(germ_day) %>% 
    group_by(tray, block, ind) %>% 
    filter(germ_day == min(germ_day)) %>% 
    filter(!duplicated(tray, block, ind)) %>% 
    select(tray, block, ind, germ_day)

# now join the germination day with seed size
seed_size <- seed_size %>% ungroup()
germ_day <- germ_day %>% ungroup()

germ_day <- seed_size %>% 
  left_join(germ_day)

germ_day %>% 
  # the start of the experiment was Julian day 260, so subtract that from
  # germ_day to get the germ_day from day 0
  mutate(germ_day = germ_day - 260) %>% 
  write.csv(snakemake@output[["seeds"]], row.names = F, quote = F)


# Seedling Areas ----------------------------------------------------------

seedlings <- read.csv("data/raw/seedling_areas.csv")

seedlings <- seedlings %>% 
  # make area numeric, then drop the NAs to get just observations.
  mutate(area = as.numeric(area)) %>% 
  drop_na(area) %>% 
  # the time stamp is the same as above for germination, just going to 
  # make it the day
  separate(time, into = c("day", NA), sep = "_") %>% 
  mutate(day = gsub("24", "", day),
         day = as.numeric(day)) %>% 
  # now average each seedling time.
  group_by(tray, block, ind, day) %>% 
  summarise(area = round(mean(area),3)) %>% 
  ungroup() %>% 
  mutate(tray = paste0("lc_", tray)) %>% 
  # join with the germ day data to get cross information as well 
  # as germination time to calculate age.
  left_join(germ_day, by = join_by(tray, block, ind)) %>% 
  mutate(age = day - germ_day) %>%
  # there are some aberrant observations, remove them see notebook1 for more information
  filter(!(paste0(tray, block, ind) %in% c("lc_2313", "lc_6911",
                                           "lc_2112", "lc_4912",
                                           "lc_5117", "lc_5117"))) %>% 
  filter(log(area) > -2.5) %>%
  # select columns and write to the csv
  select(tray, block, ind, area, age, cross, mother, father) %>%
  write.csv(snakemake@output[["seedlings"]], row.names = F, quote = F)


on.exit(sink())
