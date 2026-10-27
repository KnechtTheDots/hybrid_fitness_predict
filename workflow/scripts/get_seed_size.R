# the first three lines create the log file for the snakemake
# workflow
log <- file(snakemake@log[['log']], open = "wt")
sink(log, type = "output")
sink(log, type = "message")

# load in packages
library(dplyr)
library(tidyr)

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
seed_size %>% 
  left_join(ids) %>% 
  mutate(cross = tolower(cross)) %>% 
  write.csv(snakemake@output[[1]], row.names = F, quote = F)

on.exit(sink())
