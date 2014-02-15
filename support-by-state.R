#!/usr/bin/Rscript

require(ggplot2)
require(plyr)
require(maps)
require(grid)
require(mapproj)
require(scales)

pac <- read.csv("./committee_summary.csv", header=TRUE)
p <- pac
names(p)[12] <- "receipts"

sd <- ddply(p, "state", summarize, sd = sum(IEs.support.dems, na.rm=TRUE))

states_map <- map_data("state")

sd$statelc <- tolower(state.name[match(sd$state, state.abb)])

sd$statelc[9] <- "district of columbia"

sr <- ddply(p, "state", summarize, sr = sum(IEs.support.reps, na.rm=TRUE))

sp <- sd

sp$sr <- sr$sr

sp$pscore <- (sp$sr-sp$sd)/(sp$sd+sp$sr)

clean_theme <- theme(axis.title = element_blank(), axis.text=element_blank(), panel.background=element_blank(), panel.grid=element_blank(), axis.ticks.length = unit(0, "cm"), complete=TRUE)

ggplot(sp, aes(map_id = statelc, fill=pscore)) + 
  geom_map(map=states_map) + 
  expand_limits(x=states_map$long, y=states_map$lat) + 
  coord_map("polyconic") + 
  scale_fill_gradient2(high=muted("red"), 
                       low=muted("blue"), 
                       mid="white",
                       labels=c("Democratic","Mostly Democratic", "Even", "Mostly Republican", "Republican")) + 
  labs(fill="Supporting\nOrientation") + 
  ggtitle("Statewise Spending Orientation\n[Supporting]") +
  clean_theme
