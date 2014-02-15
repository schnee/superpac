#!/usr/bin/Rscript

require(ggplot2)
require(dplyr)
require(maps)
require(grid)
require(mapproj)
require(scales)

p <- read.csv("./committee_summary.csv", header=TRUE)

names(p)[12] <- "receipts"

superpac.spend = pac %.% 
  group_by(state) %.% 
  summarise(or=sum(IEs.oppose.reps), 
            od=sum(IEs.oppose.dems), 
            sr=sum(IEs.support.reps), 
            sd=sum(IEs.support.dems)) %.%
  arrange(state)


# superpac.spend <- ddply(p, "state", summarize, 
#                         sd = sum(IEs.support.dems, na.rm=TRUE), 
#                         sr=sum(IEs.support.reps, na.rm=T),
#                         od = sum(IEs.oppose.dems, na.rm=T),
#                         or = sum(IEs.oppose.reps, na.rm=T))

states_map <- map_data("state")

superpac.spend$statelc <- tolower(state.name[match(superpac.spend$state, state.abb)])

superpac.spend$statelc[9] <- "district of columbia"

superpac.spend$support <- (superpac.spend$sr-superpac.spend$sd)/(superpac.spend$sd+superpac.spend$sr)
superpac.spend$oppose <- (superpac.spend$od-superpac.spend$or)/(superpac.spend$od+superpac.spend$or)

clean_theme <- theme(axis.title = element_blank(), axis.text=element_blank(), panel.background=element_blank(), panel.grid=element_blank(), axis.ticks.length = unit(0, "cm"), complete=TRUE)

ggplot(superpac.spend, aes(map_id = statelc, fill=support)) + 
  geom_map(map=states_map) + 
  expand_limits(x=states_map$long, y=states_map$lat) + 
  coord_map("polyconic") + 
  scale_fill_gradient2(high=muted("red"), 
                       low=muted("blue"), 
                       mid="white",
                       labels=c("Democratic","Mostly Democratic", "Even", "Mostly Republican", "Republican")) + 
  labs(fill="Supporting\nOrientation") + 
  ggtitle("Statewise Spending Orientation\n2011-2012 Election Cycle\n[Supporting]") +
  clean_theme

ggplot(superpac.spend, aes(map_id = statelc, fill=oppose)) + 
  geom_map(map=states_map) + 
  expand_limits(x=states_map$long, y=states_map$lat) + 
  coord_map("polyconic") + 
  scale_fill_gradient2(high=muted("red"), 
                       low=muted("blue"), 
                       mid="white",
                       labels=c("Democratic","Mostly Democratic", "Even", "Mostly Republican", "Republican")) + 
  labs(fill="Opposing\nOrientation") + 
  ggtitle("Statewise Spending Orientation\n2011-2012 Election Cycle\n[Opposing]") +
  clean_theme
