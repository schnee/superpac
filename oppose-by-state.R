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

or <- ddply(p, "state", summarize, or = sum(IEs.oppose.reps, na.rm=TRUE))

states_map <- map_data("state")

or$statelc <- tolower(state.name[match(or$state, state.abb)])

or$statelc[9] <- "district of columbia"

od <- ddply(p, "state", summarize, od = sum(IEs.oppose.dems, na.rm=TRUE))

op <- or

op$od <- od$od

pscore <- (op$od-op$or)/(op$od+op$or)

qpscore <- quantile(pscore, c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm=TRUE)

clean_theme <- theme(axis.title = element_blank(), axis.text=element_blank(), panel.background=element_blank(), panel.grid=element_blank(), axis.ticks.length = unit(0, "cm"), complete=TRUE)

ggplot(op, aes(map_id = statelc, fill=(od-or)/(od+or)))+ geom_map(map=states_map) + expand_limits(x=states_map$long, y=states_map$lat)+coord_map("polyconic") + scale_fill_gradient2(high=muted("red"), low=muted("blue"), mid="white",labels=c("Democratic","Mostly Democratic", "Even", "Mostly Republican", "Republican")) + labs(fill="Opposing\nOrientation") + ggtitle("Statewise Spending Orientation\n[Opposing]") +clean_theme
