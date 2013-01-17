#!/usr/bin/Rscript

require(mapproj)
require(ggplot2)
require(grid)
library(maps)
library(plyr)

pac <- read.csv("./committee_summary.csv", header=TRUE)

p <- pac

names(p)[12] <- "receipts"

pdata <- ddply(p, "state", summarize, Sum = sum(receipts, na.rm=TRUE))

states_map <- map_data("state")

pdata$statelc <- tolower(state.name[match(pdata$state, state.abb)])

pdata$statelc[9] <- "district of columbia"

ggplot(pdata, aes(map_id = statelc, fill=Sum))+ geom_map(map=states_map) + expand_limits(x=states_map$long, y=states_map$lat)+coord_map("polyconic")


qs = quantile(pdata$Sum, c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
qsf = sprintf("$%.2f", qs)
qlab = paste(head(qsf, -1), tail(qsf, -1), sep=' - ')

pdata$Sum_q <- cut(pdata$Sum, qs, labels=qlab, include.lowest=TRUE)

pal <- colorRampPalette(c("grey80", "darkred"))(5)

clean_theme <- theme(axis.title = element_blank(), axis.text=element_blank(), panel.background=element_blank(), panel.grid=element_blank(), axis.ticks.length = unit(0, "cm"), complete=TRUE)


ggplot(pdata, aes(map_id = statelc, fill=Sum_q))+ geom_map(map=states_map, colour="black") + scale_fill_manual(values=pal) + expand_limits(x=states_map$long, y=states_map$lat)+coord_map("polyconic") + labs(fill="Receipts", x="Longitude", y="Latitude") + ggtitle("Super Political Action Committee\nReceipts by State") + clean_theme
