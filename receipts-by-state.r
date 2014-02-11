#!/usr/bin/Rscript

require(mapproj)
require(ggplot2)
require(grid)
library(maps)
require(plyr)

pac <- read.csv("./committee_summary.csv", header=TRUE)

names(pac)[12] <- "receipts"

pdata <- ddply(pac, "state", summarize, Sum = sum(receipts, na.rm=TRUE))

states_map <- map_data("state")

pdata$statelc <- tolower(state.name[match(pdata$state, state.abb)])

pdata$statelc[9] <- "district of columbia"

ggplot(data=pdata, aes(x=state, y=Sum)) + 
  geom_bar(width=1, stat="identity") + 
  theme(legend.position="none") + 
  labs(x="State", y="Receipts") + 
  ggtitle("SuperPAC Receipts by State Comparison")

pdata.noDC = subset(pdata, state!="DC")

ggplot(pdata.noDC, aes(x=state, y=Sum)) + 
  geom_bar(width=1, stat="identity") + 
  theme(legend.position="none") + labs(x="State", y="Receipts") + 
  ggtitle("SuperPAC Receipts by State Comparison")

ggplot(pdata.noDC, aes(map_id = statelc, fill=Sum)) + 
  geom_map(map=states_map) + 
  expand_limits(x=states_map$long, y=states_map$lat)+
  coord_map("polyconic")

qs = quantile(pdata.noDC$Sum, c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
qsf = sprintf("$%.2f", qs)
qlabels = paste(head(qsf, -1), tail(qsf, -1), sep=' - ')

pdata.noDC$Sum_q <- cut(pdata.noDC$Sum, qs, labels=qlabels, include.lowest=TRUE)

pal <- colorRampPalette(c("grey80", "darkgreen"))(5)

clean_theme <- theme(axis.title = element_blank(), axis.text=element_blank(), panel.background=element_blank(), panel.grid=element_blank(), axis.ticks.length = unit(0, "cm"), complete=TRUE)

ggplot(pdata.noDC, aes(map_id = statelc, fill=Sum_q))+ geom_map(map=states_map, colour="black") + 
  scale_fill_manual(values=pal) + 
  expand_limits(x=states_map$long, y=states_map$lat)+
  coord_map("polyconic") + 
  labs(fill="Receipts", x="Longitude", y="Latitude") + 
  ggtitle("SuperPAC Receipts by State\nof Registration") + 
  clean_theme
