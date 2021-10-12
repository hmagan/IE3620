# Hank Magan
# 10.04.2021
# WK7L1Magan.R (election data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Week 7")

# import libraries and datasets
library(ggplot2)
library(gridExtra)
library(dplyr)
library(maps)
source("./theme_map.R")
raw_election_2016 <- read.csv("http://chemistry.ncssm.edu/digihum/data/electiondata2016.csv", header=TRUE)
raw_election_2020 <- read.csv("http://chemistry.ncssm.edu/digihum/data/electiondata2020.csv", header=TRUE)
us_states <- map_data("state") # contains lat/long data for plotting maps

# list of party colors for filling states in on maps
party_colors <- c("#2E74C0", "#CB454A")

# merge long/lat data and election data
raw_election_2016$region <- tolower(raw_election_2016$state)
election_2016 <- left_join(us_states, raw_election_2016) # new dataset as described

# create a map of which party won each state (2016)
party_map_2016 <- ggplot(data=election_2016, mapping=aes(x=long, y=lat, group=group, fill=party)) + 
                  geom_polygon(color="blue", size=0.1) + 
                  coord_map(projection="albers", lat0=30, lat1=45) + 
                  scale_fill_manual(values=party_colors) + 
                  labs(title="Election Results 2016", fill=NULL) + 
                  theme_map()
party_map_2016

names(raw_election_2020)[1] <- "state"
raw_election_2020$region <- tolower(raw_election_2020$state)
election_2020 <- left_join(us_states, raw_election_2020)

# create a map of which party won each state (2020)
party_map_2020 <- ggplot(data=election_2020, mapping=aes(x=long, y=lat, group=group, fill=party)) + 
                  geom_polygon(color="blue", size=0.1) + 
                  coord_map(projection="albers", lat0=30, lat1=45) + 
                  scale_fill_manual(values=party_colors) + 
                  labs(title="Election Results 2020", fill=NULL) + 
                  theme_map()
party_map_2020

# displays a map of Trump's vote % in each state (2020)
trump_map_2020 <- ggplot(data=election_2020, mapping=aes(x=long, y=lat, group=group, fill=pct_trump)) + 
                  geom_polygon(color="white", size=0.1) + 
                  coord_map(projection="albers", lat0=30, lat1=45) + 
                  labs(title="Trump Election Results 2020", fill="Trump \n %") + 
                  theme_map()
trump_map_2020

# displays a map of Biden's vote % in each state (2020)
biden_map_2020 <- ggplot(data=election_2020, mapping=aes(x=long, y=lat, group=group, fill=pct_biden)) + 
                  geom_polygon(color="white", size=0.1) + 
                  coord_map(projection="albers", lat0=30, lat1=45) + 
                  labs(title="Biden Election Results 2020", fill="Biden \n %") + 
                  theme_map()
biden_map_2020

# arrange all four plots onto a single 2x2 graphic
grid.arrange(party_map_2016, party_map_2020, trump_map_2020, biden_map_2020, ncol=2, nrow=2)
