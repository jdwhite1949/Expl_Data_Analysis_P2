# the purpose of this script is to indicate how emissions from coal 
# combustion-related sources have changed from 1999 - 2008
# median (average) is used to standardize results

# SPECIAL NOTE: used base package to enabled the creation of charts
# with different y axis values - becuase the variance of values and
# range of the coal combustion types were very different

# resulting charts indicate a decline in all coal combustion types

# requirements:
# 1. data files must be located in the working directory
# 2. requires dplyr package

# assumptions
# 1. "Related data for "Coal combustion related sources" is 
#     identified as the elements in the EI.Sector variable
#     that contain Comb[ustion] and Coal - this selection
#     creates a subset containing three different Fuel Comb/Coal
#     types

# load dplyr and ggplot2 packages
library(dplyr)

# read data files into NEI and SCC variables
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# convert SCC variables from factors to characters
SCC[] <- lapply(SCC, as.character)

# select necessary columns from NEI
sub_NEI <- subset(NEI, select = c(SCC, Emissions, year))

# subset coal combustion related sources and necessary columns
sub_coal <- subset(SCC, grepl('Coal', EI.Sector, fixed = TRUE), 
            select = c(SCC, EI.Sector))

# remove NEI and SCC variables to conserve memory
rm(NEI, SCC)

# clean up names in EI.Sector variable (later used as labels in charts)
sub_coal$EI.Sector <- gsub("Fuel Comb - ", "", sub_coal$EI.Sector)
sub_coal$EI.Sector <- gsub(" - Coal", "", sub_coal$EI.Sector)

# determine number of unique sectors related to coal combustion
list_coal_sectors <- unique(sub_coal$EI.Sector)

# number of unique coal sectors
num <- length(list_coal_sectors)

# merge the two subsets
merged_data <- merge(sub_NEI, sub_coal, by = "SCC")

# remove the SCC variable
merged_data <- select(merged_data, -SCC)

# summarize data based on average for Emissions grouped by sector
# the average used in this case is the median - could also use the mean
# average value is used instead of total value because number of 
# observations may not be the same.

# group and summarize data
summary_data <- merged_data %>% group_by(EI.Sector, year) %>% summarise(Emissions = median(Emissions))

# get year labels for x axis tick marks
years <- as.vector(sort(unique(merged_data$year)))

# set (device) filename and size of png file
png(filename = "plot4.png", width = 700, height = 400, units = "px")

# establish parameters of plot sets
par(mfrow = c(1, num), oma = c(2, 2, 4, 2))

# create plots
for(n in 1:num){
  ran_col <- rgb(runif(1),runif(1),runif(1)) # create random color
  plot_data <- subset(summary_data, EI.Sector == list_coal_sectors[n])
  y_max <- max(plot_data$Emissions)*1.2 # set y axis limit
  assign(paste("plot", n, sep = ""),
         with(plot_data, 
              barplot(Emissions, main = list_coal_sectors[n], 
                names.arg = years, ylab = "PM2.5 (tons)", 
                xlab = "Year", ylim = c(0, y_max), col = ran_col)))
}

mtext("Coal Combustion PM2.5 Emissions", outer = TRUE)

# create png file
dev.off()

# clean up environment
rm(list = ls())