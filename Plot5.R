# How have emissions from motor vehicle sources changed from 
# 1999-2008 in Baltimore City?  All points are ploted with a regression
# line - a negative slope (right end of line lower that left end of line)
# may indicate that particulate matter is declining over time (1999 - 2008)

# Answer to question:
# Slopes of regression lines indicate that PM2.5 particulates may
# be decreasing from 1999 to 2008 for all ON-ROAD types

# requirements:
# 1. data files must be located in the working directory
# 2. requires dplyr package
# 3. Used base package because data ranges for different types 
#    varied considerably

# assumptions for charts
# Related data for "motor vehicles" is represented for this   
#     analysis as the following: where the SCC type variable 
#     is equal to "ON-ROAD"

# load dplyr and ggplot2 packages
library(dplyr)

# read data files into NEI and SCC variables
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# convert SCC variables from factors to characters
SCC[] <- lapply(SCC, as.character)

# subset for Baltimore, Maryland fips variable == "24510"
sub_Baltimore <- filter(NEI, fips == "24510")

# subset SCC data to necessary variables
sub_SCC <- subset(SCC, select = c(SCC, EI.Sector))

# remove NEI and SCC variables from environment to free up memory
rm(NEI, SCC)

# merge the Baltimore data set withthe mv_SCC data 
merge_data <- merge(sub_Baltimore, sub_SCC, by = "SCC")

# filter merged data on type = "ON-ROAD"
onRoad_data <- filter(merge_data, type == "ON-ROAD")

# replace long with shorter descriptions for EI.Sector
onRoad_data$EI.Sector <- gsub("Mobile - On-Road ", "", onRoad_data$EI.Sector)
onRoad_data$EI.Sector <- gsub(" Vehicles", "", onRoad_data$EI.Sector)

# select appropriate columns (variables)
plot_data <- select(onRoad_data, Emissions, EI.Sector, year)

plot_data <- plot_data %>% group_by(EI.Sector, year)

# create labels to use for separate charts
sector_labels <- unique(plot_data$EI.Sector)

# establish number of unique labels
num_labels <- length(sector_labels)

# create x axis tick mark labels
years <- as.vector(sort(unique(plot_data$year)))

# set (device) filename and size of png file
png(filename = "plot5.png", width = 600, height = 600, units = "px")

# establish parameters of plot sets
par(mfrow = c(2, num_labels/2), oma = c(2, 2, 4, 2), mar = c(2,3,4,2), font = 2)

# create plots
for(n in 1:num_labels){
  this_data <- subset(plot_data, EI.Sector == sector_labels[n]) # subset data / sector
  assign(paste("plot", n, sep = ""),
         with(this_data, 
         plot(year, Emissions, xaxt = "n")))  # plot data
  abline(lm(this_data$Emissions~this_data$year))  # add regression line
  
  # add labels and format chart
  mtext(sector_labels[n], side = 3, line = 1, cex = .9)
  mtext("Year", side = 1, line = 2, cex = .75)
  mtext("PM2.5 (tons)", side = 2, line = 3, cex = .75)
  axis(1, at = years, labels = years, cex.axis = .8)
}

mtext("Motor Vehicle PM2.5 Emissions \nCity of Baltimore", outer = TRUE)

# create png file
dev.off()

# clean up environment
rm(list = ls())
