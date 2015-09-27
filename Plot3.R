# evaluation of sources of emissions PM2.5 - Baltimore City, Maryland, from 
# four years: 1999, 2002, 2005, to 2008
# these are the only years within the dataset
# median (average) result is used to standardize

# Answer to question:
# Two types appear to be increasing (Point and Non-Point) and
# two types appear to be decreasing (Non-Road and On-Road)

# requirements:
# 1. data files must be located in the working directory
# 2. requires dplyr and ggplot2 packages

# load dplyr and ggplot2 packages
library(dplyr)
library(ggplot2)

# read data file into NEI variable
NEI <- readRDS("summarySCC_PM25.rds")

# subset for Baltimore, Maryland fips variable == "24510"
sub_Baltimore <- filter(NEI, fips == "24510")

# remove NEI variable to conserve memory
rm(NEI)

# sort sub_Baltimore variable Emissions
sub_Baltimore$Emissions <- order(sub_Baltimore$Emissions)

# summarize Emissions by type and year
sub_Baltimore <- sub_Baltimore %>% group_by(type, year) %>% summarise(Emissions = median(Emissions))

# create vector to use for x axis in charts
years <- as.numeric(unique(sub_Baltimore$year))

# setup ggplot with data frame
g <- ggplot(sub_Baltimore, aes(year, Emissions))

# add layers
g <- g + geom_bar(stat = "identity", fill = "green") 
g <- g + labs(title = "Baltimore \n Average PM2.5 Emissions by Type") 
g <- g + labs(x = "Year") + labs(y = "PM2.5 Emissions (tons)")
g <- g + scale_x_continuous(breaks=years)
g <- g + facet_grid(.~type)
g <- g + theme(plot.title = element_text(size = 18, face = "bold"),
               axis.title.x = element_text(size = 14, face = "bold"),
               axis.title.y = element_text(size = 12, face = "bold"),
               axis.text.x = element_text(colour = "black"),
               strip.text = element_text(size = 10, face = "bold", colour = "black"),
               strip.background = element_rect(fill = "yellow"))

# create png file
ggsave(filename = "Plot3.png", plot = g, width = 7, height = 4, dpi = 100)

# clean up global variables
rm(list = ls())
