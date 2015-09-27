# Comparision of emissions from motor vehicle sources from 1999 - 2008 
# for both Baltimore City and Los Angeles County, California?  
# Median averages are used for comparison in a barplot. The question to 
# answer is which area has seen the greatest changes over time?

# Answer to question: 
# Los Angeles appears to have much more PM2.5 particulates than Baltimore in 
# any given year or motor vehicle type. The results for Los # Angeles in 2008 
# for Gasoline Light Duty are an outlier and consequenctly do not appear in 
# the chart. Baltimore appears to have declining values, while those for 
# Los Angeles are more difficult to interpret.

# requirements:
# 1. data files must be located in the working directory
# 2. requires dplyr and ggplot2 packages

# assumptions for charts
# Related data for "motor vehicles" is represented for this   
#     analysis as the following: where the SCC type variable 
#     is equal to "ON-ROAD"

# load dplyr and ggplot2 packages
library(dplyr)
library(ggplot2)

# read data files into NEI and SCC variables
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# convert SCC variables from factors to characters
SCC[] <- lapply(SCC, as.character)

# subset NEI data for Baltimore City and Los Angeles and type = "ON-ROAD"
sub_NEI <- filter(NEI, fips == "24510" | fips == "06037" & type == "ON-ROAD")

# subset SCC data to include needed variables
sub_SCC <- subset(SCC, select = c(SCC, EI.Sector))

# remove NEI and SCC variables to limit memory use
rm(NEI, SCC)

# merge two data frames
merged_data <- merge(sub_NEI, sub_SCC, by = "SCC")

# remove unnecasiry variables
merged_data <- select(merged_data, -SCC, -Pollutant)

# filter dataset on type = "ON-ROAD"
onRoad_data <- filter(merged_data, type == "ON-ROAD")

# replace long with shorter descriptions for EI.Sector
onRoad_data$EI.Sector <- gsub("Mobile - On-Road ", "", onRoad_data$EI.Sector)
onRoad_data$EI.Sector <- gsub(" Vehicles", "", onRoad_data$EI.Sector)

# replace fips code by city name
onRoad_data$fips <- gsub("06037", "Los Angeles", onRoad_data$fips)
onRoad_data$fips <- gsub("24510", "Baltimore", onRoad_data$fips)

# rename fips column
onRoad_data <- rename(onRoad_data, City = fips)

# summarize data to medians for charts
city_data <- onRoad_data %>% group_by(City, EI.Sector, year) %>% summarise(Emissions = median(Emissions))

# create labels to use for separate charts
sector_labels <- unique(plot_data$EI.Sector)

# create vector to use for x axis in charts
years <- as.numeric(sort(unique(city_data$year)))

# create x axis tick mark labels
years <- as.vector(sort(unique(plot_data$year)))

# set y maximum limit - reason - results for Los Angeles, 2008, Gasoline Light Duty
# appear to be inconsistent with the other results
y_limit <- mean(city_data$Emissions)

# create plots
g <- ggplot(city_data, aes(year, Emissions, fill = City))
g <- g + geom_bar(stat="identity", position = "dodge")
g <- g + ylim(0, y_limit)
g <- g + facet_grid(.~EI.Sector)
g <- g + labs(title = "Baltimore vs Los Angeles\n Average PM2.5 Emissions by Sector") 
g <- g + labs(x = "Year") + labs(y = "PM2.5 Emissions (tons)")
g <- g + scale_x_continuous(breaks=years)        
g <- g + theme(plot.title = element_text(size = 14, face = "bold"),
               axis.title.x = element_text(size = 12, face = "bold"),
               axis.title.y = element_text(size = 12, face = "bold"),
               axis.text.x = element_text(colour = "black"),
               legend.text = element_text(size = 10, face = "bold"),
               legend.title = element_text(size = 10, face = "bold"),
               strip.text = element_text(size = 10, face = "bold", colour = "black"),
               strip.background = element_rect(fill = "yellow"))

# create png file
ggsave(filename = "Plot6.png", plot = g, width = 9, height = 5, dpi = 100)

# clean up environment
rm(list = ls())
