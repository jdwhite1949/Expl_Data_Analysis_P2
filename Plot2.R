# evaluation of total emissions PM2.5 - Baltimore City, Maryland, from 
# four years: 1999, 2002, 2005, to 2008
# these are the only years within the dataset
# for this chart the total (sum) is used

# Answer to question:
# results appear to be increasing, though this may be a function
# of the number of observations rather than average increase

# requirements:
# 1. data files must be located in the working directory
# 2. requires dplyr package

# load dplyr package
library(dplyr)

# read data file into NEI variable
NEI <- readRDS("summarySCC_PM25.rds")

# set (device) filename and size of png file
png(filename = "plot2.png", width = 480, height = 480, units = "px")

# subset for Baltimore, Maryland fips variable == "24510"
sub_Baltimore <- filter(NEI, fips == "24510")

# group by year variable
years <- group_by(sub_Baltimore, year)

# summarize Emissions column
sum_results <- summarize(years, Emissions = n())

# create vector for x axis values
x_titles <- as.vector(sum_results$year)

# chart the results in a barplot and set y limits
barplot(sum_results$Emissions, 
        main = "PM2.5 Total Emissions by Year \nBaltimore, Maryland \n(increasing)",
        names.arg = x_titles, xlab = "Year", 
        ylab = "PM2.5 (tons)", ylim = c(0, 800), col = "red")

# create png file
dev.off()

# clean up global variables
rm(list = ls())
