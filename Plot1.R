# evaluation of total emissions PM2.5 - United States from 
# four years: 1999, 2002, 2005, to 2008
# these are the only years within the dataset
# For this chart, the total (sum) results are used.

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
png(filename = "plot1.png", width = 480, height = 480, units = "px")

# group by year variable
years <- group_by(NEI, year)

# summarize Emissions column
sum_results <- summarize(years, Emissions = n())

# add column that deals with Emissions large numbers
sum_results <- mutate(sum_results, rescaled_emissions = Emissions/1000000)

# create vector for x axis values
x_titles <- as.vector(sum_results$year)

# chart the results in a barplot and set y limits
barplot(sum_results$rescaled_emissions, 
        main = "PM2.5 Total Emissions by Year \n(not decreasing)",
        names.arg = x_titles, ylim = c(0, 2), xlab = "Year", 
        ylab = "PM2.5 (millions of tons)", col = "green")

# create png file
dev.off()

# clean up global variables
rm(list = ls())
