#Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) 
#variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
#
#
# Step 1: Set WD and Bring in data
# Note: The rds file stores, connects, and saves R objects

dir <- "C:/Users/Edgar/Documents/R/Coursera/Exploratory Data Analysis/"
setwd(dir)
getwd()
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Step 2: "Clean" the Data
library(dplyr)
library(ggplot2)
data <- tbl_df(NEI) # Usable data frame form w/ dplyr package
data <- data %>% filter(data$fips == "24510") # Subset the data for Baltimore City
data$year <- as.character(data$year) # Convert year variable to character
sub_data <- data %>% group_by(year, type) %>% summarize(Emissions= sum(Emissions))
# Subset the data based on year and type

# Step 3: Create the plot

location <- "Coursera_EDA_Project2/"

png(filename= paste(location,"plot3.png",sep=''), width=480, height=480, units="px") # Set image format
baltimore<- qplot(year, Emissions, data=sub_data, color=type, size=I(3),
                  main= "Emission's Trend in Baltimore City across Type" )
baltimore <- baltimore+ geom_smooth(aes(group=type), method="loess", se=FALSE)
baltimore <- baltimore+ facet_wrap(~type)
print(baltimore) # Check Graphics
dev.off() # Close Graphics and Show Plot in WD


