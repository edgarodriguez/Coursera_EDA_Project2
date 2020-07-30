dir <- "C:/Users/Edgar/Documents/R/Coursera/Exploratory Data Analysis/"
setwd(dir)
getwd()
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 

data <- with(NEI, aggregate(Emissions, by = list(year), sum))

# Use with() to combine/sum the Emissions by year in seperate list
names(data)[1] <- "Year" # Rename the Column
names(data)[2] <- "Emissions" # Rename the Column

# Step 3: Create the plot

location <- "Coursera_EDA_Project2/"

png(filename= paste(location,"plot1.png",sep=''), width=480, height=480, units="px") # Set image format
par(mar=c(5, 5, 4.1, 3)) # Adjust the Margin sizes
plot(data, type= "b", pch= 19, yaxt="n", xaxt="n",
     xlab="Years", ylab= "Emissions (Tons)")
title(main="Plot of Total Emmisions by Years")

dev.off() # Close Graphics and Show Plot in WD




