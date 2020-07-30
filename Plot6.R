# Coursera Exploratory Data Analysis Assignment
# Plot 6: 
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
#in Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

dir <- "C:/Users/Edgar/Documents/R/Coursera/Exploratory Data Analysis/"
setwd(dir)
getwd()
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Step 2: "Clean" the Data
library(dplyr)
library(ggplot2)
NE_data <- tbl_df(NEI) # Usable data frame form w/ dplyr package
SC_data <- tbl_df(SCC)

data <- merge(NE_data, SC_data, by.x="SCC", by.y="SCC", all=TRUE) 
# Merge the data frames
data <- data %>% filter(data$fips == "24510" | data$fips == "06037") # Subset the data for Cities
data$year <- as.character(data$year) # Convert year variable to character
sub_data <- data %>% group_by(year, fips, EI.Sector) %>% summarize(Emissions= sum(Emissions, na.rm=TRUE))
sub <- sub_data %>% filter(EI.Sector=="Mobile - Locomotives" | 
                               EI.Sector=="Mobile - Non-Road Equipment - Diesel" | 
                               EI.Sector== "Mobile - Non-Road Equipment - Gasoline" | 
                               EI.Sector== "Mobile - Non-Road Equipment - Other" |
                               EI.Sector== "Mobile - On-Road Diesel Heavy Duty Vehicles" | 
                               EI.Sector=="Mobile - On-Road Diesel Light Duty Vehicles" | 
                               EI.Sector== "Mobile - On-Road Gasoline Heavy Duty Vehicles" | 
                               EI.Sector=="Mobile - On-Road Gasoline Light Duty Vehicles")  

sub_data <- sub %>% group_by(year, fips) %>% summarize(Emissions= sum(Emissions, na.rm=TRUE))
colnames(sub_data) <- c("Year", "City", "Emissions") # Fix Column Names
sub_data[sub_data == "06037"] <- "Los Angeles" # Convert Number Holds to Meaningful values
sub_data[sub_data == "24510"] <- "Baltimore"   # Convert Number Holds to Meaningful values

# Step 3: Create the plot

location <- "Coursera_EDA_Project2/"

png(filename= paste(location,"plot6.png",sep=''), width=480, height=480, units="px") # Set image format
compare<- qplot(Year, Emissions, data=sub_data, color=City, size=I(3),
                main= "Comparison Emission's Trend Los Angeles Versus Baltimore City" )
compare <- compare+ geom_smooth(aes(group=City), method="loess", se=FALSE)
compare <- compare+ scale_colour_discrete(name = "City")
compare <- compare+ labs(x = "Year") + labs(y = "Motor Emissions (Tons)")
compare <- compare+ theme(axis.text.x= element_text(angle=45, size=10)) 
compare <- compare + theme(plot.title = element_text(size=10, face="bold", vjust=2))
print(compare) # Check Graphics
dev.off() # Close Graphics and Show Plot in WD