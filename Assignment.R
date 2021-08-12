#Week 1 Assignment Solution

if(!file.exists("./workingData")) {
        dir.create("./workingData") #Creating a folder to work in
}

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileURL, destfile = "./workingData/dataset.zip") #Downloading Data
unzip(zipfile = "./workingData/dataset.zip", exdir = "./workingData") #Extracting Data

#Loading the necessary libraries
library(lubridate) #To work with dates
library(dplyr) #To work with datasets

data <- read.table("./workingData/household_power_consumption.txt", header = TRUE, sep = ";", na.strings = c("?"), colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric"))
data$Date <- dmy(data$Date) #Converting to date format
filData <- subset(data, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02")) #Filtering out data for specified dates

filData <- mutate(filData, DateTime = ymd_hms(paste(filData$Date,filData$Time))) #Creating a new column with Date and Time combined
par(mfrow = c(1,1)) #Intialising the frame

#Plot 1
hist(filData$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power") #Creating the histogram
dev.copy(png, file = "./workingData/plot1.png") #Copying out to create PNG file
dev.off()

#Plot 2
plot(filData$DateTime,filData$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.copy(png, file = "./workingData/plot2.png") #Copying out to create PNG file
dev.off()

#Plot 3
plot(filData$DateTime,filData$Sub_metering_1, type = "l", ylab = "Energy sub metering", xlab = "") #Creating the base plot
lines(filData$DateTime, filData$Sub_metering_2, col =  "red") #Adding second line
lines(filData$DateTime, filData$Sub_metering_3, col =  "blue") #Adding third line
legend("topright",lty = 1, legend = c("sub_Metering_1","sub_Metering_2","sub_Metering_3"), col = c("black","red","blue")) #Creating the legend
dev.copy(png, file = "./workingData/plot3.png") #Copying out to create PNG file
dev.off()

#Plot 4
par(mfrow = c(2,2)) #Setting up base for the plot
plot(filData$DateTime,filData$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)") #Plot 2 as described above
plot(filData$DateTime,filData$Voltage , type = "l", xlab = "", ylab = "Voltage") #New line plot
plot(filData$DateTime,filData$Sub_metering_1, type = "l", ylab = "Energy sub metering", xlab = "") #Plot 3 as above
lines(filData$DateTime, filData$Sub_metering_2, col =  "red")
lines(filData$DateTime, filData$Sub_metering_3, col =  "blue")
legend("topright",lty = 1, legend = c("sub_Metering_1","sub_Metering_2","sub_Metering_3"), col = c("black","red","blue"))
plot(filData$DateTime,filData$Global_reactive_power , type = "l", xlab = "datetime", ylab = "Global_reactive_power") #New line plot
dev.copy(png, file = "./workingData/plot4.png") #Copying out to create PNG file
dev.off()