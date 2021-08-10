#Starting course on 5th August

#Help and Resources
## 1. R Graph Gallery - https://www.r-graph-gallery.com/


#Principles of Analytic Graphics
## 1. Show Comparisons - "Compared to What?"
## 2. Show Causality - Causality, mechanism, explanation etc for the finding - show evidence of this!
## 3. Show multivariate data - "Show as much as variables on a plot as possbile"
## 4. Integrate the evidence you have - "Completely integrate words, numbers, images and diagrams"
## 5. Describe & Document evidence with labels, scales, sources etc
## 6. Content is King - Should have really good content for proper analysis


#Exploratory Graphs
pollution <- read.csv("./data/avgpm25.csv", colClasses = c("numeric","character","factor","numeric","numeric"))
head(pollution)
summary(pollution$pm25) #Quick 5 value summary of the variable
boxplot(pollution$pm25, col = "blue") #Boxplot
hist(pollution$pm25) #Generates a histogram of the variable
rug(pollution$pm25) #plots the data values under the histogram to view where the numbers lie more accurately
hist(pollution$pm25, col = "green", breaks = 100) #Increasing the breaks in the histogram

boxplot(pollution$pm25, col = "blue")
abline(h = 12) #puts a horizontal line at 12 to view the limit more accurately

hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2) #vertical line at 12 with width 2
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)

barplot(table(pollution$region), col = "wheat", main = "Number of counties in each region")

boxplot(pm25 ~ region, data =  pollution, col = "red") #2D boxplot with region and pm25 
hist(subset(pollution, region =="east")$pm25, col = "green")
hist(subset(pollution, region =="west")$pm25, col = "green") #subsetting function can be used to apply a logical condition to a vectr

with(pollution, plot(latitude, pm25)) 
abline(h = 12, lwd =2, lty = 2) #Specifying line type as well
with(pollution, plot(latitude, pm25, col = region)) #Color will be according to region since its a factor variable

par(mfrow = c(1,2), mar = c(5,4,2,1)) #Specifying the number of rows and columns as well as margin to combine the scatter plots into a single image
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "Title: West")) #Combining things and adding title
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "Title: East"))


#Plotting Systems
##Base
library(datasets)
data(cars)
with(cars, plot(speed, dist))

##Lattice System
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

#GGPLOT2
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

#Base Plotting System Detailed
library(datasets)
hist(airquality$Ozone)
with(airquality, plot(Wind, Ozone))

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

##Base plotting functions share a set of parameters
## 1. pch - plotting symbol. Takes a number to refer to a character or can take a character that you specify
## 2. lty - line type. 
## 3. lwd - line width
## 4. col - plotting color. colors() function gives you vector of colors by name
## 5. xlab - x axis label
## 6. ylab- y axis label

#The par() is used to specify global graphic parameters. These can be overriden by specifying in specific plot calls
## 1. las -  orientation of the axis labels on the plot
## 2. bg - background color
## 3. mar - margin size. (bottom, left, top, right)
## 4. oma - outer margin size
## 5. mfrow - number of plots per row, column (plots are filled row wise)
## 6. mfcol - number of plots per row, column (plots are filled column wise)
#Defaults for par() can be found by calling for ex. par("bg")

#Key base plotting functions
## 1. plot - makes a scatter plot or other type of plot depending on the class of object
## 2. lines - add lines to a plot. given a vector x values and a corresponding vector of y values (or a 2 column matrix); this function just connects the dots
## 3. points - add points to a plot.
## 4. text - add text labels to the plot using specified x,y coordinates
## 5. title - add annotations to x,y axis labels, title, subtitle, outer margin
## 6. mtext - add arbitrary text to margins
## 7. axis - add axis ticks/ labels

with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")

with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue")) #Re adding points for just the months of may in blue to above plot

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York"), type = "n") #Added title directly in plot call and also specified type to be n to just setup the plot. Doesnt add data in this case
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months")) #Adding a legend
model <- lm(Ozone ~ Wind, airquality) #Creating a liner model
abline(model, lwd = 2) #Using abline to add the linear model to the graph

par(mfrow = c(1,2)) #1 row and 2 columns
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1,3), mar = c(4,4,2,1), oma = c(0,0,2,0)) #Reduce the margins a bit lower than default and added an outer margin on top using oma
with(airquality, {
        plot(Wind, Ozone, main = "Wind and Ozone")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
        plot(Temp, Ozone, main = "Ozone and Temperature")
        mtext("Ozone and Weather in New York", outer = TRUE)
})

#Base plotting Demonstration
x <- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x,y)
plot(x,y, pch = 3) #Changing point type 
#example(points) # to see demos
plot(x,y, pch = 23, col = "green", bg = "yellow") #Point type 21 through 25 have border and fill colors specified like this
plot(x,y, pch = 20)
plot(x,y, pch = 20)
title("Scatterplot")
text(-2,-2, "Label")
legend("topleft", legend = "Data")
fit <- lm(y~x)
abline(fit)
abline(fit, lwd = 3, col =  "blue")

plot(x,y, xlab = "Weight", ylab = "Height", main = "Scatterplot", pch =20)
legend("topright", legend = "Data", pch = 20)
fit <- lm(y~x)
abline(fit, lwd = 3, col = "red")

z <- rpois(100, 2)
par(mfrow = c(2, 1))
plot(x,y, pch =20)
plot(x,z, pch = 19)
par("mar") #Checks the current margin
par( mar = c(2,2,1,1))
plot(x,y, pch =20)
plot(x,z, pch = 19)

par(mfrow = c(2,2))
plot(x,y)
plot(x,z)
plot(z,x)
plot(z,y)

par(mfrow = c(1,1))
x <- rnorm(100)
y <- x+ rnorm(100)
g <- gl(2,50, labels = c("Male","Female")) 
str(g)
plot(x,y, type = "n") #Just setups up everythign else but doesnt put in the data
points(x[g == "Male"], y[g == "Male"], col = "green") #Adding points sequentally by group
points(x[g == "Female"], y[g == "Female"], col = "blue", pch = 19)


#Graphic Devices
##Where the plot is sent to (Screen, PDF, image, SVG etc)
?devices

library(datasets) #Normal Screen plot
with(faithful, plot(eruptions, waiting))
title(main = "old Faithful Data")

pdf(file = "myplot.pdf") #Open PDF Device
with(faithful, plot(eruptions, waiting))
title(main = "old Faithful Data")
dev.off() #Close PDF device

##Multiple devices can eb open at the same time
dev.cur() #To check current active device. Returns the numbr of the graphic device  >= 2
dev.set(2) #To switch to a particular graphic device

with(faithful, plot(eruptions, waiting))
title(main = "old Faithful Data")
dev.copy(png, file = "geysorplot.png") #Copy my plot to png
dev.copy2pdf(file = "geysor.pdf") #Direct export to PDF
dev.off()

#Swirl GGPLOT
