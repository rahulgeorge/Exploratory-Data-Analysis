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


#Lattice Plotting System
library(lattice)
library(datasets)

xyplot(Ozone ~ Wind, data = airquality) #Scatterplot
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1)) #Creates plot across all month levels
p <- xyplot(Ozone ~ Wind, data = airquality)
print(p) #Lattice system returns an objet of class trellis. It can be stored and printed later. But better to generate and print

#Lattice Panel Functions
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each =50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2,1)) #Plot with panels

xyplot(y ~ x |f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...) #Call the default panel function for xyplot
        panel.abline(h = median(y), lty = 2) #Adding horizontal line at median
        panel.lmline(x, y, col = 2) #Adding a regression line
})



#GGPLOT2 Package
library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg) #Basic plot with quickplot function
qplot(displ, hwy, data = mpg, color = drv) #Legend prepared and graph colored as per dev variable
qplot(displ, hwy, data = mpg, geom = c("point","smooth"), color = drv) #Adding a geometric object which maps all the points and is smooth 
qplot(hwy, data = mpg, fill = drv) #Histogram by specifying just one varibale. Colored by drv

qplot(displ, hwy, data = mpg, facets = .~drv) #Similar to panels. Split by drv where facets take 2 argument. Left of ~ is rows variable and right if columns
qplot(hwy, data = mpg, facets = drv~., binwidth = 2) #Set the width of each bin

load(file = "./data/maacs.rda")
str(maacs)
qplot(log(eno), data = maacs, fill = mopos) 
qplot(log(eno), data = maacs, geom = "density")
qplot(log(eno), data = maacs, geom = "density", color = mopos)

qplot(log(pm25),log(eno),data=maacs, shape = mopos, color = mopos)
qplot(log(pm25),log(eno),data=maacs, shape = mopos, color = mopos) + geom_smooth(method = "lm")
qplot(log(pm25), log(eno), data = maacs, facets = .~mopos) + geom_smooth(method = "lm")

qplot(drv, hwy, data = mpg, geom = "boxplot") #Boxplot in GGPLOT
qplot(drv, hwy, data = mpg, geom = "boxplot", color = manufacturer)

##GGPLOT2 Components
## 1. Aesthetic mappings - data to color and size
## 2. Geoms - lines, points, shapes
## 3. Facets - conditional plot
## 4. Stats - Statistical transformation such as quantile, smoothing
## 5. Scales - aesthetic map scale, legend (ex. male = red, female = blue)
## 6. Coordinate System

str(maacs)
qplot(log(pm25), )

#Using below code to generate dummy maacs data
                bootstrapMissingVars <- function(){
                      
                        #some variables have been removed for privacy reasons so
                        #I'm gonna "bootstrap" a fake variables in their place,
                        #based off the graphs I can see
                        
                        library(dplyr)
                        #After some quick arthmetic, it seems logpm25 
                        #used log base 10
                        maacs <- mutate(maacs, logpm25 = log(pm25, 10))
                        
                        #Generating NocturnalSympt
                        # No values below 0 were shown so I figured I'd drop these
                        # And get rid of any NA values at this point
                        maacs <- maacs[maacs$logpm25>0 & !is.na(maacs$logpm25),] 
                        # I can kind of determine the first few and last bits 
                        # of data. So I'm going to manully enter them since 
                        # both sides didn't have many higher values, 
                        # as such these data will simulate the original 
                        # graphs a bit more accurately
                        maacs <- arrange(maacs, logpm25)
                        firstFew <- c(rep(0,7),2,0,3) # < 0.6
                        lastBit <- c(2,0,0,2,0,0,3,3,0,4,4,4,3) # >2
                        explicitNum <- sum(length(firstFew), length(lastBit))
                        #I determined these values by comparing the visual 
                        #on the graph and how the points were distributed, 
                        #it seems some values of pm25 were changed from the plots 
                        #shown, or the base of log wasn't 10
                        
                        #For the rest of the values I'm going to 
                        #do my best to count how many I see on the graph then
                        #randomly sample those values to generate NocturnalSympt
                        freqs <- c(42,44,10,11,10,5,6,8,1,2,0,0,0,11)
                        eyeCount <- c(rep(0, (length(maacs$logpm25) - sum(freqs, 
                                                                          explicitNum))),
                                      rep(1, freqs[1]), rep(2, freqs[2]), 
                                      rep(3, freqs[3]), rep(4, freqs[4]),
                                      rep(5, freqs[5]), rep(6, freqs[6]), 
                                      rep(7, freqs[7]), rep(8, freqs[8]), 
                                      rep(9, freqs[9]), rep(10, freqs[10]), 
                                      rep(14, freqs[14]))
                        
                        sim <- sample(eyeCount, 
                                      length(maacs$logpm25) - explicitNum)
                        maacs <- mutate(maacs, 
                                        NocturnalSympt = c(firstFew, sim, lastBit))
                        
                        #The final variable to simulate is bmicat, I'm going to use
                        #The data from this
                        #(https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1925022/)
                        #study and say the random sample was 
                        #representative of the population the study obtained 
                        #a point estimate of 0.42% (25/60) overweight children.
                        #I'll use this point estimate to create a 
                        #random dist of overweight for maacs
                        p <- 25/60
                        m <- length(maacs$logpm25)*p
                        
                        randDist <- rnorm(length(maacs$logpm25), mean = m,
                                          sd = sqrt((p*(1-p))/length(maacs$logpm25)))<m
                        l <- c("overweight", "normal weight")
                        maacs <- mutate(maacs, 
                                        bmicat = factor(ifelse(randDist, l[1], l[2]),
                                                        levels = c(l[1],l[2])))
                        
                        ## And that'll do it, now just to realign the data
                        maacs <- arrange(maacs, id)
                        return(maacs)}

maacs2 <- bootstrapMissingVars()
g <- ggplot(maacs2, aes(logpm25, NocturnalSympt)) #Creating the base
summary(g) #Can look at summary of ggplot2
print(g) #Nothing to print as only the base has been setup
p <- g + geom_point() #Adding points to the based onject and assigning to a new variable
print(p)

g <- ggplot(maacs2, aes(logpm25, NocturnalSympt))
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method = "lm")
g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~bmicat) + labs(title = "Plot Title", subtitle = "Wow great subtitle", caption = "Caption 1", tag = "Tag!!")

##Labels - xlab(), ylab(), labs(), ggtitle
##Geom functions can be individually modified as required
##Use global options to set general theme - use theme()

g + geom_point(color = "steelblue", size = 4, alpha = 1/2) #Alpha is the transperancy #Assigning color to be a constant here
g + geom_point(aes(color = bmicat), size =  4, alpha = 1/2) #Assigning color to be a variable using the aes function

g + geom_point(aes(color = bmicat)) + labs(title = "MAACS Cohort") + labs(x = expression("log"*PM[2.5]), y = "Nocturnal Symptoms") + geom_smooth(size = 2, linetype = 4, method = "lm", se = FALSE)
g + geom_point(aes(color = bmicat)) + theme_bw(base_family = "Times") #Applying a theme

#Axis Limits

testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100 #Outlier
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()
g + geom_line() + ylim(-3,3) #Not the right way as GGPLOT will just filter for values between -3 & 3
g + geom_line() + coord_cartesian(ylim = c(-3,3)) #This is right way

#Cutting a continous varibale to sets
cutpoints <- quantile(maacs2$logpm25, seq(0,1, length = 4), na.rm = TRUE) #using quantile to find cutpoints
maacs2$pmCat <- cut(maacs2$logpm25, cutpoints) #Cutting
levels(maacs2$pmCat) #Checking the new variable

g <- ggplot(maacs2, aes(logpm25, NocturnalSympt))
g + 
        geom_point(alpha = 1/2) + 
        geom_smooth(method = "lm") + 
        facet_grid(bmicat~pmCat) + 
        labs(title = "MAACS Cohort", x = expression("log"*PM[2.5]), y = "Nocturnal Symptoms")

#Swirl Colors Lesson
pal <- colorRamp(c("red", "blue"))
pal(seq(0,1,len = 6)) #Returns 6 colors
p1 <- colorRampPalette(c("red","blue"))
p1(6) #Returns 6 colors
p3 <- colorRampPalette(c("blue","green"), alpha = 0.5)

cols <- brewer.pal(3, "BuGn")
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))
image(volcano, col = p1(20))

