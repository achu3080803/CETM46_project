#Set the working directory. The bit between the "" needs to specify the path to the folder you wish to use
#you will see my file path below as am example
setwd("C:/Users/user/MScDataScience/CETM46/Tutorial11") # Note the single / (\\ will also work).

#Load the data. You may need to alter the file directory
Census.Data <-read.csv("practical_data.csv")

#left of the comma is the x-axis, right is the y-axis. We are using the $ command to select the columns of the data frame we want

plot(Census.Data$Unemployed,Census.Data$Qualification)

# includes axis labels
plot(Census.Data$Unemployed,Census.Data$Qualification,xlab="% in full time employment", ylab="% With a Qualification")

plot(Census.Data$Unemployed,Census.Data$Qualification,xlab="% in full time employment", ylab="% With a Qualification", pch = 19)

# Create a proportional symbols plot
symbols(Census.Data$Unemployed,Census.Data$Qualification,  circles = Census.Data$White_British, fg="white", bg ="purple", inches = 0.2) 

# bubble plot
symbols(Census.Data$Unemployed, Census.Data$Qualification,  circles = Census.Data$White_British, fg="white", bg ="purple", inches = 0.2,  xlab="% in full time employment", ylab="% With a Qualification") +
  # adds a regression line, sets the colour to red
  abline(lm(Census.Data$Qualification~ Census.Data$Unemployed), col="red") 

# a bubble plot with a dotted regression line
symbols(Census.Data$Unemployed, Census.Data$Qualification,  circles = Census.Data$White_British, fg="white", bg ="purple", inches = 0.2,  xlab="% in full time employmented", ylab="% With a Qualification") + abline(lm(Census.Data$Qualification~ Census.Data$Unemployed), col="red", lwd=2, lty=2) 

# Loads an installed package
library("ggplot2")

p <- ggplot(Census.Data, aes(Unemployed,Qualification))
p + geom_point()

p <- ggplot(Census.Data, aes(Unemployed,Qualification))
p + geom_point(aes(colour = White_British, size = Low_Occupancy))