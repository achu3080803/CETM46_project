#Set the working directory. The bit between the "" needs to specify the path to the folder you wish to use
#you will see my file path below as am example
setwd("C:/Users/user/MScDataScience/CETM46/Tutorial11") # Note the single / (\\ will also work).

#Load the data. You may need to alter the file directory
Census.Data <-read.csv("practical_data.csv")

# Runs a Pearson's correlation
cor(Census.Data$Unemployed, Census.Data$Qualification)

# Runs a Pearson's correlation
cor.test(Census.Data$Unemployed, Census.Data$Qualification)

# Runs a Spearman's correlation
cor.test(Census.Data$Unemployed, Census.Data$Qualification, method="spearman")

# creates a data1 object which does not include the 1st column from the original data 
data1 <- Census.Data[,2:5]

# creates correlation matrix
cor(data1)

# creates correlation matrix
round(cor(data1),2)

library(ggplot2) # should already be opened from the previous stage
library(reshape2)

# our correlation matrix
corr <- cor(data1)

# creates a qplot from our corr matrix
qplot(x=Var1, y=Var2, data=melt(corr), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))

install.packages('corrplot')
library(corrplot)

# creates a lower triangular corrplot from our corr matrix
corrplot(corr, type="lower", tl.col="black", tl.srt=45)

# runs a regressions model (y ~ x, data frame)
model_1 <- lm(Qualification~ Unemployed, Census.Data)

plot(Census.Data$Unemployed, Census.Data$Qualification, xlab="% Unemployed", ylab="% With a Qualification") + abline (model_1)

summary(model_1)

predict(model_1,data.frame(Unemployed= 10), interval = "confidence")

summary(model_1)
confint(model_1, level= 0.95)

# a multiple regression model
model_2 <- lm(Qualification ~ Unemployed + White_British, Census.Data)

summary(model_2)

