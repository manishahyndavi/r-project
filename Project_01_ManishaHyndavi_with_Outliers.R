# Load the required packages
library(ggplot2)
library(reshape2)
library(MASS)
library(car)
library(tidyverse)
library(caret)

# DATA EXPLORATION

# Load the data set
splicingData <- read.csv("C:/Users/manis/OneDrive - Rutgers University/Semester 3/R Programming/Projects/splicing_data.csv",header = TRUE)
names(splicingData)

#Create a copy of data set as a safety purpose
splicingDatacopy <- (splicingData)
summary(splicingData)

# Check for missing values
splicingData[is.na(splicingData)]
# Total number of missing values in the data

# There are no missing values in the data

# Check for Outliers by both the box plot and the statistics
factor1outliers <- boxplot(splicingData$SplicingFactor1, ylab = "values", xlab = "Splicing Factor 1", main = "Graph Summary of Splicing Factor 1")
boxplot.stats(splicingData$SplicingFactor1)

factor2outliers <- boxplot(splicingData$SplicingFactor2, ylab = "values", xlab = "Splicing Factor 2", main = "Graph Summary of Splicing Factor 2")
boxplot.stats(splicingData$SplicingFactor2)

factor3outliers <- boxplot(splicingData$SplicingFactor3, ylab = "values", xlab = "Splicing Factor 3", main = "Graph Summary of Splicing Factor 3")
boxplot.stats(splicingData$SplicingFactor3)

eventoutliers <- boxplot(splicingData$SplicingEvent, ylab = "values", xlab = "Splicing Event",main = "Graph Summary of Splicing Event")
boxplot.stats(splicingData$SplicingEvent)


# Correlation matrix
cordata <- splicingData[ ,c("SplicingFactor1","SplicingFactor2","SplicingFactor3","SplicingEvent")]
cor(cordata)

# DATA VISUALIZATIONS

# Data Distribution by Histograms/ Density Plots

histogram <- ggplot(splicingData, aes(SplicingFactor1))
histogram + geom_histogram(bins = 20, binwidth = 0.3,fill = "firebrick",color = "gold")+ 
  labs(x = "Splicing Factor 1", y ="Frequency",title = " Distribution of Splicing Factor 1")

density <- ggplot(splicingData,aes(SplicingFactor1))
density + geom_density(color = "aquamarine3", fill = "aquamarine3")+ labs(x = "Splicing Factor 1", y = "Frequency", title = "Density Graph for Splicing Factor 1")


histogram1 <- ggplot(splicingData, aes(SplicingFactor2))
histogram1 + geom_histogram(bins = 20, binwidth = 0.4,fill = "darkcyan",color = "deeppink4")+ 
  labs(x = "Splicing Factor 2", y ="Frequency",title = " Distribution of Splicing Factor 2")

density1 <- ggplot(splicingData,aes(SplicingFactor2))
density1 + geom_density(color = "cornsilk4", fill = "cornsilk4")+ labs(x = "Splicing Factor 2", y = "Frequency", title = "Density Graph for Splicing Factor 2")


histogram2 <- ggplot(splicingData, aes(SplicingFactor3))
histogram2 + geom_histogram(bins = 20, binwidth = 0.3,fill = "navyblue",color = "floralwhite")+ 
  labs(x = "Splicing Factor 3", y ="Frequency",title = " Distribution of Splicing Factor 3")

density2 <- ggplot(splicingData,aes(SplicingFactor3))
density2 + geom_density(color = "burlywood4",fill ="burlywood4")+ labs(x = "Splicing Factor 3", y = "Frequency", title = "Density Graph for Splicing Factor 3")


histogram3 <- ggplot(splicingData, aes(SplicingEvent))
histogram3 + geom_histogram(bins = 30, binwidth = 0.8,fill = "darkgreen",color = "khaki2")+ 
  labs(x = "Splicing Event", y ="Frequency",title = " Distribution of Splicing Event")

density3 <- ggplot(splicingData,aes(SplicingEvent))
density3 + geom_density(color = "darkslategray",fill ="darkslategray")+ labs(x = "Splicing Event", y = "Frequency", title = "Density Graph for Splicing Event")



# Scatter plot

scatter <- ggplot(splicingData, aes(SplicingFactor1,SplicingEvent))
scatter + geom_point() + geom_smooth(alpha = 0.08, color = "red" )+
  labs(x ="Splicing Factor 1", y = "Splicing Event",title = "Relation b/w Splicing Factor 1 and Splicing Event")

scatter1 <- ggplot(splicingData, aes(SplicingFactor2,SplicingEvent))
scatter1 + geom_point() + geom_smooth(alpha = 0.08, color = "deeppink4" )+
  labs(x ="Splicing Factor 2", y = "Splicing Event",title = "Relation b/w Splicing Factor 2 and Splicing Event")

scatter2 <- ggplot(splicingData, aes(SplicingFactor3,SplicingEvent))
scatter2 + geom_point() + geom_smooth(alpha = 0.08, color = "firebrick" )+
  labs(x ="Splicing Factor 3", y = "Splicing Event",title = "Relation b/w Splicing Factor 3 and Splicing Event")

#Box Plot
newdata <- stack(splicingData, id = c(SplicingEvent), select = c(SplicingFactor1, SplicingFactor2, SplicingFactor3))
newdata <- cbind(newdata, splicingData$SplicingEvent)
colnames(newdata)[3] <- "SplicingEvent"


boxplot <- ggplot(newdata, aes(ind, SplicingEvent))
boxplot + geom_boxplot(color = "black",fill = "khaki2") + stat_boxplot(geom = "errorbar",width = 0.1, col= c("darkgreen", "red", "navyblue"))+ 
  labs(x = "Splicing Factor", y = "Splicing Event",title = "Distribution of splicing event values across different levels of each splicing factor")

# Split the data
set.seed(1234)
sample <- sample(c(TRUE, FALSE), nrow(splicingData), replace=TRUE, prob=c(0.7,0.3))
train  <- splicingData[sample, ]
test   <- splicingData[!sample, ]

# Multiple Linear Regression

model <- lm(SplicingEvent ~ SplicingFactor1 + SplicingFactor2 + SplicingFactor3, data = train)
summary(model)

# Linear Regression to find the individual variability

modelr <- lm(SplicingEvent ~ SplicingFactor1,data = train)
summary(modelr)

ggplot(train, aes(x = SplicingFactor1, y = SplicingEvent)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Simple Linear Regression: Splicing Factor 1 vs. Splicing Event",
       x = "Splicing Factor 1", y = "Splicing Event")

modelm <- lm(SplicingEvent ~ SplicingFactor2,data = train)
summary(modelm)

ggplot(train, aes(x = SplicingFactor2, y = SplicingEvent)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "limegreen") +
  labs(title = "Simple Linear Regression: Splicing Factor 2 vs. Splicing Event",
       x = "Splicing Factor 2", y = "Splicing Event")

modely <- lm(SplicingEvent ~ SplicingFactor3,data = train)
summary(modely)

ggplot(train, aes(x = SplicingFactor3, y = SplicingEvent)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "purple4") +
  labs(title = "Simple Linear Regression: Splicing Factor 3 vs. Splicing Event",
       x = "Splicing Factor 3", y = "Splicing Event")

# Run step AIC 

stepmodel <- stepAIC(model,direction = "both")
summary(stepmodel)
vif(stepmodel)
avPlots(stepmodel)
# Validate the test data

predictions <- predict(stepmodel,test)
predictions

# Add predicted values as a column to test data

test["PredictedValues"] <- predictions
View(test)

# Plot Predicted and actual values
plot(test$PredictedValues,test$SplicingEvent,xlab = "Actual Splicing Event Values",
     ylab = "Predicted Values", main = "Actual Splicing Event Vs Predicted Values")

# Adding a trend line;
abline(lm(test$Predicted ~ test$SplicingEvent))

# Evaluate the models performance


r_squared <- R2(test$PredictedValues, test$SplicingEvent)
print(paste("R-squared (R²) Score:", r_squared))

mse <- mean((test$PredictedValues - test$SplicingEvent)^2)
print(paste("Mean Squared Error (MSE):", mse))

rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))





