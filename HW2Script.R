library(mlbench)
library(ISLR)
library(caret)#For KNN
library(lattice)#For visualizations also required by caret
library(ggplot2)#For graphs also required by caret

data("Ionosphere")
summary(Ionosphere)
head(Ionosphere)
df <- Ionosphere
###About the data: 351 Observations and 34 Independent Variables(Removed one)
###Last column in the data is categorical variable called Class: good/bad
df <- subset(df, select = -V2) #Removed V2 bc all 0's
str(df)

#Here are some graphical summaries of the Ionosphere data
hist(df$V7, main = "Histogram of V7 Occurance")
hist(df$V10, main = "Histogram of V10 Occurance")
boxplot(V7 ~ Class, data = df, col = c("blue", "green"), main = "Boxplot of V7 by Class")
boxplot(V10 ~ Class, data = df, col = c("blue", "green"), main = "Boxplot of V10 by Class")

#Here are the corresponding numerical summaries 
summary(df$V7[df$Class == "good"])
summary(df$V7[df$Class == "bad"])
summary(df$V10[df$Class == "good"])
summary(df$V10[df$Class == "bad"])

set.seed(4323)
#Data slicing
intrainQ3 <- createDataPartition(y = df$Class, p= 0.7, list = FALSE)
trainingQ3 <-df[intrainQ3,]
testingQ3 <- df[-intrainQ3,]


#checking to see if our dimensions add up 
dim(trainingQ3)
dim(testingQ3)

#
trControl <- trainControl(method  = "cv",
                          number  = 5)
fit <- train(Class ~ .,
             method     = "knn",
             trControl  = trControl,
             tuneGrid   = expand.grid(k = 1:10),
             data       = df)
fit

test_predictionQ3 <- predict(fit, newdata= testingQ3)
test_predictionQ3

confusionMatrix(test_predictionQ3, testingQ3$Class)

###########Q4

data("Auto")
summary(Auto)
attach(Auto)
mpg01 <- ifelse( mpg > median(mpg), yes = 1, no = 0)
newAuto <- data.frame(Auto, mpg01)
str(newAuto)

#Scatter plot matrix
pairs(newAuto)

#Here we standardized the data, since knn works on distance we don't want anything skewed
#cbind makes a matrix, apply, applies the scale (standardize) to the columns 2 not the rows 1
newAuto <- data.frame(mpg01, apply(cbind(cylinders, weight, displacement, horsepower, acceleration), 
                                2, scale), year)
str(newAuto)

intrainQ4 <- createDataPartition(y = mpg01, p= 0.7, list = FALSE)
trainingQ4 <-newAuto[intrainQ4,]
testingQ4 <- newAuto[-intrainQ4,]

dim(trainingQ4)
dim(testingQ4)

set.seed(1)

trControlQ4 <- trainControl(method  = "cv",
                          number  = 5)
fitQ4 <- train(as.factor(mpg01) ~ .,
             method     = "knn",
             trControl  = trControlQ4,
             tuneGrid   = expand.grid(k = 1:10),
             data       = newAuto)
fitQ4

test_predictionQ4 <- predict(fitQ4, newdata= testingQ4)
test_predictionQ4

confusionMatrix(test_predictionQ4, as.factor(testingQ4$mpg01))

###########Q5

head(Auto)
summary(Auto)
set.seed(1)

trControlQ5 <- trainControl(method  = "LOOCV",
                            number  = 5)
newAutoQ5 <- data.frame(Auto, mpg01)

fitQ5 <- train(as.factor(mpg01) ~ .,
               method     = "knn",
               trControl  = trControlQ5,
               tuneGrid   = expand.grid(k = 1:10),
               data       = newAutoQ5)
fitQ5

set.seed(1)
newAutoQ5Scaled <- data.frame(mpg01, apply(cbind(cylinders, weight, displacement, horsepower, acceleration), 
                                   2, scale), year)
fitQ5 <- train(as.factor(mpg01) ~ .,
               method     = "knn",
               trControl  = trControlQ5,
               tuneGrid   = expand.grid(k = 1:10),
               data       = newAutoQ5Scaled)
fitQ5
