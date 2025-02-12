print("Hello World")
library(mlbench)
summary(Ionosphere)
head(Ionosphere)
df <- Ionosphere
###About the data: 351 Observations and 35 Independent Variables 
###Last column in the data is categorical variable called Class: good/bad


df <- subset(df, select = -V2) #Removed V2 bc all 0's

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