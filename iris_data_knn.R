# Brady Lange
# R Script 4
# Course: CSIS 239
# 4/14/18
# This program trains and tests rigorously the kNN algorithm with iris data to classify the species of flower.

graphics.off()
rm(list = ls())
setwd("D:/Users/Brady Lange/Downloads")
library(class)

# 1.)
# Displaying the first 6 rows of iris and printing the summary
irisData <- iris
head(irisData)
summary(irisData)

# 2.)
# Installing the library ggplot2
install.packages("ggplot2")
library(ggplot2)
# Scatter plot of the iris data set
ggplot(irisData, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point()

# 3.)
# Creating a new data plot of the iris data set, with petal length (x) and petal width (y)
ggplot(irisData, aes(x = Petal.Length, y = Petal.Width, colour = Species)) + geom_point()

# 4.)
# Observing if the data had clustering behavior
# The data for the Sepal and Petal plot had clustering behavior
# The different species seemed to all cluster in the same general area

# 5.)
# Rescaling the iris data set so that I can use the kNN algorithm to classify the species it will be
irisData[ ,1:4] <- scale(irisData[ ,1:4])

# 6.)
# Splitting the iris data into 80% training and 20% testing
irisDataSize <- nrow(irisData)
sampling.rate <- 0.8
# Training data:
# Training data size
numTrain <- irisDataSize * sampling.rate
# Randomly selecting training data
train_idx <- sample(1:irisDataSize, numTrain, replace = F)
# Selecting the rows 
train_data <- irisData[train_idx, 1:4]
# True classification labels
train_labels <- irisData$Species[train_idx]

# Testing data:
# Testing data size
numTest <- (1 - sampling.rate) * irisDataSize
# Randomly selecting testing data that aren't in the training set
test_idx <- setdiff(1:irisDataSize, train_idx)
# Selecting the rows
test_data <- irisData[test_idx, 1:4]
# Species label for the test set
test_true_labels <- irisData$Species[test_idx]

# 7.)
# Using the kNN alorithm to classify the flower species with the four attributes
test_species_classi <- knn(train_data, test_data, train_labels, k = 3)

# 8.)
# Assessing the accuracy of the model by outputting misclassification rate
knnAcc <- sum(test_species_classi != test_true_labels) / numTest

# 9.)
# Testing a range of k values to find the most beneficial one for the algorithm
misclassification_rate <- numeric(20)
for(k in 1:20)
{
  print(k)
  # Training the model with different amounts of neighbors
  test_predicted_labels <- knn(train_data, test_data, train_labels, k)
  # Amount of incorrect labels produced
  num_incorrect_labels <- sum(test_predicted_labels != test_true_labels)
  # Converting the amount of labels incorrect into a percentage
  misclassification_rate[k] <- num_incorrect_labels / numTest
  print(paste(k, "=", misclassification_rate[k]))
}

# 10.)
# Optimizing k with several test sets
# The variable i keeps track of the amount of random data sets that are created 
for(i in 1:1000)
{
  # Splitting the iris data into 80% training and 20% testing
  irisDataSize <- nrow(irisData)
  sampling.rate <- 0.8
  # Training data:
  # Training data size
  numTrain <- irisDataSize * sampling.rate
  # Randomly selecting training data
  train_idx <- sample(1:irisDataSize, numTrain, replace = F)
  # Selecting the rows 
  train_data <- irisData[train_idx, 1:4]
  # True classification labels
  train_labels <- irisData$Species[train_idx]
  
  # Testing data:
  # Testing data size
  numTest <- (1 - sampling.rate) * irisDataSize
  # Randomly selecting testing data that aren't in the training set
  test_idx <- setdiff(1:irisDataSize, train_idx)
  # Selecting the rows
  test_data <- irisData[test_idx, 1:4]
  # Species label for the test set
  test_true_labels <- irisData$Species[test_idx]
  
  k <- 1
  for(k in 1:20)
  {
    # Training the model with different amounts of neighbors
    test_predicted_labels <- knn(train_data, test_data, train_labels, k)
    # Amount of incorrect labels produced
    num_incorrect_labels <- sum(test_predicted_labels != test_true_labels)
    # Converting the amount of labels incorrect into a percentage
    misclassification_rate[k] <- num_incorrect_labels / numTest
    optimalK[i,k] <- misclassification_rate[k]
  }
}

# 11.)
# Taking the averages of each column 
kAvg <- colMeans(optimalK)

# 12.)
# Creating a scatter plot of mean misclassification rates for each value of k
plot(1:length(kAvg), kAvg, ylab = "Average Misclassification Rates", xlab = "Index of K", main = "Misclassification Rates by k-Clusters")

