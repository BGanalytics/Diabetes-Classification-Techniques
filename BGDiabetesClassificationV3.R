# Date: 27/08/2020
# Author: Bahareh Golfar
# Version 3 

# Description: KNN classification Analysis of Diabetes in women, calssifying their risk based on age
#             and body mass index
#             https://www.kaggle.com/vincentlugat/pima-indians-diabetes-eda-prediction-0-906/notebook
# About this file
#This dataset describes the medical records for Pima Indians
#and whether or not each patient will have an onset of diabetes within ve years.
#Fields description follow:
#             preg = Number of times pregnant
#             plas = Plasma glucose concentration a 2 hours in an oral glucose tolerance test
#             pres = Diastolic blood pressure (mm Hg)
#             skin = Triceps skin fold thickness (mm)
#             test = 2-Hour serum insulin (mu U/ml)
#             mass = Body mass index (weight in kg/(height in m)^2)
#             pedi = Diabetes pedigree function
#             age = Age (years)
#             class = Class variable (1:tested positive for diabetes, 0: tested negative for diabetes)


# Data Preprocessing 

rm(list=ls())
# Importing the dataset
data = read.csv('diabetes_csv.csv')
dataset=data[,c(1,6,8,9)]

# Taking care of missing data

dataset$preg = ifelse(is.na(dataset$preg),
                      ave(dataset$preg, FUN = function(x) mean(x, na.rm = TRUE)),
                      dataset$preg)

dataset$age = ifelse(is.na(dataset$age),
                     ave(dataset$age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$age)

dataset$mass = ifelse(is.na(dataset$mass),
                      ave(dataset$mass, FUN = function(x) mean(x, na.rm = TRUE)),
                      dataset$mass)

Minpreg=min(dataset$preg)
Maxpreg=max(dataset$preg)

Minmass=min(dataset$mass)
Maxmass=max(dataset$mass)

#Creating density plots for healthy and diabetic individuals
library(ggplot2)

sPreg<-ggplot(data=dataset, aes(x=preg , color=class)) + 
  geom_density()+
  ggtitle('Plot 1: Density Plot for Number od Pregnancies') 

sPreg

sAge<-ggplot(data=dataset, aes(x=age , color=class)) + 
  geom_density()+
  ggtitle('Plot 2: Density Plot for Age') 

sAge

library(ggplot2)
sMass<-ggplot(data=dataset, aes(x=mass , color=class)) + 
  geom_density()+
  ggtitle('Plot 3: Density Plot for BMI') 

sMass

# Encoding categorical data
dataset$class = factor(dataset$class,
                       levels = c('tested_negative', 'tested_positive'),
                       labels = c(0, 1))


#Correlation matrix and plot
df1=data.frame(dataset[,-4])
#install.packages("corrplot")
library(corrplot)
Mcormatrix<- cor(df1, method = "pearson", use = "complete.obs")
corrplot(Mcormatrix, method="circle")

palette = colorRampPalette(c("yellow", "orange", "red")) (20)
heatmap(x = Mcormatrix, col = palette, symm = TRUE)

##################################################################
Newdataset=dataset[,-3]

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(Newdataset$class, SplitRatio = 0.75)
training_set = subset(Newdataset, split == TRUE)
test_set = subset(Newdataset, split == FALSE)



# Feature Scaling
training_set[,-3] = scale(training_set[,-3])
test_set[,-3] = scale(test_set[,-3])



# Fitting Kernel SVM to the Training set
#install.packages('e1071')
library(e1071)
classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[,-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)


# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('preg', 'mass')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
     xlab = 'preg', ylab = 'mass',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'tomato', 'yellow'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'red3', 'yellow3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('preg', 'mass')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Kernel SVM (Test set)',
     xlab = 'preg', ylab = 'mass',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'tomato', 'yellow'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'red3', 'yellow3'))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#trying randomforest classificaton

library(randomForest)
set.seed(123)
classifier2 = randomForest(x = training_set[,-3],
                           y = training_set$class,
                           ntree = 500)

# Predicting the Test set results
y_pred2 = predict(classifier2, newdata = test_set[,-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred2)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('preg', 'mass')
y_grid = predict(classifier2, grid_set)
plot(set[, -3],
     main = 'Random Forest Classification (Training set)',
     xlab = 'Number of pregnancies', ylab = 'BMI',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('preg', 'mass')
y_grid = predict(classifier2, grid_set)
plot(set[, -3], main = 'Random Forest Classification (Test set)',
     xlab = 'Number of Pregnancies', ylab = 'BMI',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))




