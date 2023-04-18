#  Name      Sec BN
# Noran Hany  2 34
# Hala Hamdy  2 35


#  1. First of all, start by cleaning the workspace and setting the working directory 
rm(list=ls())
# install.packages("e1071")
library("e1071")

# 2. Import the dataset nbtrain.csv into a data frame. What are the variables of this data set?
sample <- read.table("nbtrain.csv", header=TRUE, sep=",")
# A2: The vars used are  "age","gender","educ","income"
vars <-sample[0,]

# 3. Divide the data into two data frames: a training set containing the first 9000 rows,
# and a test set containing the remaining rows.
# Why do we split data into training and test sets?
traindata <- as.data.frame(sample[1:9000,])
testdata <- as.data.frame(sample[9001:nrow(sample),])
# A3: We split data into training and test sets to test the accuracy of the model.


# 4. Train a Naïve Bayes Classifier model with income as the target variable and all
# other variables as independent variables. Smooth the model with Laplace
# smoothing coefficient = 0.01
# What does Laplace smoothing coefficient mean?
model1 <- naiveBayes(income ~.,traindata)
model2 <- naiveBayes(income ~.,traindata,laplace=.01)
# A4: Laplace smoothing is a smoothing technique that handles the problem of zero probability in Naïve Bayes
# Whatever positive integer this is set to will be added into for every class.

# 5. Display the resulting model.
print(model2)

# 6. Use the model to predict the income values of the test data
results2 <- predict (model2,testdata)


# 7. Display a confusion matrix for the predict values of the test data versus the actual values. 
# Investigate the results.Explain the variation in the model’s classification power across income classes.
# stats <- confusionMatrix(data=results2, reference = as.factor(testdata$income))
conf_matrix=table(results2,testdata$income)
print(conf_matrix)
# rows are predictions , cols are actual
# Prediction 10-50K 50-80K GT 80K
#     10-50K    797    127     67
#     50-80K      0      0      0
#     GT 80K      6      5      8
# The model fails completely to classify the second class 50-80K 
#  and managed to classify almost all in first class 10-50K (797 correctly classified and only 6 missclassified)
#  finally, it only managed to get 8 out of (67+8=75) were correctly classified. 
# so it basically classifies most of the data in 1st class


# 8.Display the accuracy of the model. Comment on the result.
# it can be seen in the stats table above 
# comment on the result of accuracy 
accuracy= (conf_matrix[1]+conf_matrix[5]+conf_matrix[9])/sum(conf_matrix)
print(accuracy)
# A8: The accuracy of the model is 0.797
# The majority class(10-50K) was almost correctly classified. 
# The 2 other classes although most of them are missclassified, they represent a minority comparing to the 10-50k class.
# Due to the way accuracy neglects the frequency of each class, the accuracy is dominated by the majority good class.
# By observing the 50-80k class, the model failed to predict any example with this class, 
# however, the accuracy doesn't reflect this bad performance of the model.

# 9.Display the overall 10-50K, 50-80K, GT 80K misclassification rates
columns=colSums(conf_matrix)
missclass_1= (conf_matrix[2]+conf_matrix[3])/columns[1]
missclass_2= (conf_matrix[4]+conf_matrix[6])/columns[2]
missclass_3= (conf_matrix[7]+conf_matrix[8])/columns[3]
print(missclass_1)
print(missclass_2)
print(missclass_3)