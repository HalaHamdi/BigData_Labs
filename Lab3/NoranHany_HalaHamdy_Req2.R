#  Name      Sec BN
# Noran Hany  2 34
# Hala Hamdy  2 35


setwd(getwd())

# install.packages("rpart.plot")
# install.packages("ROCR")
library("rpart")
library("rpart.plot")
library("ROCR")

#Read the data
play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
play_decision
summary(play_decision)

#Build the tree to "fit" the model
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3),
             parms=list(split='information'))
# split='information' : means split on "information gain" 
#plot the tree
rpart.plot(fit, type = 4, extra = 1)

summary(fit)
#######################################################################################
# Q1: what is the defult value for split?    
# A1:The default value is gini 
# which is a metric used in deciding on the attribute to branch the tree upon 
# and is calculated by subtracting the sum of the squared probabilities of each class from one.
# It favours mostly the larger partitions

# Q2: what are the meanings of these control parameters?  
#          1- "minsplit=2"
#           A2.1: it is the minimum number of observations that must exist in a node in order for a split to be attempted
#           so here it is 2 so it will split as long as there is 2  observations in the node 
#           for the next graph (fit1) havinf g minsplit=4  will result on not splitting the left hand side of the tree
#           because it has only 3 observations
fit1 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=4, maxdepth = 3),
             parms=list(split='information'))
rpart.plot(fit1, type = 4, extra = 1)

#          2- "maxdepth=3" 
#            A2.2: it is the maximum depth of any node of the final tree, with the root node counted as depth 0
#            so maxdepth=3 means the tree will have 4 levels (root + 3 levels) at most
#            for the next tree we will set maxdepth=1 so the tree will have only 2 levels that will 
#            split only once on attribute Temperature    
fit2 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 1),
             parms=list(split='information'))
rpart.plot(fit2, type = 4, extra = 1)

#          3- "minbucket=4" 
#           A2.3: the minimum number of classes of the observations in any terminal (leaf node. If only
#           one of minbucket or minsplit is specified, the code either sets minsplit to
#           minbucket*3 or minbucket to minsplit/3
#           so for the fit3 , minbucket=2 so it will give the graph of a tree where the terminal nodes having atleast 2 classes 
#           of observations 
fit3 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minbucket=2, maxdepth = 3),
             parms=list(split='information'))
rpart.plot(fit3, type = 4, extra = 1)
# Support your answers with graphs for different values of these parameters.




#Q3: What will happen if only one of either minsplit or minbucket is specified
#    and not the other?

#A3:        If only one of minbucket or minsplit is specified, the code either sets minsplit to
#           minbucket*3 or minbucket to minsplit/3

#Q4: What does 'type' and 'extra' parameters mean in the plot function?
#A4:  type  is for specifying the type of plot to be drawn. It can take one of the following params:
#     0 --> Draw a split label at each split and a node label at each leaf.
#     1 --> Label all nodes, not just leaves. Similar to text.rpart’s all=TRUE.
#     2 --> Default. Like 1 but draw the split labels below the node labels.
#     3 --> Draw separate split labels for the left and right directions.
#     4 --> Like 3 but label all nodes, not just leaves.
#     5 --> Show the split variable name in the interior nodes.

#    extra is for specifying the extra information to be displayed in the plot. It can take one of the following params:
# 0 --> No extra information.
# 1 --> Display the number of observations that fall in the node 
# 2 --> Class models: display the classification rate at the node, expressed as the
# number of correct classifications and the number of observations in the node.
# 3 --> Class models: misclassification rate at the node, expressed as the number of
# incorrect classifications and the number of observations in the node.
# 4 --> Class models: probability per class of observations in the node 
# 5 --> Class models: like 4 but don’t display the fitted class.
# 6 --> Class models: the probability of the second class only. Useful for binary responses.
# 7 --> Class models: like 6 but don’t display the fitted class.
# 8 --> Class models: the probability of the fitted class.
# 9 --> Class models: The probability relative to all observations – the sum of these
# probabilities across all leaves is 1. This is in contrast to the options above, which
# give the probability relative to observations falling in the node – the sum of the
# probabilities across the node is 1.
# 10 --> Class models: Like 9 but display the probability of the second class only.
# Useful for binary responses.
# 11 --> Class models: Like 10 but don’t display the fitted class.



#Q5: Plot the tree with propabilities instead of number of observations in each node.
fit4 <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3),
             parms=list(split='information'))
rpart.plot(fit4, type = 4, extra = 4)
######################################################################################
 
#Predict if Play is possible for condition rainy, mild humidity, high temperature and no wind
newdata <- data.frame(Outlook="overcast",Temperature="mild",Humidity="high",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))
# type can be class, prob or vector for classification trees.

######################################################################################
#Q6: What is the predicted class for this test case? 
# A6: yes

#Q7: State the sequence of tree node checks to reach this class (label).
# A7: 1- Temperature = mild   --> so we go to the LHS of tree
#     2- Outlook = overcast   --> so we go to the RHS of tree 
#     finally we are at the leafs and we have 0 observations for no and 1 observations for yes
#     so the predicted class is yes


## ================================= END ===================================== ##
