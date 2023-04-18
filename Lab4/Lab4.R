# https://rpubs.com/Mentors_Ubiqum/Filter_Transactions 
rm(list=ls())
setwd(getwd())

# install.packages("arules")
# install.packages("arulesViz")
library("arules")
library("arulesViz")

# 3.Load the transactions in the file AssociationRules.csv using the function  read.transactions. 
# Make sure you don’t include the header line in the dataset. 
# The file is in the same folder as the R script.
data <- read.transactions("AssociationRules.csv",  header = FALSE)

# 4.Display the transactions in a readable format using the function inspect. Display 
# only the first 100 transactions. 
inspect(data[1:100])


# 5.What are the most frequent two items in the dataset? What are their frequencies?
# Hint: use the function itemFrequency or use the function summary.
items<-itemFrequency(data, type = "absolute")
print(head(sort(items, decreasing = TRUE), n=2))

# 6.Plot the 5 most frequent items of the transactions using the function itemFrequencyPlot
itemFrequencyPlot(data, topN = 5, type = "absolute", horiz=TRUE,col='steelblue3',xlab='',)

# 7. Generate the association rules from the transactions using the apriori algorithm. Set 
# the minimum support = 0.01, minimum confidence = 0.5, minimum cardinality 
# (number of items in the rule) = 2. Use the function apriori

rules <- apriori(data, parameter = list(supp = 0.01, conf = 0.5 , minlen = 2))
# 8.Now, sort the generated rules by support. Search the function sort found in the 
# arules package. Show only the first 6 rules
inspect(sort(rules, by = "support", decreasing = TRUE)[1:6])

# 9. Sort the generated rules by confidence. Show only the first 6 rules
inspect(sort(rules, by = "confidence", decreasing = TRUE)[1:6])

# 10. Sort the generated rules by lift. Show only the first 6 rules.
inspect(sort(rules, by = "lift", decreasing = TRUE)[1:6])


# 11.  Plot the generated rules with support as x-axis, confidence as y-axis and lift as 
# shading. Use the function plot in arules package.
plot(rules,measure=c("support","confidence"),shading="lift",jitter=0)
 
# 12. Based on (8-11), Can you tell now what are the most interesting rules that are really 
# useful and provide a real business value and an insight to the concerned corporate?

