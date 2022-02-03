

#install.packages("readxl","dplyr" , "ggplot2", "caret")
library(readxl)
library(caret)
library(ggplot2)
library(dplyr)

data = read_excel("~/prospect.xls",sheet = "Sheet1")

# converting SEX variable into numerical data type.

table(data$SEX)
data$Sex_C = ifelse(data$SEX == "M",1,0)

# converting CLIMATE variable into numerical data type (dummy variables).

table(data$CLIMATE)
data$climate10 =  ifelse(data$CLIMATE == 10,1,0)
data$climate20 =  ifelse(data$CLIMATE == 20,1,0)

# AS per the question, excluding the fields "LOCATION" and "ID" before modeling
# and dropping categorical sex and climate
drop <- c("ID", "LOC", "CLIMATE", "SEX")
data = data[,!(names(data) %in% drop)]

# renaming FICO>=700
names(data)[5] = "CreditScore"

# checking null
is.null(data)
# as there is no null values, so no modification is required

# checking na
is.na(data)
#But there are na values, we will deal with them later (104 line)

#Outlier detection and removal
# IQR method

# 1: Age
outliers= boxplot(data$AGE)$out
datanoOut = ifelse(data$AGE %in% outliers, NA, data$AGE)
boxplot(datanoOut)
summary(datanoOut)
data$AGE_new = datanoOut
data$AGE_new[is.na(data$AGE_new)] = mean(data$AGE_new, na.rm = T)
sum(is.na(data$AGE_new))
drop <- c("AGE")
data = data[,!(names(data) %in% drop)]
names(data)[8] = "AGE"
outliers= boxplot(data$AGE)$out

# 2: Income
outliers= boxplot(data$INCOME)$out
datanoOut = ifelse(data$INCOME %in% outliers, NA, data$INCOME)
boxplot(datanoOut)
summary(datanoOut)
data$Income_new = datanoOut
data$Income_new[is.na(data$Income_new)] = mean(data$Income_new, na.rm = T)
sum(is.na(data$Income_new))
drop <- c("INCOME")
data = data[,!(names(data) %in% drop)]
names(data)[8] = "INCOME"
outliers= boxplot(data$INCOME)$out

# normalize AGE variable
data_num = data[,7]

max = max(data_num)
min = min(data_num)

data$AGE_Normalised = scale(data_num, center = min, scale = max - min)
drop <- c("AGE")
data = data[,!(names(data) %in% drop)]

names(data)[8] = "AGE"

# normalize INCOME variable
data_num = data[,7]

max = max(data_num)
min = min(data_num)

data$INCOME_Normalised = scale(data_num, center = min, scale = max - min)
drop <- c("INCOME")
data = data[,!(names(data) %in% drop)]
names(data)[8] = "INCOME"

#****************************************************************
# Variable Distribution

names(data)[4] = "SEX"

summary(data$SEX) # 106 NA
summary(data$MARRIED) #106 NA
summary(data$OWNHOME) #106 NA
summary(data$CreditScore) #106 NA

# NA removed
data = data[!is.na(data$SEX) 
            |!is.na(data$MARRIED) |!is.na(data$OWNHOME) |!is.na(data$CreditScore)  , ]


# histogram for quantitative variables
hist(data$AGE)
hist(data$INCOME)
summary(data$AGE)
summary(data$INCOME)

#---------------------------------------MODELING---------------------------

#--------PartA------------------------------

set.seed(7)
kml <- kmeans(data,4,nstart =100)
kml

plot(data, kml$cluster , main = "K-Means Clustering Model")

# Clusters size
kml$size


# Cluster Variance
kml$withinss

#Cluster means
kml$centers 


#------PartB--------
# For each of the four clusters, briefly describe the characteristics of members of that cluster.
# Hint: You can check how different variables vary in each cluster.

# from normalized income and age, we are converting back income and age to get mean values 
# {normalized value * (max - min) + min}

# we are estimating below values using kml$centers 

# Cluster1 - Group belonging to cluster1 contains 57% married members, 31% of them own a house, 
# 38% of these have a credit score higher than 700 , this cluster has 56% male,
# all the people lives in climate code 10 , average age is 47yo and average annual income is 
# around 42k

# Cluster2 - Group belonging to cluster2 contains all  married members, 16% of them own a house, 
# nobody have a credit score more than 700, this cluster has 48% male,
# 77% of people lives in climate code 20 and 23% lives in climate code 30, average age is 
# around 54 yo and average annual income is around 39k.

# Cluster3 - 83%  members of this cluster are married, 56% of them own a house, 
# all of them have a credit score more than 700, this cluster has 54% male,
# 76% of people lives in climate code 20 and 24% lives in climate code 30, average age is 
# around 50 yo and average annual income is around 46k

# Cluster4 - all the  members of this cluster are married, 20% of them own a house, 
# 12% has a credit score more than 700, this cluster has 53% male,
# 76% of people lives in climate code 20 and 24% lives in climate code 30, average age is 
# around 42 yo and avergae annual income is around 42k

# c: What is the best value of k for this data set?------

mydata <- data
wss <- (nrow(mydata) -1)*sum(apply(mydata,2,var))
for(i in 1:15)
  wss[i] <- kmeans(mydata, centers = i)$withinss

plot(1:15, wss, type = "b",xlab = "numbers of clusters", ylab = "within groups  sum of squares",
     pch = 20,  cex = 2)

#Optimal K - 4  After this there is no change in wss (within sum of squares).

# d: What is the Silhouette measure of the clusters obtained by best k
library(cluster)

avg_sil = function(k){
  kmModel = kmeans(data, centers = k, nstart = 100)
  ss = silhouette(kmModel$cluster, dist(data))
  mean(ss[, 3 ])
}

avg_sil(4)

# ----------------------------------END---------------------------
