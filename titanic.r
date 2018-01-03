# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

#library(ggplot2) # Data visualization
#library(readr) # CSV file I/O, e.g. the read_csv function
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")

# Any results you write to the current directory are saved as output.

train <- read.csv("train.csv",stringsAsFactors=FALSE)
test <- read.csv("test.csv",stringsAsFactors=FALSE)

#str(train) # Shows data types and initial values
#table(train$Survived) # shows occurences of variable values
#prop.table(table(train$Survived)) #shows proportion of the above
#test$Survived <- rep(0,418) # creates survived column if not created else overwrites value
#table(test$Survived)
#table(train$Sex) 
#summary(train$Sex) # basic description of the variable
#table(train$Sex, train$Survived) # each value is that particular value divided by total population
#prop.table(table(train$Sex, train$Survived),1) #each value is the particular value divided by total population of that class
#prop.table(table(train$Sex, train$Survived),2)
# 1 signifies row
# 2 signifies column

#summary(train$Age)
#test$Survived <- 0
#test$Survived[test$Sex == 'female'] <- 1
#train$Child <- 0
#train$Child[train$Age < 18] <- 1
#train$Fare2 <- '30+'
#train$Fare2[train$Fare >= 20 & train$Fare < 30] <- '20-30'
#train$Fare2[train$Fare >= 10 & train$Fare < 20] <- '10-20'
#train$Fare2[train$Fare < 10] <- '<10'

#aggregate(Survived ~ Child + Sex, data=train, FUN=sum) # total survived IMPORTANT
#aggregate(Survived ~ Child + Sex, data=train, FUN=length) # total population
#aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})
#aggregate(Survived ~ Fare2 + Sex + Pclass, data=train, FUN=function(x){sum(x)/length(x)})

#test$Survived <- 0
#test$Survived[test$Sex == 'female'] <- 1
#test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
#head(test)

# Decision Trees
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data = train, method = "class")
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data = train, method = "class",
#             control = rpart.control(minsplit = 2,cp=0))# causes overfitting
#fancyRpartPlot(fit)
#Prediction <- predict(fit, test, type="class")

# Feature Engineering

test$Survived <- NA
combi <-rbind(train,test)
#strings are automatically imported in R as factors
combi$Name <- as.character(combi$Name) # converts factors to strings
combi$Name[1]
strsplit(combi$Name[1],split = '[,.]')[[1]] #splits the string that is either separated by comma or dot 
strsplit(combi$Name[1],split = '[,.]')[[1]][2]
# using sapply function we are running the above function over entire Name column of combi dataset
combi$Title <- sapply(combi$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][2]})
# remove beginning spaces from above
combi$Title <- sub(' ','',combi$Title)
table(combi$Title)
#submit <- data.frame(PassengerId <- test$PassengerId, Survived <- Prediction)
#names(submit) <- c("PassengerId","Survived")
#write.csv(submit, "DecisionTreeSubmission.csv", row.names=FALSE)