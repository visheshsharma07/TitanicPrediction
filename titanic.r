# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

#library(ggplot2) # Data visualization
#library(readr) # CSV file I/O, e.g. the read_csv function
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
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
# combing redundant titles
combi$Title[combi$Title %in% c('Mine','Mile')] <- 'Mile'
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona','Lady','the Countess','Jonkheer')] <- 'Mile'
# Change the type back to factor
combi$Title <- factor(combi$Title)
# getting family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][1]})
# create family id
combi$FamilyId <- paste(as.character(combi$FamilySize),combi$Surname,sep="")
combi$FamilyId[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyId)
famId <- data.frame(table(combi$FamilyId))
famId <- famId[famId$Freq <= 2,]
combi$FamilyId[combi$FamilyId %in% famId$Var1] <- 'Small'
combi$FamilyId <- factor(combi$FamilyId)
train <- combi[0:891,]
test <- combi[892:1309,]
# Decision trees are biased to favour factors with many levels
# A variable with more distinct values will be prefered over others
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyId, data=train, method = "class")
#fancyRpartPlot(fit)
Prediction <- predict(fit, test, type="class")

# Random Forest predictions

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi$Embarked)
which(combi$Embarked == "") # tells give row has a blank value
#row number 62 and 830 are having blank values so we input 'S' as a value for it
combi$Embarked[c(62,830)] <- 'S'
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
#1044 rows has a blank value for Fare variable
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
combi$FamilyID2 <- combi$FamilyId
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- as.factor(combi$FamilyID2)
combi$Sex <- as.factor(combi$Sex)
train <- combi[0:891,]
test <- combi[892:1309,]
test$Survived <- as.factor(0)

#fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +Embarked + Title + FamilySize + FamilyID2,data=train, importance=TRUE, ntree=2000)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))
#varImpPlot(fit)
#Prediction <- predict(fit,test)
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId <- test$PassengerId, Survived <- Prediction)
names(submit) <- c("PassengerId","Survived")
write.csv(submit, "RandomForestSubmission.csv", row.names=FALSE)