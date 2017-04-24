# cleans memory
rm(list=ls())
library(lattice) 

# Data Dictionary (https://www.kaggle.com/c/titanic/data)
# 
# Variable  Definition  
# survival  Survival    
# pclass    Ticket class    
# sex       Sex 
# Age       Age in years    
# sibsp     # of siblings / spouses aboard the Titanic  
# parch     # of parents / children aboard the Titanic  
# ticket    Ticket number   
# fare      Passenger fare  
# cabin     Cabin number    
# embarked  Port of Embarkation C = Cherbourg, Q = Queenstown, S = Southampton
# 
# sibsp: The dataset defines family relations in this way...
# Sibling = brother, sister, stepbrother, stepsister
# Spouse = husband, wife (mistresses and fiancés were ignored)
# 
# parch: The dataset defines family relations in this way...
# Parent = mother, father
# Child = daughter, son, stepdaughter, stepson
# Some children travelled only with a nanny, therefore parch=0 for them.

# sets working directory
setwd("~/Documents/bootcamp/Kaggle")

# Read train and test datasets
titanic.train <- read.csv("train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test  <- read.csv ("test.csv", stringsAsFactors = FALSE, header = TRUE)

# creates a column to identify if it's a train or test set
titanic.train$isTrainSet <- TRUE
titanic.test$isTrainSet  <- FALSE

# creates a column "Survived" in titanic.test (test dataset)
titanic.test$Survived <- NA

# check if columns are the same in the 2 datasets
titanic.train.colums <- colnames(titanic.train)
titanic.test.colums  <- colnames(titanic.test)
if (sum(! (sort(titanic.train.colums) == sort(titanic.test.colums))) == 0) {
    cat(paste("OK, same columns", "\n"))
} else {
    cat(paste("*** columns are different ***", "\n"))
}


# create a single dataset (union of Train and Test DataSets)
# in order to have more data to be analyzed
titanic.full <- rbind(titanic.train, titanic.test)

#################################################################################
# CLEANING MISSING DATA
#################################################################################

# Clean Missing Data (column "Embarked")
  table(titanic.full$Embarked)
  sum(is.na(titanic.full$"Embarked")) / nrow(titanic.full)  # returns ZERO
  sum(titanic.full$"Embarked" == '') / nrow(titanic.full)   # returns 2 records
  titanic.full[titanic.full["Embarked"]=='',"Embarked"] <- 'S' # Mode of $Embarked
  
# Clean Missing Data (column "Pclass")
  table(titanic.full$Pclass)
  sum(is.na(titanic.full$"Pclass")) / nrow(titanic.full)  # returns ZERO or close, replace by mode
  #titanic.full[is.na(titanic.full["Pclass"]),"Pclass"] <- 3

# Clean Missing Data (column "Sex")
  table(titanic.full$Sex)
  
  
# Is She Married ?
  titanic.full[titanic.full$Sex == "female" & grepl('Mrs',titanic.full$Name),]   # ,c("Sex","Name","Age")   
  index <- titanic.full$Sex == "female" & grepl('Mrs',titanic.full$Name)
  titanic.full$IsSheMarried[index] <- TRUE
  index <- titanic.full$Sex == "female" & !grepl('Mrs',titanic.full$Name)
  titanic.full$IsSheMarried[index] <- FALSE
  index <- titanic.full$Sex == "male"
  titanic.full$IsSheMarried[index] <- FALSE
  summary(titanic.full$IsSheMarried)
  
  
  
  
# Clean Missing Data (column "Age")
  table(titanic.full$Age)
  library(ggplot2)
  
  qtde_breaks <- 20
  
  # Histogram Age + Survived
  p1 <- hist((titanic.full[titanic.full["Age"]<999 & !is.na(titanic.full["Age"]),"Age"]),breaks=qtde_breaks)
  p2 <- hist((titanic.full[titanic.full["Age"]<999 & !is.na(titanic.full["Age"] & titanic.full["Survived"]==1),"Age"]),breaks=qtde_breaks)
  plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,80))  # first histogram
  plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,80), add=T)  # second
  
  # Histogram Age + Survived + Sex=Female
  p1 <- hist((titanic.full[titanic.full["Age"]<999 & !is.na(titanic.full["Age"]) & titanic.full["Sex"]=='female',"Age"]),breaks=qtde_breaks)
  p2 <- hist((titanic.full[titanic.full["Age"]<999 & !is.na(titanic.full["Age"] & titanic.full["Sex"]=='female' & titanic.full["Survived"]==1),"Age"]),breaks=qtde_breaks)
  plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,80), ylim=c(0,180))  # first histogram
  plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,80), ylim=c(0,180), add=T)  # second
  
  # Histogram Age + Survived + Sex=Male
  p1 <- hist((titanic.full[titanic.full["Age"]<999 & !is.na(titanic.full["Age"]) & titanic.full["Sex"]=='male',"Age"]),breaks=qtde_breaks)
  p2 <- hist((titanic.full[titanic.full["Age"]<999 & !is.na(titanic.full["Age"] & titanic.full["Sex"]=='male' & titanic.full["Survived"]==1),"Age"]),breaks=qtde_breaks)
  plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,80), ylim=c(0,180))  # first histogram
  plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,80), ylim=c(0,180), add=T)  # second
  
  # Creates a category column that informs Age_Category of Each Passenger
  # and then plot Age_Category x Survived to check how Age Category is related to Survived
  age_breaks <- hist((titanic.full[titanic.full["Age"]<999 & !is.na(titanic.full["Age"]),"Age"]),breaks=qtde_breaks)$breaks
  titanic.full$AgeRange <- as.integer(titanic.full$Age/5)
  titanic.full$AgeRange <- as.factor(titanic.full$AgeRange)
  str(titanic.full)
  titanic.full[,c("Age", "AgeRange","Survived")]
  
  sum(is.na(titanic.full$"Age")) / nrow(titanic.full)  # 20% of missing values (NA)
  #sum(titanic.full[,"Age"] == '')
  
  # check how Age is correlated to another variable
  library(corrgram)
  corrgram(titanic.full, order=NULL, lower.panel=panel.shade,
           upper.panel=NULL, text.panel=panel.txt,
           main="Correlacao Titanic Full")
  

  # Age x SibSp
  sem_NAs <- (!is.na(titanic.full["Age"]) & !is.na(titanic.full["SibSp"]))
  xyplot(titanic.full[sem_NAs,"Age"] ~ titanic.full[sem_NAs,"SibSp"])  

  # Age x Parch (# of parents / children aboard the Titanic)
  sem_NAs <- (!is.na(titanic.full["Age"]) & !is.na(titanic.full["PArch"]))
  xyplot(titanic.full[sem_NAs,"Age"] ~ titanic.full[sem_NAs,"Parch"])  
  
  # Age x Class
  sem_NAs <- (!is.na(titanic.full["Age"]) & !is.na(titanic.full["Pclass"]))
  sum(sem_NAs)
  nrow(titanic.full[sem_NAs,"Age"])
  cor(titanic.full[sem_NAs,"Age"] , titanic.full[sem_NAs,"Pclass"])  
  # printing boxplot Age x Pclass
  boxplot(titanic.full$Age ~ titanic.full$Pclass)
  
  
  # Class 1 is slightly different from the classes 2 and 3
  # boxplot Class 1
  boxplot(titanic.full[titanic.full["Pclass"]==1,"Age"])
  # boxplot Class 1 x Sex
  boxplot(titanic.full[titanic.full["Pclass"]==1,"Age"] ~ titanic.full[titanic.full["Pclass"]==1,"Sex"])
  # Sex makes Median(Age) varies
  male_median_age_class_1  <- boxplot.stats(titanic.full[titanic.full["Pclass"]==1 & titanic.full["Sex"]== "male" ,"Age"])$stats[3]
  woman_median_age_class_1 <- boxplot.stats(titanic.full[titanic.full["Pclass"]==1 & titanic.full["Sex"]== "female" ,"Age"])$stats[3]
  
  First_Class_No_Age_DF <- titanic.full[titanic.full["Pclass"]==1 & is.na(titanic.full["Age"]),]  # 39 1stClass NoAge
  nrow(titanic.full[titanic.full["Pclass"]==1 & !is.na(titanic.full["Age"]),]) # 284 1stClass Age OK
    
  
  
  
  has_age <- titanic.full[!is.na(titanic.full["Age"]) & titanic.full["isTrainSet"]==TRUE ,]
  age.equation <- "Age ~ Pclass + Sex + Parch + SibSp"  ## Age as related to Pclass,Sex,Parch,SibSp
  
  age.model <- lm(formula = age.equation,
                   data = has_age)
  
  summary(age.model)
  
  
  
  age.row <- titanic.full[is.na(titanic.full$Age), c("Pclass", 
                                                       "Sex" ,
                                                       "Age" ,
                                                       "AgeRange",
                                                       "SibSp",
                                                       "Parch",
                                                       "Embarked")]
  
  age.predictions <- predict(age.model,newdata = age.row)
  titanic.full[is.na(titanic.full$Age),"Age"] <- age.predictions  
  boxplot(titanic.full$Age ~ titanic.full$Pclass)
  
  median(titanic.full$Age,na.rm=TRUE)
  
  
   
  #### Criar coluna "tem filho" ou "é filho de" para ver a influência no Survived
  
   
  
  
  graph_data <- titanic.full[!is.na(titanic.full["Age"]) , c("Age","Sex", "Pclass")]
  
  library(lattice) 
  # graph sex x pclass x age
  xyplot(Pclass ~
         Age, data=graph_data,
         groups=Sex,
         auto.key=TRUE
  )
      
  
# Verifies if there's any missing value 
table(titanic.full$Embarked)

# How many missing values in the Age Column ?
sum(is.na(titanic.full$Age))  # or..... table(is.na(titanic.full$Age))
library(lattice)

#Idade dos Homens por Classe
plot(x=titanic.full[titanic.full$Sex == "male","Age"], y=titanic.full[titanic.full$Sex == "male","Pclass"])

#Idade das Mulheres por SibSp
plot(x=titanic.full[titanic.full$Sex == "female","Age"], y=titanic.full[titanic.full$Sex == "female","SibSp"])


# Examinando Correlações
    library(corrgram)
    corrgram(titanic.full, order=NULL, lower.panel=panel.shade,
             upper.panel=NULL, text.panel=panel.txt,
             main="Correlacao Titanic Train")
    
    #forte correlacionado positivamente
    #  . Pclass com Fare
    #  . Survived com Pclass
    #  . Pclass com Age
    #  . Age com SibSp
    #  . Age com Parch
    
    boxplot(titanic.full$Age ~ titanic.full$Pclass)
    boxplot(titanic.full$Age ~ titanic.full$SibSp)
    boxplot(titanic.full$Age ~ titanic.full$Parch)
    
    
    #install.packages("randomForest")
    library(randomForest)
    
    
    

# How many missing values in the Fare Column ? 
sum(is.na(titanic.full$Fare))  # or..... table(is.na(titanic.full$Fare)

# Replace Fare Missing Values with the median
#titanic.full[is.na(titanic.full["Fare"]),"Fare"] <- median(titanic.full$Fare, na.rm = TRUE)

# after watching video_2, predicting fare value based on class (previously NA fares were being replaced by the median)
sum(is.na(titanic.full$Fare)) ## Quantos NA's
fare.upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < fare.upper.whisker
fare.equation <- "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(formula = fare.equation,
                 data = titanic.full[outlier.filter,])
fare.row <- titanic.full[is.na(titanic.full$Fare), c("Pclass", 
                                                     "Sex" ,
                                                     "Age" ,
                                                     "SibSp",
                                                     "Parch",
                                                     "Embarked")]

fare.predictions <- predict(fare.model,newdata = fare.row)
titanic.full[is.na(titanic.full$Fare),"Fare"] <- fare.predictions

# Categorical Casting
titanic.full$Pclass   <- as.factor(titanic.full$Pclass)
titanic.full$Sex      <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# bringing back to original datasets the data that has been cleaned
titanic.train <- titanic.full[titanic.full["isTrainSet"]==TRUE,]
titanic.test  <- titanic.full[titanic.full["isTrainSet"]==FALSE,]

# Train DataSet / casts column Survived
titanic.train$Survived <- as.factor(titanic.train$Survived)

str(titanic.train)


# select which columns will be used to predict the model
survived.equation <-  "Survived ~ Pclass +
                                    Sex    +
                                    Age    + 
                                    SibSp  +
                                    Parch  +
                                    Fare   + 
                                    Embarked"

survived.formula <- as.formula(survived.equation)
library(randomForest)



tuneRF(titanic.train[, c("Pclass","Sex","Age", "SibSp","Fare")],
       titanic.train[, c("Survived")],
       stepFactor=0.5)


titanic.model <- randomForest(formula = survived.formula, 
                              data=titanic.train, 
                              ntree = 50, 
                              mtry = 2, 
                              nodesize = 0.1 * nrow(titanic.test))

titanic.model


features.equation <- "Pclass +
                      Sex    +
                      Age    + 
                      SibSp  +
                      Parch  +
                      Fare   + 
                      Embarked"


Survived <- predict(titanic.model, newdata = titanic.test)

# creates a vector to store PassengerId
PassengerId <- titanic.test$PassengerId

# creates a dataframe that will be used an output to Kaggle
output.df          <- as.data.frame(PassengerId)
output.df$Survived <- Survived
write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)




