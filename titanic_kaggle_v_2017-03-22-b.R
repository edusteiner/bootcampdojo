# cleans memory
rm(list=ls())

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

# check if columns are the same in the 2 datasets
titanic.train.colums <- colnames(titanic.train)
titanic.test.colums  <- colnames(titanic.test)

# creates a column "Survived" in titanic.test (test dataset)
titanic.test$Survived <- NA

# create a single dataset (union of Train and Test DataSets)
titanic.full <- rbind(titanic.train, titanic.test)

#################################################################################
# CLEANING MISSING DATA
#################################################################################

# Clean Missing Data (column "Embarked")
  table(titanic.full$Embarked)
  sum(is.na(titanic.full$"Embarked")) / nrow(titanic.full)  # returns ZERO or close, replace by mode
  titanic.full[is.na(titanic.full["Embarked"]),"Embarked"] <- 'S' # Mode of $Embarked
  titanic.full[titanic.full["Embarked"]=='',"Embarked"] <- 'S' # Mode of $Embarked
  
# Clean Missing Data (column "Pclass")
  table(titanic.full$Pclass)
  sum(is.na(titanic.full$"Pclass")) / nrow(titanic.full)  # returns ZERO or close, replace by mode
  titanic.full[is.na(titanic.full["Pclass"]),"Pclass"] <- 3

# Clean Missing Data (column "Sex")
  table(titanic.full$Sex)
  
# Clean Missing Data (column "Age")
  table(titanic.full$Age)
  histogram(titanic.full$Age)
  sum(is.na(titanic.full$"Age")) / nrow(titanic.full)  # 20% of missing values (NA)
  #sum(titanic.full[,"Age"] == '')
  
  # check how Age is correlated to another variable
  corrgram(titanic.full, order=NULL, lower.panel=panel.shade,
           upper.panel=NULL, text.panel=panel.txt,
           main="Correlacao Titanic Train")
  
  # Age x SibSp
  sem_NAs <- (!is.na(titanic.full["Age"]) & !is.na(titanic.full["SibSp"]))
  sum(sem_NAs)
  nrow(titanic.full[sem_NAs,"Age"])
  cor(titanic.full[sem_NAs,"Age"] , titanic.full[sem_NAs,"SibSp"])  
  
  # Age x Parch
  sem_NAs <- (!is.na(titanic.full["Age"]) & !is.na(titanic.full["PArch"]))
  sum(sem_NAs)
  nrow(titanic.full[sem_NAs,"Age"])
  cor(titanic.full[sem_NAs,"Age"] , titanic.full[sem_NAs,"Parch"])  
  
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
  # Fills in values for NA(Age) Male and Female in Class 1
  women_1st_list <- titanic.full[titanic.full["Pclass"]==1 & 
                                 titanic.full["Sex"]== "female" &
                                 is.na(titanic.full["Age"]), "PassengerId"]
  
  men_1st_list   <- titanic.full[titanic.full["Pclass"]==1 & 
                                     titanic.full["Sex"]== "male" &
                                     is.na(titanic.full["Age"]), "PassengerId"]
  
  # for (i in 1:length(women_1st_list))  {
  #     titanic.full[women_1st_list[i],"Age"] <- woman_median_age_class_1
  # }
  # 
  # for (i in 1:length(men_1st_list))  {
  #     titanic.full[men_1st_list[i],"Age"] <- male_median_age_class_1
  # }
  # 
  # #### até aqui ok

  
  # 19:01 after watching video_2, predicting Age value based on class (previously NA fares were being replaced by the median)
  sum(is.na(titanic.full$Age)) ## Quantos NA's
  
  # 1st class, Male, Predict Age
  age.upper.whisker <- boxplot.stats(titanic.full[titanic.full["Pclass"]==1 & titanic.full["Sex"]== "male" ,"Age"])$stats[5]
  outlier.filter <- titanic.full[titanic.full["Pclass"]==1 & titanic.full["Sex"]== "male" ,] < age.upper.whisker
  
  
  nrow(titanic.full[titanic.full["Pclass"]==1 & titanic.full["Sex"]== "male" ,])
  
  
  age.equation <- "Age ~ Pclass + Sex + Parch"
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
  
  
  
  
   
  
  # boxplot Class 2 x Sex
  boxplot(titanic.full[titanic.full["Pclass"]==2,"Age"] ~ titanic.full[titanic.full["Pclass"]==2,"Sex"])
  class_2nd_noage <- titanic.full[titanic.full["Pclass"]==2 & is.na(titanic.full["Age"]),]
  
  
  
  # boxplot Class 3 x Sex
  boxplot(titanic.full[titanic.full["Pclass"]==3,"Age"] ~ titanic.full[titanic.full["Pclass"]==3,"Sex"])
  class_3rd_noage <- titanic.full[titanic.full["Pclass"]==3 & is.na(titanic.full["Age"]),]
  
  
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

# replace missing values of Age with the median...... check later : main column with a lot of missing values
median_age <- median(titanic.full$Age, na.rm = TRUE)
#titanic.full[is.na(titanic.full["Age"]),"Age"] <- median_age
#women_median_age <- median(titanic.full[titanic.full$Sex == "female","Age"], na.rm = TRUE)
#male_median_age <- median(titanic.full[titanic.full$Sex == "male","Age"], na.rm = TRUE)
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
    
######### Predicting Age based on Pclass, SibSp, Parch
    
    # select which columns will be used to predict the model
    age.equation <-  "Age ~ Pclass + SibSp + Parch"
    age.formula <- as.formula(age.equation)
    #install.packages("randomForest")
    library(randomForest)
    
    titanic.model <- randomForest(formula = age.formula, 
                                  data=titanic.full[!is.na(titanic.full$Age),],
                                  ntree = 500, 
                                  mtry = 3, 
                                  nodesize = 0.01 * nrow(titanic.test))
    
    nrow(titanic.test[is.na(titanic.test$Age),])
    
    
    Age.equation <- "Pclass +
    SibSp  +
    Parch"
    
    
    Age <- predict(titanic.model, newdata = titanic.test)
    
#################    
    
    
    
    
    
    # Predicting AGE based on Pclass, SibSp, Parch
    age.equation <- "Age ~ Pclass + SibSp + Parch + Sex"
    
    boxplot(titanic.full$Age)
    age.upper.whisker <- boxplot.stats(titanic.full$Age)$stats[5]
    outlier.filter <- titanic.full$Age < age.upper.whisker
    
    histogram(titanic.full$Age)
        
    age.model  <- lm(formula = age.equation, data = titanic.full[outlier.filter,])
    
    age.row <- titanic.full[is.na(titanic.full$Age), c("Pclass", 
                                                         "Age" ,
                                                         "Sex",
                                                         "SibSp",
                                                         "Parch",
                                                         "Embarked")]
    
    age.predictions <- predict(age.model,newdata = age.row)
    titanic.full[is.na(titanic.full$Age),"Age"] <- age.predictions
    
    women_median_age <- median(titanic.full[titanic.full$Sex == "female","Age"], na.rm = TRUE)
    male_median_age <- median(titanic.full[titanic.full$Sex == "male","Age"], na.rm = TRUE)


    
    
    
    
    
    
    

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
install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, 
                              data=titanic.train, 
                              ntree = 500, 
                              mtry = 3, 
                              nodesize = 0.01 * nrow(titanic.test))

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




