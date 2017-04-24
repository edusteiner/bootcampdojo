# sets working directory
setwd("~/Documents/bootcamp/Kaggle")

# Read train and test datasets
titanic.train <- read.csv("train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test  <- read.csv ("test.csv", stringsAsFactors = FALSE, header = TRUE)

median(titanic.train$Age, na.rm=TRUE)
median(titanic.test$Age,  na.rm=TRUE)

# creates a column to identify if it's a train or test set
titanic.train$isTrainSet <- TRUE
titanic.test$isTrainSet <- FALSE

# check if colums are the same in the 2 datasets
titanic.train.colums <- colnames(titanic.train)
titanic.test.colums <- colnames(titanic.test)

# creates a column "Survived" in titanic.test (test dataset)
titanic.test$Survived <- NA

# create a single dataset (union of Train and Test DataSets)
titanic.full <- rbind(titanic.train, titanic.test)


# Clean Missing Data

# Clean Missing Data (column "Embarked")
titanic.full[titanic.full$Embarked =='',"Embarked"] <- 'S' # Mode of $Embarked

# Verifies if there's any missing value 
table(titanic.full$Embarked)

# How many missing values in the Age Column ?
sum(is.na(titanic.full$Age))  # or..... table(is.na(titanic.full$Age))

# replace missing values of Age with the median...... check later : main column with a lot of missing values
median_age <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full["Age"]),"Age"] <- median_age

# How many missing values in the Fare Column ? 
sum(is.na(titanic.full$Fare))  # or..... table(is.na(titanic.full$Fare)

# Replace Fare Missing Values with the median
titanic.full[is.na(titanic.full["Fare"]),"Fare"] <- median(titanic.full$Fare, na.rm = TRUE)

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




