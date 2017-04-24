getwd()
setwd("/Users/eduardo.steiner/Documents/bootcamp/Datasets")
titanic <- read.csv("titanic.csv")
head(titanic)
str(titanic)
# Casting & Readability
titanic$Survived <- as.factor(titanic$Survived)
levels(titanic$Survived) <- c("Dead", "Survived")
levels(titanic$Embarked) <- c("Unknown", "Cherbourg",
                              "Queenstown", "Southampton")

# Is Sex a good predictor?
male <- titanic[titanic$Sex=="male",]
female <- titanic[titanic$Sex=="female",]
par(mfrow=c(1, 2))
pie(table(male$Survived), labels=c("Dead", "Survived"), 
    main="Survival Portion of Men"
)
pie(table(female$Survive), labels=c("Dead", "Survived"),
    main="Survival Portion of Women"
)


# Exercise 3:
# Create 2 box plots of Age, one segmented by Sex, the other by Survived
# Create a histogram of Age
# Create 2 density plot of Age, also segmented by Sex and Survived
par(mfrow=c(1, 1))

boxplot(Age ~ Sex, data=titanic,
        main="Age Distribution By Gender",
        col=c("red","green"))

table(titanic$Sex, titanic$Survived)

boxplot(Age ~ Survived, data=titanic,
        main="Age Distribution By Survival",
        col=c("red","green"), ylab="Age")

table(titanic$Age, titanic$Survived)

densityplot(~Sex, data=titanic, groups = Survived, auto.key=TRUE)
densityplot(~Survived, data=titanic, groups = Sex, auto.key=TRUE)


