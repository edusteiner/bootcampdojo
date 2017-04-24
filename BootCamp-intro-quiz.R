getwd()
setwd("/Users/eduardo.steiner/Documents/bootcamp/steiner")
setwd("/Users/eduardo.steiner/Documents/bootcamp/")
dados <- read.csv("Datasets/Iris_Data.csv")
head(dados)
colnames(dados)[5] <- "Type"
colnames(dados)
dados[c(1:5), c((ncol(dados)-2):ncol(dados))]

str(dados)
boxplot(dados$Sepal.Length)

unique(dados$Type)

library(lattice)

xyplot(Sepal.Width ~ Sepal.Length, data=dados, groups = Type, auto.key=TRUE)
xyplot(Petal.Length ~ Petal.Width, data=dados, groups = Type, auto.key=TRUE)
cor(dados$Petal.Length,dados$Petal.Width)
histogram(dados$Petal.Length, breaks=10, main="Histogram")

#Density Plots
densityplot(dados$Petal.Length)

# Multiple Density Plots
densityplot(~Petal.Width, data=dados, groups = Type, auto.key=TRUE)

#Scatterplot Matrix
pairs(dados[,1:4], main = "Scatterplot Matrix")
#splom ...

library(GGally)
ggpairs(iris, ggplot2::aes(color=Species))
?ggpairs

mtcars_df <- mtcars
str(mtcars_df)

xyplot(mtcars_df$mpg ~ mtcars_df$cyl)
xyplot(mtcars_df$mpg ~ mtcars_df$disp)
xyplot(mtcars_df$mpg ~ mtcars_df$hp)


pairs(mtcars_df[,1:11], main = "Scatterplot Matrix")


str(mtcars_df)
mtcars_df$cyl <- as.factor(mtcars_df$cyl)
ggpairs(mtcars_df, ggplot2::aes(color=cyl))

library(ggplot2)
diamonds_df <- diamonds
head(diamonds_df)





