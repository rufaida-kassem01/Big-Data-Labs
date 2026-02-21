# clean the environment
rm(list = ls())
# import the data file
titanic <- read.csv("./Lab_1/Requirement_TITANIC/titanic.csv")
# view the dimension of the data
dim(titanic)
# view the structure of the data
str(titanic)
# view the first and last 10 rows of the data
head(titanic, 10)
tail(titanic, 10)
# view the summary of the data
summary(titanic)