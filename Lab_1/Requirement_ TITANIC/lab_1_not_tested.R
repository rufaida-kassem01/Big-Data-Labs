# 1
# clean the environment
rm(list = ls())


# 2
# import the data file
titanic <- read.csv("./Lab_1/Requirement_TITANIC/titanic.csv")


# 3 data exploration
# a. Show the dimensions of the data frame. Hint: Use dim()
dim(titanic)
# b. Show the structure of the data frame. Hint: Use str()
str(titanic)
# c. Get more insight into data by exploring the first and the last TEN rows in the dataset.
head(titanic, 10)
tail(titanic, 10)
# d. Show summary of all variables in the data frame.
summary(titanic)

# 4 variable exploration
# a. Show a summary for the variable age only
summary(titanic$Age)
# b. What are the first and third quartile values for this variable? What do these values mean?
# The first quartile (Q1) is the value below which 25% of the data falls, 
# and the third quartile (Q3) is the value below which 75% of the data falls.
# These values help to understand the distribution of the age variable and identify any potential outliers.

# c. Are there any missing values in the variable age? (i.e. written as <NA>)
# Hint: Read the documentation for is.na() and anyNA() to find out how to
# know if a certain variable has missing values. What are the differences
# between them? Which one is better to use in this case?
is.na(titanic$Age)
anyNA(titanic$Age)
# The is.na() function returns a logical vector indicating which elements are NA, 
# while the anyNA() function returns a single logical value indicating if there are any NA values in the entire vector. 
# In this case, anyNA() is better to use as it provides a quick way to check for the presence of missing values 
# without having to inspect each element individually.

# d. What is the type of the variable embarked? Show the levels of this variable. Is that what you
# were expecting?
str(titanic$Embarked)
levels(titanic$Embarked)
# TODO

# e. Can you conclude what’s needed at this step in the data analysis cycle?
# At this step in the data analysis cycle, we need to perform data cleaning and preprocessing.


# 5 data preprocessing
# a. Remove the rows containing <NA> in the age variable from the data frame
titanic_clean <- na.omit(titanic)

# b. Remove the rows containing any unexpected value in the embarked variable from the
#dataset.
titanic_clean <- titanic_clean[titanic_clean$Embarked %in% c("C", "Q", "S"), ]

# c. Now, check that no NA values exist in the age variable. Also, factor the embarked variable
# and display its levels. Is that what you are expecting?
anyNA(titanic_clean$Age)
titanic_clean$Embarked <- factor(titanic_clean$Embarked)
levels(titanic_clean$Embarked)

# d. Some variables are not very interesting and provide no real indicative value. Remove
# columns Cabin and Ticket from the dataset
titanic_clean <- titanic_clean[, !(names(titanic_clean) %in% c("Cabin", "Ticket"))]

# 6 data analysis

# a. Show the number of males and females aboard the Titanic.
table(titanic_clean$Sex)

# b. Plot a pie chart showing the number of males and females aboard the Titanic. (Hint: use
# pie() function).
pie(table(titanic_clean$Sex), main = "Number of Males and Females aboard the Titanic", col = c("blue", "red"))

# c. Indicate males with a blue color and females with a red color in the above plot. (Hint: There
# is a color parameter in any plot function).
# c. Show the number of people who survived and didn’t survive from each gender.
table(titanic_clean$Sex, titanic_clean$Survived)
# d. Plot a pie chart showing the number of males and females who survived only.
survived_by_sex <- table(titanic_clean$Sex[titanic_clean$Survived == 1])
pie(survived_by_sex, main = "Number of Survivors by Gender", col = c("blue", "red"))
# e. What do you conclude from that?
# TODO
# f. Show the relationship between social class and survival i.e. show how many people
# survived and how many people didn’t survive from each class.
table(titanic_clean$Pclass, titanic_clean$Survived)
# g. Plot this relationship as a stacked bar plot. (Hint: use barplot() function)
survival_by_class <- table(titanic_clean$Pclass, titanic_clean$Survived)
barplot(survival_by_class, main = "Survival by Social Class", xlab = "Social Class", ylab = "Number of Passengers", col = c("red", "blue"), legend = TRUE)
# h. Indicate survived passengers with a blue color and un-survived passengers with a red color
# in the above plot.
# i. What do you conclude from that?
# TODO
# j. Plot a box and whiskers plot for the variable age (Hint: use boxplot() function)
# Read about the box and whiskers plot and understand it properly.
boxplot(titanic_clean$Age, main = "Boxplot of Age", ylab = "Age")
# k. What does this plot mean?
# The box and whiskers plot (or boxplot) provides a visual summary of the distribution of the Age variable. The box represents the interquartile range (IQR), which contains the middle 50% of the data. The line inside the box represents the median age. The "whiskers" extend from the box to the smallest and largest values within 1.5 times the IQR from the lower and upper quartiles, respectively. Any points outside of this range are considered outliers and are typically plotted as individual points. This plot helps to identify the central tendency, variability, and potential outliers in the Age variable.
# l. Plot a density distribution for the variable age.
plot(density(titanic_clean$Age), main = "Density Plot of Age", xlab = "Age", ylab = "Density")

# 7
# Remove all columns but passenger name and whether they survived or not. Export the new
# dataset to a file named “titanic_preprocessed.csv”
titanic_final <- titanic_clean[, c("Name", "Survived")]
write.csv(titanic_final, "titanic_preprocessed.csv", row.names = FALSE