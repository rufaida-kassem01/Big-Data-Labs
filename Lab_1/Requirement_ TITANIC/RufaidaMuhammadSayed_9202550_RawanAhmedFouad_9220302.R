# Student 1 Name: Rufaida Muhammad Sayed Ahmed
# Student 1 ID: 9202550
# Student 2 Name: Rawan Ahmed Fouad
# Student 2 ID: 9220302

# Lab 1:
# 1
# clean the environment
rm(list = ls())


# 2
# import the data file
titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

cat ("Data imported successfully\n")
# # 3 data exploration
# # a. Show the dimensions of the data frame. Hint: Use dim()
cat("3.a. Dimensions of the data frame:\n")
print(dim(titanic))

# b. Show the structure of the data frame. Hint: Use str()
cat("3.b. Structure of the data frame:\n")
print(str(titanic))

# c. Get more insight into data by exploring the first and the last TEN rows in the dataset.
cat("3.c First 10 rows of the dataset:\n")
print(head(titanic, 10))
cat("3.c Last 10 rows of the dataset:\n")
print(tail(titanic, 10))

# d. Show summary of all variables in the data frame.
cat("3.d Summary of all variables in the data frame:\n")
print(summary(titanic))

# 4 variable exploration
# a. Show a summary for the variable age only
cat("4.a Summary for the variable age:\n")
print(summary(titanic$Age))

# b. What are the first and third quartile values for this variable? What do these values mean?
# The first quartile (Q1) is the value below which 25% of the data falls, 
# and the third quartile (Q3) is the value below which 75% of the data falls.
# These values help to understand the distribution of the age variable and identify any potential outliers.

# c. Are there any missing values in the variable age? (i.e. written as <NA>)
# Hint: Read the documentation for is.na() and anyNA() to find out how to
# know if a certain variable has missing values. What are the differences
# between them? Which one is better to use in this case?
cat("4.c Are there any missing values in the variable age?\n")
print(anyNA(titanic$Age))

# commented the line below to avoid printing a long logical vector
# print(is.na(titanic$Age))

# The is.na() function returns a logical vector indicating which elements are NA, 
# while the anyNA() function returns a single logical value indicating if there are any NA values in the entire vector. 
# In this case, anyNA() is better to use as it provides a quick way to check for the presence of missing values 
# without having to inspect each element individually.

# # d. What is the type of the variable embarked? Show the levels of this variable. Is that what you
# # were expecting?
cat("4.d Type of the variable embarked:\n")
print(class(titanic$Embarked))
cat("4.d Levels of the variable embarked:\n")
print(levels(titanic$Embarked))
# The variable embarked is of type character, and it does not have levels since it is not a factor. 
# We were expecting it to be a factor variable with levels corresponding to the different ports of embark


# # e. Can you conclude what’s needed at this step in the data analysis cycle?
# # At this step in the data analysis cycle, we need to perform data cleaning and preprocessing.


# 5 data preprocessing
# a. Remove the rows containing <NA> in the age variable from the data frame
titanic_clean <- titanic[!is.na(titanic$Age), ]
cat("5.a Rows containing <NA> in the age variable have been removed.\n")

# b. Remove the rows containing any unexpected value in the embarked variable from the
#dataset.
titanic_clean <- titanic_clean[titanic_clean$Embarked %in% c("C", "Q", "S"), ]
cat("5.b Rows containing unexpected values in the embarked variable have been removed.\n")

# # c. Now, check that no NA values exist in the age variable. Also, factor the embarked variable
# # and display its levels. Is that what you are expecting?
cat("5.c Check for NA values in the age variable:\n")
print(anyNA(titanic_clean$Age))
titanic_clean$Embarked <- factor(titanic_clean$Embarked)
cat("5.c Levels of the embarked variable:\n")
print(levels(titanic_clean$Embarked))

# d. Some variables are not very interesting and provide no real indicative value. Remove
# columns Cabin and Ticket from the dataset
titanic_clean <- titanic_clean[, !(names(titanic_clean) %in% c("Cabin", "Ticket"))]
cat("5.d Columns Cabin and Ticket have been removed from the dataset.\n")
# print the cleaned dataset column names to verify
cat("Columns in the cleaned dataset:\n")
print(names(titanic_clean))

# 6 data analysis

# a. Show the number of males and females aboard the Titanic.
cat("6.a Number of males and females aboard the Titanic:\n")
print(table(titanic_clean$Gender))

# b. Plot a pie chart showing the number of males and females aboard the Titanic. (Hint: use
# pie() function).
# Indicate males with a blue color and females with a red color in the above plot. (Hint: There
# is a color parameter in any plot function).
# create new figure for the pie chart
dev.new()
pie(table(titanic_clean$Gender), main = "Number of Males and Females aboard the Titanic", col = c("blue", "red"))

# c. Show the number of people who survived and didn’t survive from each gender.
cat("6.c Number of people who survived and didn’t survive from each gender:\n")
print(table(titanic_clean$Gender, titanic_clean$Survived))

# d. Plot a pie chart showing the number of males and females who survived only.
survived_by_gender <- table(titanic_clean$Gender[titanic_clean$Survived == 1])
# create new figure for the pie chart
dev.new()
pie(survived_by_gender, main = "Number of Survivors by Gender", col = c("blue", "red"))

# e. What do you conclude from that?
# TODO
# f. Show the relationship between social class and survival i.e. show how many people
# survived and how many people didn’t survive from each class.
cat("6.f Relationship between social class and survival:\n")
print(table(titanic_clean$Pclass, titanic_clean$Survived))

# g. Plot this relationship as a stacked bar plot. (Hint: use barplot() function)
# h. Indicate survived passengers with a blue color and un-survived passengers with a red color
# in the above plot.
survival_by_class <- table(titanic_clean$Pclass, titanic_clean$Survived)
# create new figure for the bar plot
dev.new()
barplot(survival_by_class, main = "Survival by Social Class", xlab = "Social Class", ylab = "Number of Passengers", col = c("red", "blue"), legend = TRUE)

# i. What do you conclude from that?
# TODO

# j. Plot a box and whiskers plot for the variable age (Hint: use boxplot() function)
# Read about the box and whiskers plot and understand it properly.
# create new figure for the boxplot
dev.new()
boxplot(titanic_clean$Age, main = "Boxplot of Age", ylab = "Age")

# k. What does this plot mean?
# TODO

# l. Plot a density distribution for the variable age.
# create new figure for the density plot
dev.new()
plot(density(titanic_clean$Age), main = "Density Plot of Age", xlab = "Age", ylab = "Density")

# 7
# Remove all columns but passenger name and whether they survived or not. Export the new
# dataset to a file named “titanic_preprocessed.csv”
titanic_final <- titanic_clean[, c("Name", "Survived")]
write.csv(titanic_final, "titanic_preprocessed.csv", row.names = FALSE)