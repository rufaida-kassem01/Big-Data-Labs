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
cat("\n3.a. Dimensions of the data frame:\n")
print(dim(titanic))

# b. Show the structure of the data frame. Hint: Use str()
cat("\n3.b. Structure of the data frame:\n")
print(str(titanic))

# c. Get more insight into data by exploring the first and the last TEN rows in the dataset.
cat("\n3.c First 10 rows of the dataset:\n")
print(head(titanic, 10))
cat("\n3.c Last 10 rows of the dataset:\n")
print(tail(titanic, 10))

# d. Show summary of all variables in the data frame.
cat("\n3.d Summary of all variables in the data frame:\n")
print(summary(titanic))

# 4 variable exploration
# a. Show a summary for the variable age only
cat("\n4.a Summary for the variable age:\n")
print(summary(titanic$Age))
# b. What are the first and third quartile values for this variable? What do these values mean?
cat("\n4.b The first quartile (Q1) is the value below which 25% of the data falls,\n 
and the third quartile (Q3) is the value below which 75% of the data falls.\n")
# These values help to understand the distribution of the age variable and identify any potential outliers.

# c. Are there any missing values in the variable age? (i.e. written as <NA>)
# Hint: Read the documentation for is.na() and anyNA() to find out how to
# know if a certain variable has missing values. What are the differences
# between them? Which one is better to use in this case?
cat("\n4.c Are there any missing values in the variable age?\n")
print(anyNA(titanic$Age))

# commented the line below to avoid printing a long logical vector
# print(is.na(titanic$Age))

# The is.na() function returns a logical vector indicating which elements are NA, 
# while the anyNA() function returns a single logical value indicating if there are any NA values in the entire vector. 
# In this case, anyNA() is better to use as it provides a quick way to check for the presence of missing values 
# without having to inspect each element individually.

# # d. What is the type of the variable embarked? Show the levels of this variable. Is that what you
# # were expecting?
cat("\n4.d Type of the variable embarked:\n")
print(class(titanic$Embarked))
cat("\n4.d Levels of the variable embarked:\n")
print(levels(titanic$Embarked))
# The variable embarked is of type character, and it does not have levels since it is not a factor. 
# We were expecting it to be a factor variable with levels corresponding to the different ports of embark


# # e. Can you conclude what’s needed at this step in the data analysis cycle?
# # At this step in the data analysis cycle, we need to perform data cleaning and preprocessing.
cat("\n4.e Conclusion for this step in the data analysis cycle:\n")
cat("\nAt this step, we need to clean the data by handling missing values in the age variable and ensuring that the embarked variable contains only valid values.\n
We also need to convert the embarked variable to a factor type to facilitate analysis.\n")

# 5 data preprocessing
# a. Remove the rows containing <NA> in the age variable from the data frame
titanic_clean <- titanic[!is.na(titanic$Age), ]
cat("\n5.a Rows containing <NA> in the age variable have been removed.\n")

# b. Remove the rows containing any unexpected value in the embarked variable from the
#dataset.
titanic_clean <- titanic_clean[titanic_clean$Embarked %in% c("C", "Q", "S"), ]
cat("\n5.b Rows containing unexpected values in the embarked variable have been removed.\n")

# # c. Now, check that no NA values exist in the age variable. Also, factor the embarked variable
# # and display its levels. Is that what you are expecting?
cat("\n5.c Check for NA values in the age variable:\n")
print(anyNA(titanic_clean$Age))
titanic_clean$Embarked <- factor(titanic_clean$Embarked)
cat("\n5.c Levels of the embarked variable:\n")
print(levels(titanic_clean$Embarked))
# The embarked variable is now a factor with levels C, Q, and S, which is what we were expecting.
cat("\n5.c Conclusion for this step in the data analysis cycle:\n")
cat("\nThe data has been cleaned successfully. There are no NA values in the age variable, \n
and the embarked variable has been converted to a factor with the expected levels.\n")

# d. Some variables are not very interesting and provide no real indicative value. Remove
# columns Cabin and Ticket from the dataset
titanic_clean <- titanic_clean[, !(names(titanic_clean) %in% c("Cabin", "Ticket"))]
cat("\n5.d Columns Cabin and Ticket have been removed from the dataset.\n")
# print the cleaned dataset column names to verify
cat("\nColumns in the cleaned dataset:\n")
print(names(titanic_clean))

# 6 data analysis

# a. Show the number of males and females aboard the Titanic.
cat("\n6.a Number of males and females aboard the Titanic:\n")
print(table(titanic_clean$Gender))

# b. Plot a pie chart showing the number of males and females aboard the Titanic. (Hint: use
# pie() function).
# Indicate males with a blue color and females with a red color in the above plot. (Hint: There
# is a color parameter in any plot function).
# create new figure for the pie chart
dev.new()
pie(table(titanic_clean$Gender), main = "Number of Males and Females aboard the Titanic", col = c("blue", "red"))

# c. Show the number of people who survived and didn’t survive from each gender.
cat("\n6.c Number of people who survived and didn’t survive from each gender:\n")
survived_by_gender <- table(titanic_clean$Gender, titanic_clean$Survived)
rownames(survived_by_gender) <- c("Female", "Male")
colnames(survived_by_gender) <- c("Did Not Survive", "Survived")
print(survived_by_gender)

# d. Plot a pie chart showing the number of males and females who survived only.
survived_by_gender <- table(titanic_clean$Gender[titanic_clean$Survived == 1])
# create new figure for the pie chart
dev.new()
pie(survived_by_gender, main = "Number of Survivors by Gender", col = c("blue", "red"))

# e. What do you conclude from that?
cat("\n6.e numbers of males survived < numbers of female survived\n\n")
# f. Show the relationship between social class and survival i.e. show how many people
# survived and how many people didn’t survive from each class.
cat("\n6.f Relationship between social class and survival:\n")
survival_by_class <- table(titanic_clean$Pclass, titanic_clean$Survived)
rownames(survival_by_class) <- c("1st Class", "2nd Class", "3rd Class")
colnames(survival_by_class) <- c("Did Not Survive", "Survived")
print(survival_by_class)

# g. Plot this relationship as a stacked bar plot. (Hint: use barplot() function)
# h. Indicate survived passengers with a blue color and un-survived passengers with a red color
# in the above plot.
# create new figure for the bar plot
dev.new()
barplot(t(survival_by_class),
        beside  = FALSE,
        col     = c("red", "blue"),
        legend  = c("Did Not Survive", "Survived"),
        main    = "Survival by Passenger Class",
        xlab    = "Passenger Class",
        ylab    = "Count",
        args.legend = list(x = "topleft", bty = "n"))

# i. What do you conclude from that?
cat("\n6.i The relationship between passenger class and survival is inversely proportional. \n
Passengers in 1st class had a higher survival rate compared to those in 2nd and 3rd class.\n
This suggests that social class played a significant role in the likelihood of survival during the Titanic disaster.\n\n")

# j. Plot a box and whiskers plot for the variable age (Hint: use boxplot() function)
# Read about the box and whiskers plot and understand it properly.
# create new figure for the boxplot
dev.new()
boxplot(titanic_clean$Age, main = "Boxplot of Age", ylab = "Age")

# k. What does this plot mean?
cat("\n6.k The grey rectangle shows the middle 50% of the data.\n
Bottom of the box is the 1st quartile .\n
Top of the box is the 3rd quartile.\n
The thick horizontal line inside the box is the median Age ~28 \n
the circles outside the plot are outliers")

# l. Plot a density distribution for the variable age.
# create new figure for the density plot
dev.new()
plot(density(titanic_clean$Age), main = "Density Plot of Age", xlab = "Age", ylab = "Density")

# 7
# Remove all columns but passenger name and whether they survived or not. Export the new
# dataset to a file named “titanic_preprocessed.csv”
titanic_final <- titanic_clean[, c("Name", "Survived")]
write.csv(titanic_final, "titanic_preprocessed.csv", row.names = FALSE)