#setwd("~/LAB")
rm(list=ls())

#=============================Part(1)=====================================
x <- runif(100, 0, 10)     # 100 draws between 0 & 10

#(Q1) Try changing the value of standard deviation (sd) in the next command 
#How do the data points change for different values of standard deviation?
y <- 5 + 6*x + rnorm(100, sd = 2)  # default values for rnorm (mean = 0 and sigma = 1)
#The above command generates 100 random numbers from a normal distribution with mean 0 and standard deviation 2, and adds them to the linear function 5 + 6*x.
# Answer: The data points will be scattered around the line defined by the equation 5 + 6*x. The amount of scatter (or noise) in the data points is controlled by the standard deviation of the normal distribution used to generate the random numbers. A higher standard deviation will result in more scattered data points, while a lower standard deviation will result in data points that are closer to the line.

#The standard deviation controls the amount of noise in the data. A higher standard deviation will result in more scattered data points around the line defined by 5 + 6*x, while a lower standard deviation will result in data points that are closer to the line.
# so let's repeat the above command with different values of standard deviation to see how the data points change:
y_sd1 <- 5 + 6*x + rnorm(100, sd = 1)  
y_sd10 <- 5 + 6*x + rnorm(100, sd = 10)
y_sd100 <- 5 + 6*x + rnorm(100, sd = 100)

#Plot them to visualize the effect of different standard deviations on the data points:
# create new figure for plotting
dev.new()
par(mfrow=c(2,2)) # parameters for the next plot
plot(x, y_sd1, main="SD = 1", xlab="x", ylab="y")
plot(x, y, main="SD = 2", xlab="x", ylab="y")
plot(x, y_sd10, main="SD = 10", xlab="x", ylab="y")
plot(x, y_sd100, main="SD = 100", xlab="x", ylab="y")

# OLS model
# OLS : Ordinary Least Squares
model1 <- lm(y ~ x)
# Learn about this object by saying ?lm and str(d)

# Compact model results
print(model1)
#(Q2) How are the coefficients of the linear model affected by changing the value
#of standard deviation in Q1?
# Answer: The coefficients of the linear model (the intercept and the slope) are not affected by changing the value of standard deviation in Q1. The coefficients are determined by the underlying relationship between x and y, which is defined by the equation 5 + 6*x. The standard deviation only affects the amount of noise in the data points, but it does not change the underlying relationship between x and y, so the coefficients remain the same regardless of the standard deviation used to generate the data.
# to make it more clear, let's fit the linear model to the data generated with different standard deviations and compare the coefficients:
model_sd1 <- lm(y_sd1 ~ x)
model_sd10 <- lm(y_sd10 ~ x)
model_sd100 <- lm(y_sd100 ~ x)
print(model_sd1)
print(model_sd10)
print(model_sd100)
# As you can see, the coefficients (intercept and slope) are very close to each other across the different models, confirming that the standard deviation does not affect the coefficients of the linear model.
# but the 10 standard deviation model has more noise, so the coefficients may be less accurate and have a wider confidence interval compared to the models with lower standard deviations.

# Regression diagnostics --
ypred <- predict(model1) # use the trained model to predict the same training data
# Learn about predict by saying ?predict.lm
# dev.new()
# par(mfrow=c(2,2)) # parameters for the next plot
# plot(y,y, type="l", xlab="true y", ylab="predicted y") # ploting the ideal line
# points(y, ypred) # plotting the predicted points
                 # the nearer to the ideal line the better
ypred_sd1 <- predict(model_sd1)
ypred_sd10 <- predict(model_sd10)
ypred_sd100 <- predict(model_sd100)
# create new figure for plotting
dev.new()
par(mfrow=c(1,1)) # parameters for the next plot
plot(y,y, type="l", xlab="true y", ylab="predicted y") # ploting the ideal line
points(y, ypred_sd1, col="blue") # plotting the predicted points for sd=1
points(y, ypred, col="black") # plotting the predicted points for sd=2
points(y, ypred_sd10, col="red") # plotting the predicted points for sd=10
points(y, ypred_sd100, col="green") # plotting the predicted points for sd=100
legend("topleft", legend=c("SD=1", "SD=2", "SD=10", "SD=100"), col=c("blue", "black", "red", "green"), pch=1)

# Detailed model results
d1 <- summary(model1)
print(model1)

#(Q3) How is the value of R-squared affected by changing the value
#of standard deviation in Q1?
# repeat the above steps for different standard deviations and compare the R-squared values:
d_sd1 <- summary(model_sd1)
d_sd10 <- summary(model_sd10)
d_sd100 <- summary(model_sd100)

# Learn about this object by saying ?summary.lm and by saying str(d)
cat("OLS sd(1) gave slope of ", d1$coefficients[2,1],   
    "and an R-sqr of ", d_sd1$r.squared, "\n")
cat("OLS sd(2) gave slope of ", d1$coefficients[2,1],   
    "and an R-sqr of ", d_sd1$r.squared, "\n")
cat("OLS sd(10) gave slope of ", d_sd10$coefficients[2,1],   
    "and an R-sqr of ", d_sd10$r.squared, "\n")
cat("OLS sd(100) gave slope of ", d_sd100$coefficients[2,1],   
    "and an R-sqr of ", d_sd100$r.squared, "\n")
# Answer: The value of R-squared is affected by changing the value of standard deviation in Q1.
# As the standard deviation increases, the amount of noise in the data increases, which can lead to a decrease in the R-squared value. 
# This is because R-squared measures the proportion of variance in the dependent variable (y) that is explained by the independent variable (x) in the model. 
# When there is more noise in the data, it becomes harder for the model to explain the variance in y, resulting in a lower R-squared value near 0.
# Conversely, when there is less noise (lower standard deviation), the model can explain more of the variance in y, leading to a higher R-squared value nearer to 1.


#Graphic dignostic (cont.)
# create new figure for plotting
dev.new()
par(mfrow=c(2,2)) # parameters for the next plot
plot(model1, 1) # plot the diagnostic graphs
plot(model_sd1, 1) # plot the diagnostic graphs for sd=1
plot(model_sd10, 1) # plot the diagnostic graphs for sd=10
plot(model_sd100, 1) # plot the diagnostic graphs for sd=100

#(Q4)What do you conclude about the residual plot? Is it a good residual plot?
# Answer: The residual plot shows the residuals (the differences between the observed and predicted values) on the y-axis and the fitted values (the predicted values) on the x-axis.
# A good residual plot should show a random scatter of points around the horizontal line at zero, indicating that the residuals are randomly distributed and that the model is a good fit for the data.
# In this case, if the residual plot shows a random scatter of points around the horizontal line at zero, it would suggest that the model is a good fit for the data. However, if the residual plot shows a pattern (such as a funnel shape, a curve, or a systematic trend), it would indicate that the model is not a good fit for the data and that there may be some issues with the model, such as heteroscedasticity (non-constant variance of residuals) or non-linearity. 
# Therefore, the conclusion about the residual plot would depend on the specific pattern observed in the plot.


# This IS a good residual plot because:
# - Residuals are randomly scattered around the horizontal zero line
# - No visible pattern or curve in the residuals
# - Variance of residuals appears roughly constant (homoscedasticity)
# - This confirms the linear model is appropriate for this data

#========================End of Part(1)==============================================

#========================Part(2)=====================================================
#Training a linear regression model
x1 <- runif(100) 
# introduce a slight nonlinearity
#(A)
y1 = 5 + 6*x1 + 0.1*x1*x1 + rnorm(100)
# create new figure for plotting
dev.new()
plot(x1,y1)
model <- lm(y1 ~ x1)

summary(model)

#Creating a test set (test vector)

#EDIT: We renamed the variable as x1 instead of xtest (in previous versions)
#becaues the lm function searches in the formula for variables named 
#with x1 and not any other name.
#So, if you used xtest, the lm function will not know what is xtest and
#a random plot will be generated. 

x1 <- runif(100)
#(B)
ytrue = 5 + 6*x1 + 0.1*x1*x1 + rnorm(100)  # same equation of y1 but on xtest to get true y for xtest

ypred <- predict(model, data.frame(x1))
# create new figure for plotting
dev.new()
par(mfrow=c(1,1))
plot(ytrue, ytrue, type="l", xlab="true y", ylab="predicted y")
points(ytrue, ypred)

# graphic dignostic (cont.)
# create new figure for plotting
dev.new()
par(mfrow=c(1,1)) # parameters for the next plot
plot(model, 1) # plot the diagnostic graphs

#(Q5)What do you conclude about the residual plot? Is it a good residual plot?

# This is still a GOOD residual plot.
# Because the nonlinear coefficient (0.1) is very small, the nonlinearity is negligible.
# Residuals appear randomly scattered with no clear pattern.
# The linear model is a good approximation in this case.


#(Q6)Now, change the coefficient of the non-linear term in the original model for (A) training 
#and (B) testing to a large value instead. What do you notice about the residual plot?
x1 <- runif(100)
y1 = 5 + 6*x1 + 50*x1*x1 + rnorm(100)  # increased the non-linear coefficient to 5
model <- lm(y1 ~ x1)
x1 <- runif(100)
ytrue = 5 + 6*x1 + 50*x1*x1 + rnorm(100)  # same equation of y1 but on xtest to get true y for xtest to get true y for xtest
ypred <- predict(model, data.frame(x1))
# create new figure for plotting
dev.new()
par(mfrow=c(1,1))
plot(ytrue, ytrue, type="l", xlab="true y", ylab="predicted y")
points(ytrue, ypred)
# graphic dignostic (cont.)
# create new figure for plotting
dev.new()
par(mfrow=c(1,1)) # parameters for the next plot
plot(model, 1) # plot the diagnostic graphs 

# With a large nonlinear coefficient (e.g., 50), the residual plot shows a clear
# curved/U-shaped pattern. Residuals are NOT randomly scattered — they are
# systematically positive in the middle and negative at the extremes (or vice versa).
# This indicates the linear model is NOT appropriate, and a polynomial/nonlinear
# model should be used instead.

#===============================End of Part(2)=============================================

# #=================================Part(3)==================================================
# #(Q7) Import the dataset LungCapData.tsv. What are the variables in this dataset?

# #(Q8) Draw a scatter plot of Age (x-axis) vs. LungCap (y-axis). Label x-axis "Age" and y-axis "LungCap"

# #(Q9) Draw a pair-wise scatter plot between Lung Capacity, Age and Height. 
# #Check the slides for how to plot a pair-wise scatterplot

# #(Q10) Calculate correlation between Age and LungCap, and between Height and LungCap.
# #Hint: You can use the function cor

# #(Q11) Which of the two input variables (Age, Height) are more correlated to the 
# #dependent variable (LungCap)?

# #(Q12) Do you think the two variables (Height and LungCap) are correlated ? why ?

# #(Q13) Fit a liner regression model where the dependent variable is LungCap 
# #and use all other variables as the independent variables

# #(Q14) Show a summary of this model

# #(Q15) What is the R-squared value here ? What does R-squared indicate?

# #(Q16) Show the coefficients of the linear model. Do they make sense?
# #If not, which variables don't make sense? What should you do?

# #(Q17) Redraw a scatter plot between Age and LungCap. Display/Overlay the linear model (a line) over it.
# #Hint: Use the function abline(model, col="red").
# #Note (1) : A warning will be displayed that this function will display only the first two 
# #           coefficients in the model. It's OK.
# #Note (2) : If you are working correctly, the line will not be displayed on the plot. Why?

# #(Q18)Repeat Q13 but with these variables Age, Smoke and Cesarean as the only independent variables.

# #(Q19)Repeat Q16, Q17 for the new model. What happened?

# #(Q20)Predict results for this regression line on the training data.

# #(Q21)Calculate the mean squared error (MSE)of the training data.