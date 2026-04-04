setwd("D:/CUFE_CMP_2024/Fourth_year_26/second_semster/BD/labs/Big-Data-Labs/Lab 5 - Predictive Analysis II/Linear Regression Requirement")
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
# print Q1 answer in the console
cat("Q1: As the standard deviation increases, the data points become more scattered around the line defined by 5 + 6*x. With SD = 1, the points are closely clustered around the line, while with SD = 100, the points are widely scattered, showing a lot of noise in the data.\n")

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
cat("Q2: The coefficients of the linear model (intercept and slope) remain relatively consistent across different standard deviations, as they are determined by the underlying relationship between x and y. However, as the standard deviation increases, the noise in the data increases, which can lead to less accurate estimates of the coefficients and wider confidence intervals. Therefore, while the coefficients themselves may not change significantly, their reliability and precision can be affected by the level of noise in the data.\n")

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
cat("Q3: As the standard deviation increases, the R-squared value decreases. This is because a higher standard deviation introduces more noise into the data, making it harder for the model to explain the variance in the dependent variable (y). With more noise, the model's predictions become less accurate, leading to a lower R-squared value. Conversely, with a lower standard deviation, the data points are closer to the true line defined by 5 + 6*x, resulting in a higher R-squared value as the model can explain more of the variance in y.\n")
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
cat("Q4: The residual plot for the model with SD=2 shows a random scatter of points around the horizontal line at zero, indicating that the residuals are randomly distributed and that the model is a good fit for the data. However, as the standard deviation increases (SD=10 and SD=100), the residual plots show more scatter and potential patterns, suggesting that the model may not be a good fit for the data with higher noise levels. Therefore, the residual plot for SD=2 is a good residual plot, while the plots for SD=10 and SD=100 indicate potential issues with the model fit due to increased noise in the data.\n")


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
cat("Q5: The residual plot for the model with a slight nonlinearity (coefficient of 0.1) still shows a random scatter of points around the horizontal line at zero, indicating that the residuals are randomly distributed and that the model is a good fit for the data. The nonlinearity introduced is negligible, and the linear model is still an appropriate approximation for this data.\n")

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
cat("Q6: With a large nonlinear coefficient (e.g., 50), the residual plot shows a clear curved/U-shaped pattern, indicating that the residuals are not randomly scattered around the horizontal line at zero. Instead, they are systematically positive in the middle and negative at the extremes (or vice versa). This suggests that the linear model is not appropriate for this data, and a polynomial or nonlinear model should be considered to better capture the relationship between x and y.\n")
#===============================End of Part(2)=============================================

#=================================Part(3)==================================================
#(Q7) Import the dataset LungCapData.tsv. What are the variables in this dataset?
data <- read.table("LungCapData.tsv", header=TRUE, sep="\t")
cat("Q7: The variables in the LungCapData.tsv dataset are: ")
print(colnames(data))
#(Q8) Draw a scatter plot of Age (x-axis) vs. LungCap (y-axis). Label x-axis "Age" and y-axis "LungCap"
# create new figure for plotting
dev.new()
plot(data$Age, data$LungCap, xlab="Age", ylab="LungCap", main="Scatter Plot of Age vs. LungCap")

#(Q9) Draw a pair-wise scatter plot between Lung Capacity, Age and Height. 
#Check the slides for how to plot a pair-wise scatterplot
# create new figure for plotting
dev.new()
pairs(data[, c("LungCap", "Age", "Height")], main="Pair-wise Scatter Plot of LungCap, Age, and Height")

#(Q10) Calculate correlation between Age and LungCap, and between Height and LungCap.
#Hint: You can use the function cor
cor_age_lungcap <- cor(data$Age, data$LungCap)
cor_height_lungcap <- cor(data$Height, data$LungCap)
cat("Q10: Correlation between Age and LungCap: ", cor_age_lungcap, "\n")
cat("Correlation between Height and LungCap: ", cor_height_lungcap, "\n")

#(Q11) Which of the two input variables (Age, Height) are more correlated to the 
#dependent variable (LungCap)?
# Answer: The variable that is more correlated to the dependent variable (LungCap) is the one with the higher absolute value of correlation.
# Height is more correlated to LungCap (0.9122) than Age (0.8197).
# Both are strongly correlated, but Height has a slightly higher correlation value.
cat("Q11: Height is more correlated to LungCap (correlation = 0.9122) than Age (correlation = 0.8197). Both variables show a strong positive correlation with LungCap, but Height has a slightly higher correlation value, indicating it may be a stronger predictor of LungCap compared to Age.\n")

#(Q12) Do you think the two variables (Height and LungCap) are correlated ? why ?
# Yes, Height and LungCap are strongly positively correlated (r = 0.9122).
# This makes biological sense: taller people have larger chest cavities,
# which means greater lung capacity. As height increases, lung capacity
# increases proportionally.
cat("Q12: Yes, Height and LungCap are strongly positively correlated with a correlation coefficient of 0.9122. This strong correlation makes biological sense because taller individuals typically have larger chest cavities, which can accommodate larger lungs, leading to greater lung capacity. Therefore, as height increases, lung capacity tends to increase proportionally.\n")

#(Q13) Fit a liner regression model where the dependent variable is LungCap 
#and use all other variables as the independent variables
model <- lm(LungCap ~ ., data=data)
# The dot (.) means: use ALL other columns as independent variables
# i.e., Age + Height + Smoke + Gender + Caesarean

#(Q14) Show a summary of this model
cat("Q14: Summary of the linear regression model:\n")
summary(model)
# get the R-squared value from the summary output
r_squared <- summary(model)$r.squared
cat("R-squared value: ", r_squared, "\n")

#(Q15) What is the R-squared value here ? What does R-squared indicate?
# R-squared = 0.8542
# This means the model explains ~85.4% of the variance in LungCap.
# R-squared measures the goodness of fit: a value closer to 1 means the
# independent variables explain most of the variability in the dependent variable.
# 0.8542 is a strong R-squared, indicating a good model fit.
cat("Q15: The R-squared value of the model is 0.8542. This indicates that approximately 85.4% of the variance in the dependent variable (LungCap) is explained by the independent variables (Age, Height, Smoke, Gender, and Caesarean) in the model. R-squared is a measure of the goodness of fit of the model, with values closer to 1 indicating that the model explains a larger proportion of the variance in the dependent variable. An R-squared value of 0.8542 suggests that the model has a strong fit to the data.\n")


#(Q16) Show the coefficients of the linear model. Do they make sense?
#If not, which variables don't make sense? What should you do?
cat("Q16: Coefficients of the linear model:\n")
print(coef(model))
# - Age (+0.1605)       : Makes sense — older people have larger lung capacity
# - Height (+0.2641)    : Makes sense — taller people have more lung capacity
# - Smoke (-0.6096)     : Makes sense — smokers have lower lung capacity
# - Gender (+0.3870)    : Makes sense — males tend to have larger lung capacity
# - Caesarean (-0.2142) : This is QUESTIONABLE. It's not immediately obvious why
#                         being born via C-section would reduce lung capacity.
#                         This coefficient may not make biological sense.
#
# What should you do about variables that don't make sense?
# → Investigate further (check literature or domain knowledge)
# → Consider removing the variable and comparing model performance
# → Check if it's statistically significant (look at p-value in summary)


#(Q17) Redraw a scatter plot between Age and LungCap. Display/Overlay the linear model (a line) over it.
#Hint: Use the function abline(model, col="red").
# create new figure for plotting
dev.new()
plot(data$Age, data$LungCap, xlab="Age", ylab="LungCap", main="Scatter Plot of Age vs. LungCap with Regression Line")
abline(model, col="red")
#Note (1) : A warning will be displayed that this function will display only the first two 
#           coefficients in the model. It's OK.
#Note (2) : If you are working correctly, the line will not be displayed on the plot. Why?
            # abline() only uses the first two coefficients of the model:
            # intercept and the coefficient of Age.
            # But our model has 5 predictors — the line it draws assumes all other variables
            # are zero, which doesn't match the actual 2D plot of Age vs LungCap.
            # So the line appears outside the visible range of the plot.
            # To properly overlay a line, you'd need to use a simple model with Age only.

#(Q18)Repeat Q13 but with these variables Age, Smoke and Caesarean as the only independent variables.
model2 <- lm(LungCap ~ Age + Smoke + Caesarean, data=data)
cat("Q18: Summary of the linear regression model with Age, Smoke, and Caesarean as independent variables:\n")
summary(model2)

#(Q19)Repeat Q16, Q17 for the new model. What happened?
cat("Q19: Coefficients of the new linear model:\n")
print(coef(model2))
# - Age (+0.5562)       : Makes sense — increases with age
# - Smoke (-0.6431)     : Makes sense — smokers have lower lung capacity
# - Caesarean (-0.1460) : Still questionable (same as before)
#
# WHAT HAPPENED compared to Model 1?
# - The Age coefficient increased from 0.1605 to 0.5562.
#   This is because Height was removed. Height and Age are correlated,
#   so when Height is absent, Age "absorbs" more of the explanatory power.
#   This is a classic case of omitted variable bias / multicollinearity effect.
# - The model is simpler but less accurate (R-squared drops from 0.8542 to 0.6778)

# create new figure for plotting
dev.new()
plot(data$Age, data$LungCap, xlab="Age", ylab="LungCap", main="Scatter Plot of Age vs. LungCap with Regression Line (Model 2)")
abline(model2, col="red")

# Now the line IS visible because Age is the first predictor in this simpler model.
# The line shows a positive slope — as Age increases, LungCap increases.

#(Q20)Predict results for this regression line on the training data.
ypred2 <- predict(model2)
# create new figure for plotting
dev.new()
par(mfrow=c(1,1))
plot(data$Age, data$LungCap, xlab="Age", ylab="LungCap", main="Scatter Plot of Age vs. LungCap with Predicted Values (Model 2)")
points(data$Age, ypred2, col="blue", pch=16)
legend("topleft", legend=c("Actual LungCap", "Predicted LungCap"), col=c("black", "blue"), pch=c(1, 16))


#(Q21)Calculate the mean squared error (MSE)of the training data.
mse <- mean((data$LungCap - ypred2)^2)
cat("Q21: Mean Squared Error (MSE) of the training data: ", mse, "\n")