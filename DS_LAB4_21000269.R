getwd()
toy <- read.csv("D:/Users/amiru/Downloads/toydata.csv")
plot(toy$X,toy$y, xlab = 'x',ylab = 'y')

#fitting a model
lm(y~X, data=toy)

#explore a data
fit <- lm(y~X, data=toy)
summary(fit)

#make prediction
yhat <- fit$coefficents[[1]] + fit$coefficients[[2]]*toy$X
yhat
yhat <- predict(fit,toy)
yhat

plot(toy$y,toy$X)
lines(toy$X,yhat,lwd=2)

#adjuting outliers
fit2 <- lm(y~X, data=toy, subset = 1:9)
summary(fit2)
summary(toy)

yhat2 <- predict(fit2,toy)
plot(toy$y~toy$X)
lines(toy$X,yhat2,lwd=2, col="pink")

#evaluate
mean((toy$y-yhat)^2) #include outlier
mean((toy$y-yhat2)^2) #exclude outlier

#ACTIVITY 3
# 1. Load the data and plot the variable
df <- read.csv("D:/Users/amiru/Downloads/toydata.csv")

# Plotting the data
plot(df$`X`, df$`y`, main="Scatter Plot of Data", xlab="X", ylab="Y")

# 2. Fit a simple linear regression model
model <- lm(`y` ~ `X`, data=df)

# Get the intercept and regression coefficient
intercept <- coef(model)[1]
regression_coefficient <- coef(model)[2]

# Print the intercept and coefficient
cat("Intercept:", intercept, "\n")
cat("Regression Coefficient:", regression_coefficient, "\n")

# 3. Check the p-value for the regression coefficient
p_value <- summary(model)$coefficients["`X`", "Pr(>|t|)"]
cat("P-value for Regression Coefficient:", p_value, "\n")

# 4. Perform predictions
predictions <- predict(model, df)

# 5. Remove an outlier data point (e.g., based on your judgment)
df_clean <- df[-c(5),]  # Remove the fifth row as an example

# Fit a model without the outlier
model_clean <- lm(`y` ~ `X`, data=df_clean)

# Compare the estimates, p-value, and R-squared
summary(model_clean)

# 6. Predict using model fitted without outlier
predictions_clean <- predict(model_clean, df)

# Plot both fitted lines
plot(df$`X`, df$`y`, main="Scatter Plot of Data with Two Fitted Lines", xlab="X", ylab="Y")
lines(df$`X`, predictions, col="red", lwd=2, legend.label="Original Model")
lines(df$`X`, predictions_clean, col="blue", lwd=2, legend.label="Model without Outlier")
legend("topright", legend=c("Original Model", "Model without Outlier"), col=c("red", "blue"), lwd=2)
