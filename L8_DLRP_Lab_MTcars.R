# =====================================================
# LESSON 8: Linear Regression Laboratory - MTCars

# Author: Rathina Grace Monica / Adjunct Lecturer
# =====================================================

# Set working directory to the specified context path
setwd("C:/Users/rathi/R_Portfolio") 

# Load and view data
data(mtcars)
head(mtcars, 10)      # View first 10 lines
str(mtcars)           # Overall structure
summary(mtcars)       # Mean, Min, Max for each variable

# Visualizing relationships
pairs(mtcars)         # Scatterplot matrix

plot(mtcars$wt, mtcars$mpg, main="Weight vs MPG")
print("The slope is negative")
plot(mtcars$mpg ~ mtcars$wt)

# Plot specific variables
plot(mtcars$wt, mtcars$mpg, 
     main="Car Weight vs. Fuel Efficiency", 
     xlab="Weight (1000 lbs)", ylab="MPG", 
     pch=19, col="darkgray")

# Correlation test
cor(mtcars$wt, mtcars$mpg)
cor.test(mtcars$wt, mtcars$mpg)

# Data Management
# Exclude 'vs' and 'am' using vector notation
my_mtcars <- mtcars[, -c(8, 9)]

# Exclude 'gear' and 'carb' using subset function
my_mtcars <- subset(my_mtcars, select = -c(gear, carb))

# Check dimensions (Should be 32 obs of 7 variables)
dim(my_mtcars)

# Convert 'cyl' to a factor
my_mtcars$cyl <- as.factor(my_mtcars$cyl)
class(my_mtcars$cyl)

# Grouped operation: Mean mpg per cylinder group
tapply(my_mtcars$mpg, my_mtcars$cyl, mean)

#Handling Outliers
# Set plot area to 1 row, 2 columns
par(mfrow=c(1, 2))
boxplot(my_mtcars$mpg, main="Boxplot MPG")
boxplot(my_mtcars$wt, main="Boxplot Weight")

# Identify outlier values for weight
boxplot.stats(my_mtcars$wt)$out

# Remove outliers (weight > 5.3)
my_mtcars <- subset(my_mtcars, wt <= 5.3)
# Check: Should now have 30 observations
dim(my_mtcars)

# Linear Regression Models
# Reset plot window to 1x1
par(mfrow=c(1,1))

# Model 1: mpg predicted by weight
lm1 <- lm(mpg ~ wt, data = my_mtcars)
summary(lm1)

# Plot the regression
plot(my_mtcars$wt, my_mtcars$mpg, main="Weight vs MPG with Regression Line")
abline(lm1, col="blue", lwd=2)

# Model 2 and 3: Adding Horsepower (hp)
lm2 <- lm(mpg ~ wt + hp, data = my_mtcars)
lm3 <- lm(mpg ~ hp, data = my_mtcars)

# Compare models using AIC
AIC(lm1, lm2, lm3)

#Prediction
# Create new data frame for prediction
new_car <- data.frame(hp = 225, wt = 4.0)

# Method 1: Using the predict function
predict(lm2, newdata = new_car)

# Method 2: Manual calculation using coefficients
# Equation: mpg = Intercept + (coeff_wt * wt) + (coeff_hp * hp)
coefs <- lm2$coefficients
coefs[1] + (coefs[2] * 4.0) + (coefs[3] * 225)