# =====================================================
# LESSON 3: LOGISTIC REGRESSION DEMONSTRATION
# Goal: Predict customer default based on balance/income
# Author: Rathina Grace Monica / Adjunct Lecturer
# =====================================================

# 1. Install and Load the ISLR library 

install.packages("ISLR")
library(ISLR)

# 2. Load and inspect the 'Default' dataset
data(Default)
print("Dataset Loaded Successfully")

# View Column Names
# This shows you the headers of your table
names(Default)

# Total Number of Rows and Columns (Shape)
# In R, dim() returns (rows, columns)
dim(Default)

# Total number of rows specifically
nrow(Default)

# Data Types and Structure
# This is the most important command. It shows:
# - The number of observations (rows)
# - The number of variables (columns)
# - The data type of each column (Factor, Numeric, etc.)
str(Default)

# Statistical Summary of columns
# This gives you the Min, Max, Mean, and Median for numeric columns,
# and counts for categorical (Factor) columns.
summary(Default)

# Shows the first 6 rows
head(Default)  

# 3. Fit the Logistic Regression Model
# '~' means 'predicted by'
# 'family = "binomial"' tells R to use Logistic Regression
model_glm <- glm(default ~ balance + income, 
                 family = "binomial", 
                 data = Default)

# 4. Show the statistical summary
# Small values (with stars) mean the variable is a significant predictor.
summary(model_glm)

# 5. Optional: Make a quick prediction 
# Predict probability for a person with balance of 1500 and income of 40000
new_data <- data.frame(balance = 1500, income = 40000)
prob <- predict(model_glm, new_data, type = "response")
print(paste("Probability of default for this profile:", round(prob, 4)))
