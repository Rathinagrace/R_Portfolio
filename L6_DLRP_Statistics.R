# =====================================================

# LESSON 6: Statistics slide exercise
# Author: Rathina Grace Monica / Adjunct Lecturer

# =====================================================
# Slide 5
# --- Step 1: Generate Data ---
# We generate 1000 random numbers from a normal distribution 
# with a mean of 0 and a standard deviation of 1

data <- rnorm(1000, mean = 0, sd = 1)

# --- Step 2: Plot the Histogram ---
hist(data, 
     main = "Normal Distribution", 
     xlab = "Data", 
     ylab = "Frequency", 
     col = "skyblue",      # Adds professional visual appeal
     border = "white")     # Makes individual bars easier to see

# Slide 7
# # Probability of getting exactly 4 correct answers in 12 trials 
# with a success probability of 0.2
probability <- dbinom(4, size = 12, prob = 0.2)

# Display the result in the console
print(probability)

# Optional: Rounding for a cleaner presentation
round(probability, 4)

# Visualize the full distribution for the demo
successes <- 0:12
probs <- dbinom(successes, size = 12, prob = 0.2)
barplot(probs, names.arg = successes, 
        main = "Binomial Distribution (n=12, p=0.2)",
        col = "lightgreen", xlab = "Number of Successes")

# Slide 9
# --- Step 1: Create a sample contingency table ---
data_table <- as.table(rbind(c(762, 327, 468), 
                             c(484, 54, 150), 
                             c(433, 109, 150)))

# --- Step 2: Define dimension names for clarity ---
dimnames(data_table) <- list(gender = c("Male", "Female", "Unknown"),
                             result = c("Approve", "Disapprove", "Undecided"))

# --- Step 3: Perform the Chi-Square Test ---
chi_sq_result <- chisq.test(data_table)

# --- Step 4: Display Results ---
print(chi_sq_result)

#Slide 10
# --- Step 1: Load & Understand Data ---
# Assigning iris to 'data' and checking column names
data <- iris
names(data)

# Opens the documentation for the iris dataset 
help(iris) 

# --- Step 2: Create Frequency Table ---
# This shows the count for each of the 3 species
species_table <- table(data$Species)
print(species_table)

barplot(species_table, main="Species Distribution", col=c("red", "green", "blue"))
str(data)

#Slide 11
# --- Example 1: Iris Dataset (Sepal Length vs. Species) ---
# This shows how many instances of each Sepal Length exist per species
cross_tab_iris <- table(iris$Sepal.Length, iris$Species)
print(cross_tab_iris)

# --- Example 2: mtcars Dataset (Cylinders vs. Gears) ---
# This shows the distribution of engine cylinders across different gear counts
cross_tab_cars <- table(mtcars$cyl, mtcars$gear)
print(cross_tab_cars)

mosaicplot(cross_tab_cars, main="Cylinders vs Gears", color=TRUE)

# Slide 12
# Create the CSV File 
# We will create a data frame with some sample data
# This ensures the "random" data is the same every time you demo
set.seed(42)
sample_data <- data.frame(
  Eye.Color = sample(c("Brown", "Blue", "Green"), 100, replace = TRUE),
  Sex = sample(c("Male", "Female"), 100, replace = TRUE)
)

# Write this to a CSV file in your current folder
write.csv(sample_data, "people2.csv", row.names = FALSE)
message("File 'people2.csv' has been created successfully!")


# Now the file exists, so this will work perfectly!
people2 <- read.csv("people2.csv")

# Check column names
print("Column Names:")
print(names(people2))

# Create and Display the Cross Tabulation ---
table2 <- table(people2$Eye.Color, people2$Sex)

print("Cross Tabulation (Eye Color vs Sex):")
print(table2)

# Slide 13
# --- Step 1: Overall Proportion ---
# Expresses each cell as a fraction of the entire dataset (Sum of all cells = 1)
prop_overall <- prop.table(table2)
print("Overall Proportions:")
print(prop_overall)

# --- Step 2: Row-wise Proportion (margin = 1) ---
# Useful to see: "Of all people with Blue eyes, what % are Male vs Female?"
prop_row <- prop.table(table2, margin = 1)
print("Row Proportions (Sum of each row = 1):")
print(prop_row)

# --- Step 3: Column-wise Proportion (margin = 2) ---
# Useful to see: "Of all Females, what % have Brown eyes?"
prop_col <- prop.table(table2, margin = 2)
print("Column Proportions (Sum of each column = 1):")
print(prop_col)

# --- Step 4: Table of Percentages ---
# Makes the data much easier for an audience to read during a presentation
percent_table <- round(prop.table(table2) * 100, 2) # Added 2 decimal places for neatness
print("Table of Percentages (%):")
print(percent_table)

# Slide 14
# --- Step 1: Add Marginal Totals ---
# Adds both row and column totals (The "Grand Total" is in the bottom right)
table_with_totals <- addmargins(table2)
print("Table with All Marginal Sums:")
print(table_with_totals)

# --- Step 2: Specific Totals (For Demo Variation) ---
# Sum of each column (Column total)
print("Table with Column Totals Only (margin=1):")
print(addmargins(table2, margin = 1))

# Sum of each row (Row total)
print("Table with Row Totals Only (margin=2):")
print(addmargins(table2, margin = 2))

# --- Step 3: Test of Association ---
# This is a 'hassle-free' shortcut to get the Chi-Square result
print("Chi-Square Test Summary:")
summary(table2)

# Slide15
# --- Step 1: Update the data with a third variable ---
# We add "Height.Cat" (Short, Tall) to our existing data
people2$Height.Cat <- sample(c("Short", "Tall"), nrow(people2), replace = TRUE)

# Verify the new column exists
names(people2)

# --- Step 2: Create the 3D Contingency Table ---
# This categorizes Eye Color by Sex AND Height Category
table_3d <- table(people2$Eye.Color, people2$Sex, people2$Height.Cat)

# --- Step 3: Display the result ---
# R will display this as multiple 2D tables (slices)
print(table_3d)

# Probability Distribution using R
# Slide 26
# --- Scenario: A coin is tossed 5 times (n=5, p=0.5) ---

# 1. Probability of getting exactly ONE head
prob_1_head <- dbinom(1, size = 5, prob = 0.5)
print(paste("Probability of 1 head:", prob_1_head))

# 2. Probability of getting exactly THREE heads
prob_3_heads <- dbinom(3, size = 5, prob = 0.5)
print(paste("Probability of 3 heads:", prob_3_heads))

# Calculate for 1 and 3 heads simultaneously
dbinom(c(1, 3), size = 5, prob = 0.5)

# Slide 28
# Cumulative probability distribution
# Scenario: 45% female, sample of 10. 
# Find probability of 2 or less (0, 1, or 2).
cumulative_prob <- pbinom(2, size = 10, prob = 0.45)

print(cumulative_prob)


#Slide 30
# --- Scenario: Fair Coin, 10 Flips (n=10, p=0.5) ---

# 1. Forward Calculation (pbinom)
# Question: What is the probability of getting 3 or fewer heads?
prob_value <- pbinom(3, size = 10, prob = 0.5)
print(paste("Probability (Cumulative):", prob_value))


# 2. Reverse Calculation (qbinom)
# Question: To reach a cumulative probability of 0.171875, 
# how many heads (successes) do we need?
heads_needed <- qbinom(prob_value, size = 10, prob = 0.5)
print(paste("Number of heads (Quantile):", heads_needed))

#Slide 31
# --- Scenario: Simulating Data ---
# n = 8 (number of values to generate)
# size = 150 (number of trials per value)
# prob = 0.4 (probability of success)

# Generate the random values
x <- rbinom(n = 8, size = 150, prob = 0.4)

# Display the generated numbers
print(x)

# Poisson Distribution
#Slide 36
# --- Scenario: Poisson Distribution ---
# lambda: average rate of success (10 calls/hour)
# x: number of successes we are interested in (7 calls)

# 1. Define parameters
lambda_val <- 10
x_val <- 7

# 2. Calculate the probability mass function (PMF)
prob <- dpois(x = x_val, lambda = lambda_val)

# 3. Display the result
print(prob)

#Slide 37 - Cumulative Poisson Probability (ppois)
# Scenario: Hospital experiences average of 4 births per hour (lambda = 4)
# What is the probability that 4 or fewer births occur?
# Manual sum: P(0)+P(1)+P(2)+P(3)+P(4)

prob_le_4 <- ppois(q = 4, lambda = 4)
print(prob_le_4)
# Result: ~0.6288 or 62.88%

#Slide 38
# --- Scenario: Website Visits ---
# lambda: the average number of events per unit time (500 visits/day)
# p: the cumulative probability threshold (95% or 0.95)

# Calculate the minimum number of people expected with 95% probability
result <- qpois(p = 0.95, lambda = 500)

# Display the result
print(result)

#Slide 39
# --- Scenario: Website Traffic Simulation ---
# n: the number of random variables (numbers) to generate
# lambda: the mean (average rate) of the Poisson distribution

# Example: Generate 10 random numbers from a Poisson distribution with a mean of 5
random_numbers <- rpois(n = 10, lambda = 5)

# Display the generated numbers
print(random_numbers)

# Slide 45
# Generate 1000 random numbers from a normal distribution
data <- rnorm(1000, mean = 0, sd = 1)

# Plot the histogram
hist(data, 
     main = "Normal Distribution", 
     xlab = "Data", 
     ylab = "Frequency",
     col = "steelblue", 
     border = "white")

#The GRE(Graduate Record Examinations ) is widely used to help predict 
#the performance of applicants to graduate schools. The range of possible 
#scores on a GRE is 200 to 900. The psychology department at a university 
#finds that the students in their department have scores with a mean of 544 
#and standard deviation of 103. Find the value of the density function at x=550

# Calculate the density function for a GRE score of 550
# dnorm(x, mean, sd)
result <- dnorm(550, 544, 103)

# Display the result
print(result)

#Slide 46 Cumulative Distribution Function (pnorm)
# Find the probability that a student has a score less than 480
# Syntax: pnorm(q, mean, sd)
pnorm(480, 544, 103)

#Slide 47 Quantile Function (qnorm)
# Find the 90th percentile height
# Syntax: qnorm(p, mean, sd)
qnorm(0.9, 170, 5)

#Slide 48
# --- Scenario: Normal Distribution Simulation ---
# n: the number of random values to produce
# mean: the average of the distribution
# sd: the standard deviation

# Example: Generate 10 random numbers with a mean of 5 and SD of 2
random_variates <- rnorm(n = 10, mean = 5, sd = 2)

# Display the generated numbers
print(random_variates)
