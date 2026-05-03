# =====================================================

# LESSON 9: Data Visualisation
# Author: Rathina Grace Monica / Adjunct Lecturer

# =====================================================

# Slide 4
# --- 1. SAMPLE DATA PREPARATION ---
# Defining numerical values for the chart
slices <- c(10, 12, 4, 16, 8)

# Defining categorical labels
lbls <- c("US", "UK", "Australia", "Germany", "France")

# --- 2. BASIC PIE CHART ---
# Quick visualization to check distribution
pie(slices, labels = lbls, main = "Pie Chart of Countries")

# --- 3. ADVANCED PIE CHART (Percentages & Colors) ---
# Calculate percentages for professional reporting
pct <- round(slices/sum(slices) * 100)

# Create descriptive labels using paste()
lbls_pct <- paste(lbls, "-", pct, "%", sep = " ") 

# Plot with rainbow colors and improved title
pie(slices, 
    labels = lbls_pct, 
    col = rainbow(length(lbls)), 
    main = "Country Distribution Analysis (%)")

# Slide 5
# --- 1. BASIC BAR CHART ---
# Representing monthly commuter or sales data
counts <- c(17, 32, 8, 53, 1)
months <- c("Jan", "Feb", "Mar", "Apr", "May")

# plot with professional 'steelblue' color
barplot(counts, 
        names.arg = months, 
        col = "steelblue", 
        main = "Monthly Trend Analysis",
        xlab = "Month", 
        ylab = "Frequency")

# --- 2. GROUPED BAR CHART (Using Matrices) ---
# Creating a 2x3 matrix to compare two categories (e.g., Target vs. Actual)
data_matrix <- matrix(c(2, 9, 3, 11, 9, 4), nrow = 2, ncol = 3)

# 'beside = TRUE' places bars side-by-side instead of stacked
barplot(data_matrix, 
        beside = TRUE, 
        col = c("green3", "orange"), 
        main = "Comparative Category Analysis",
        legend.text = c("Group A", "Group B"),
        args.legend = list(x = "topright"))

# Slide 6
# --- 1. SINGLE VARIABLE BOXPLOT ---
# Analyzing the distribution of Sepal Length in the iris dataset
boxplot(iris$Sepal.Length, 
        main = "Statistical Distribution: Sepal Length", 
        col = "orange",
        ylab = "Length (cm)",
        border = "brown")

# --- 2. GROUPED BOXPLOT BY FACTOR ---
# This demonstrates variance across different species
# Using 'notch = TRUE' helps visualize if medians differ significantly
boxplot(Sepal.Length ~ Species, 
        data = iris, 
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Sepal Length Variation by Species",
        xlab = "Iris Species",
        ylab = "Sepal Length (cm)",
        notch = TRUE)

# Slide 7
# --- 1. BASIC HISTOGRAM ---
# Visualizing temperature frequency from the built-in airquality dataset
hist(airquality$Temp, 
     main = "Temperature Distribution", 
     xlab = "Temperature (Fahrenheit)", 
     col = "lightgray",
     border = "white")

# --- 2. CUSTOM HISTOGRAM WITH NORMAL DENSITY CURVE ---
# Step A: Create the histogram with 'freq = FALSE' to show density instead of counts
hist(airquality$Temp, 
     freq = FALSE, 
     col = "gray90", 
     main = "Temperature Distribution with Normal Curve",
     xlab = "Temp",
     border = "darkgray")

# Step B: Overlay the theoretical Normal Curve
# Using mean() and sd() from the data to define the curve
curve(dnorm(x, 
            mean = mean(airquality$Temp, na.rm = TRUE), 
            sd = sd(airquality$Temp, na.rm = TRUE)), 
      add = TRUE, 
      col = "red", 
      lwd = 2)

#Slide 8
# --- 1. SCATTERPLOT ---
# Examining the relationship between Horsepower and Miles Per Gallon
# 'pch = 19' creates solid circles, 'col = blue' for clarity
plot(mtcars$hp, mtcars$mpg, 
     main = "Correlation: MPG vs Horsepower", 
     xlab = "Horsepower (HP)", 
     ylab = "Miles Per Gallon (MPG)", 
     pch = 19, 
     col = "blue")

# --- 2. LINE GRAPH ---
# Visualizing a trend over a time sequence
# 'type = "l"' is the key argument that connects the dots
x_time <- c(1, 2, 3, 4, 5)
y_value <- c(10, 15, 12, 18, 20)

plot(x_time, y_value, 
     type = "l", 
     main = "Trend Line Analysis", 
     xlab = "Time Period", 
     ylab = "Value", 
     col = "red",
     lwd = 2) # Adding line width for better slide visibility

# Slide 10
# --- 1. SETTING UP THE PROJECT ---
# It is best practice to use R Projects (.Rproj) to handle paths automatically.
# Download project from: http://tinyurl.com/kp6bxt4

# --- 2. LOADING DATA ---
# Using the read.csv() function to import comma-separated values
# The path 'data/' indicates that the file is in a sub-folder named 'data'
surveys <- read.csv('data/surveys_complete.csv')

# --- 3. DATA VERIFICATION ---
# After loading, always inspect the data structure
head(surveys)   # View the first 6 rows
str(surveys)    # View data types and column structure
summary(surveys)# Get a statistical overview of the dataset

# Slide 12
# --- 1. THE BASIC GGPLOT STRUCTURE ---
# We use the '+' operator to add layers to our visualization

library(ggplot2)

# Building a scatterplot using the Grammar of Graphics
ggplot(data = surveys, 
       mapping = aes(x = weight, 
                     y = hindfoot_length)) + 
  geom_point()

#Slide 13
# --- ADDING TRANSPARENCY TO GEOMS ---
# We use the 'alpha' argument inside the geom layer 
# to make points semi-transparent.

ggplot(data = surveys, 
       mapping = aes(x = weight, 
                     y = hindfoot_length)) + 
  geom_point(alpha = 0.1) # 0.1 means 10% opacity

# Slide 14
# --- ADDING FIXED COLOR TO GEOMS ---
# We specify 'color' inside the geom layer to apply 
# the same color to every single data point.

ggplot(data = surveys, 
       mapping = aes(x = weight, 
                     y = hindfoot_length)) + 
  geom_point(alpha = 0.1, 
             color = "blue")
#Slide 15
# --- MAPPING COLOR TO A VARIABLE ---
# By putting 'color' inside the aes() function, R will 
# automatically assign a unique color to each species.

ggplot(data = surveys, 
       mapping = aes(x = weight, 
                     y = hindfoot_length)) + 
  geom_point(alpha = 0.1, 
             aes(color = species_id))
#Slide 16
# --- EXERCISE 1: COLOR BY PLOT_ID ---

# 1. Check the initial class
class(surveys$plot_id) # Returns "integer"

# 2. Convert plot_id to a factor (categorical data)
# This is crucial for the mapping logic
surveys$plot_id <- as.factor(surveys$plot_id)

# 3. Create the visualization
ggplot(data = surveys, 
       mapping = aes(x = weight, 
                     y = hindfoot_length)) + 
  geom_point(alpha = 0.1, 
             aes(color = plot_id))
# Slide 17
# --- GGPLOT2 BOXPLOT ---
# Building the visualization using the 'surveys' object
ggplot(data = surveys, 
       mapping = aes(x = species_id, 
                     y = hindfoot_length)) + 
  geom_boxplot()

# Slide 18
# --- EXERCISE 2: VIOLIN PLOT ---
# Swapping geom_boxplot for geom_violin to see density distribution
ggplot(data = surveys, 
       mapping = aes(x = species_id, 
                     y = hindfoot_length)) + 
  geom_violin(fill = "lightblue", color = "darkblue")

#Slide 19 ; Time Series data
# --- 1. RESHAPE DATA ---
# Grouping data by year and species to count occurrences
yearly_counts <- surveys %>%
  group_by(year, species_id) %>%
  tally()

# --- 2. PLOT TIME SERIES ---
# Visualizing the count (n) over time using a line graph
ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n)) + 
  geom_line()

#Slide 20
# --- SEPARATING TRENDS BY CATEGORY ---
# We use the 'group' aesthetic to tell ggplot to draw 
# a separate line for every unique species_id.

ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n, 
                     group = species_id)) + 
  geom_line()

#Slide 21
# --- COLOR MAPPING BY CATEGORY ---
# By adding 'colour' to aes(), R assigns a unique color 
# and creates a legend for each species_id.

ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n, 
                     group = species_id,
                     colour = species_id)) + 
  geom_line()

# Slide 22
# --- APPLYING A PREMADE THEME ---
# We use theme_bw() (Black and White) to remove the default 
# gray background and create a professional, print-ready look.

ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n, 
                     color = species_id, 
                     group = species_id)) + 
  geom_line() + 
  theme_bw()
# Slide 23
# --- 1. DATA TRANSFORMATION ---
# Group by year and species, then calculate the mean weight
yearly_weight <- surveys %>%
  group_by(year, species_id) %>%
  summarize(avg_weight = mean(weight, na.rm = TRUE))

# --- 2. VISUALIZATION ---
# Create a professional time-series plot
ggplot(data = yearly_weight, 
       mapping = aes(x = year, 
                     y = avg_weight, 
                     group = species_id, 
                     color = species_id)) + 
  geom_line() + 
  theme_bw() + 
  labs(title = "Average Weight Change Over Time by Species",
       x = "Year",
       y = "Average Weight (g)",
       color = "Species ID")

#Slide 24
# --- MODIFYING A THEME ---
# Beyond premade themes, we can selectively remove 
# components using element_blank().

ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n, 
                     color = species_id)) + 
  geom_line() + 
  theme_bw() + 
  # Removing all grid lines for a minimalist high-contrast look
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank())

# Slide 25
# --- CUSTOMIZING AXIS LABELS AND TITLES ---
# We use the labs() function to provide human-readable titles 
# and axis labels, which is essential for professional reporting.

ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n, 
                     color = species_id)) + 
  geom_line() + 
  # Adding descriptive labels
  labs(title = 'Observed Species in Time', 
       x = 'Year of observation', 
       y = 'Count') + 
  theme_bw()

#Slide 26
# --- CUSTOMIZING FONT SIZE AND LABELS ---
# We use labs() for descriptive text and theme(text = ...) 
# to control global typographical settings.

ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n, 
                     color = species_id)) + 
  geom_line() + 
  # Adding clear, professional labels
  labs(title = 'Observed Species in Time', 
       x = 'Year of observation', 
       y = 'Count') + 
  theme_bw() + 
  # Adjusting global text properties for better visibility
  theme(text = element_text(size = 16, 
                            family = "Arial"))

#Slide 27
# --- ROTATING AXIS LABELS ---
# We use the theme() function to target 'axis.text.x' and rotate it.
# This prevents labels from overlapping when space is tight.

ggplot(data = yearly_counts, 
       mapping = aes(x = year, 
                     y = n, 
                     color = species_id)) + 
  geom_line() + 
  labs(title = 'Species count over time', 
       x = 'Year of observation', 
       y = 'Count') + 
  theme_bw() + 
  theme(axis.text.x = element_text(color = "grey20", 
                                   size = 12, 
                                   angle = 90, 
                                   hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(color = "grey20", 
                                   size = 12),
        text = element_text(size = 16, 
                            family = "Arial"))

#Slide 28
# --- SAVING A CUSTOM THEME OBJECT ---
# We define our theme once and save it to an object.
# This allows us to apply the same look to any plot instantly.

arial_grey_theme <- theme(
  axis.text.x = element_text(color = "grey20", 
                             size = 12, 
                             angle = 90, 
                             hjust = 0.5, 
                             vjust = 0.5),
#Slide 29
# --- APPLYING YOUR SAVED THEME ---
# Notice how we can use the 'arial_grey_theme' object on a boxplot
# even though we originally designed it for a line graph.

ggplot(data = surveys, 
       mapping = aes(x = species_id, 
                     y = hindfoot_length)) + 
  geom_boxplot() + 
  arial_grey_theme

#Slide 30
# --- SAVING YOUR FINAL PLOT ---
# First, we assign the ggplot object to a variable name (my_plot).
# Then, we use ggsave() to export it as a high-resolution image.

arial_grey_theme <- theme(
  axis.text.x = element_text(color = "grey20", 
                             size = 12, 
                             angle = 90, 
                             hjust = 0.5, 
                             vjust = 0.5),
  axis.text.y = element_text(color = "grey20", 
                             size = 12),
  text = element_text(size = 16, 
                      family = "Arial")
)

# --- 2. CREATE AND SAVE THE PLOT OBJECT ---
my_plot <- ggplot(data = yearly_counts, 
                  mapping = aes(x = year, 
                                y = n, 
                                color = species_id)) + 
  geom_line() + 
  labs(title = 'Observed species in time', 
       x = 'Year of observation', 
       y = 'Number of species') + 
  theme_bw() +
  arial_grey_theme  # Applying your custom style here

# --- 3. EXPORT THE PLOT ---
ggsave("observed_species_plot.png", my_plot, width = 15, height = 10)