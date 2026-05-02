# =====================================================

# LESSON 4: Data Loading and Cleaning
# Author: Rathina Grace Monica / Adjunct Lecturer

# =====================================================

# 1. First, let's create a clean, consistent CSV file
clean_csv_text <- "id,name,salary,start_date,dept
1,Rick,623.30,2012-01-01,IT
2,Dan,515.20,2013-09-23,Operations
3,Michelle,611.00,2014-11-15,IT
4,Ryan,729.00,2014-05-11,HR
5,Gary,843.25,2015-03-27,Finance
6,Nina,578.00,2013-05-21,IT
7,Simon,632.80,2013-07-30,Operations
8,Guru,722.50,2014-06-17,Finance"

# Write this text to your local folder
writeLines(clean_csv_text, "input.csv")

# Now load the clean data
data <- read.csv("input.csv")

# Clean up any accidental spaces in the column names
names(data) <- trimws(names(data))



View(data)
print(data)

# Data Validation
# Check if the object was successfully imported as a Data Frame
print(is.data.frame(data))

# Structural Inspection
# Identify the dimensions of the dataset (Columns and Rows)
print(ncol(data)) # Number of variables/features
print(nrow(data)) # Number of observations/records

# ========================================================
# Get the max salary from data frame.
# ========================================================
sal <- max(data$salary) 
print(sal)

# ========================================================
# Get the person detail having max salary.
# ========================================================
retval <- subset(data, salary == max(salary)) 
print(retval)

# ========================================================
# Filter for the IT department
# ========================================================
retval <- subset(data, dept == "IT")

# Print the result
print(retval)

# Create a data frame. 
info <- subset(data, salary > 600 & dept == "IT")
print(info)

# ========================================================
# Get the people who joined on or after 2014
# ========================================================
retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))
print(retval) 

# ========================================================
# Create a new data frame. 
# ========================================================
retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01")) # Write filtered data into a new file. 
write.csv(retval,"output.csv") 
newdata <- read.csv("output.csv") 
print(newdata) 

C<-subset(data, salary>600, c(name, dept))
C

# ========================================================
# Install xlsx Package
# ========================================================
install.packages("xlsx") 

#Verify and Load the "xlsx" Package
# Verify the package is installed. 
any(grepl("xlsx",installed.packages())) 
# Load the library into R workspace.
library("xlsx") 

# Read the first worksheet in the file input.xlsx. 
data <- read.xlsx("input.xlsx", sheetIndex = 1) 
print(data) 

# ========================================================
# 1. Create your data
x <- 1:50

# 2. Save it directly into your R_Portfolio folder
# Using just the filename saves it to your current working directory
save(x, file = "x.Rdata")

# 3. Clean the environment to test it
rm(x)

# 4. Load it back from your portfolio folder
load("x.Rdata")

# Check if it's back
print(x)

# Install the complete tidyverse with
install.packages("tidyverse") 

data()

# ========================================================
# Loading a built in R-Data
# ========================================================
# Loading 
data(mtcars) 

# Print the first 6 rows 
head(mtcars, 6)

# Help
?mtcars

# The easiest way to get tidyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just tidyr:
install.packages("tidyr")

library(tidyr)

#relig_income - data about religious group in the US by tradition, family and denomination by Pew Religious center. 
head(relig_income)
# Pivot_longer increases the number of rows and decreases the number of columns of the data.
relig_income %>% pivot_longer(!religion, names_to = "income", values_to = "count")

#pivot_wider() - increases the number of columns and secreasing the number of rows. the inverse transformation of pivot_longer
head(us_rent_income)
us_rent_income %>% pivot_wider(names_from = variable,values_from = c(estimate,moe))

# ========================================================
# --- Create Data Frame ---
# ========================================================
player <- c('A', 'A', 'B', 'B', 'C', 'C')
df <- data.frame(
  player,
  year  = c(1, 2, 1, 2, 1, 2),
  stats = c('22-2', '29-3', '18-6', '11-8', '12-5', '19-2'),
  date  = c("27/01/2015", "23/02/2015", "31/03/2015", "20/01/2015", "23/02/2015", "31/01/2015")
)

df

library(tidyr)


#separate stats column into points and assists columns
a<-separate(df, col=date,into=c('date','month','year'),sep='/')
print(a)


b <-unite(a,Date,c(date,month,year),sep=".")
print(b)

# ========================================================
# Data Manipulation usin dplyr
# ========================================================
library(tidyverse)

library(dplyr)

# load flights dataset from nyflights13 which is a tibble
install.packages("nycflights13")
flights <- nycflights13::flights
head(flights)

# ========================================================
# Select
# ========================================================
# Select columns by name
head(select(flights, year, month, day))

# Select all columns except those from year to day (inclusive)
head(select(flights, -(year:day)))

#Use rename() rename ‘talinum’ as ‘tail_num’
head(rename(flights, tail_num = tailnum))

#move ‘time_hour’, ‘air_time’ to the begin 
head(select(flights, time_hour, air_time, everything()))

# ========================================================
# Arrange
# ========================================================
# Sort data by year, month, day
head(arrange(flights, year, month, day))

#Use desc() to re-order by a column in descending order:
head(arrange(flights, desc(dep_delay)))

#Missing values are always sorted at the end:
df <- tibble(x = c(5, NA, 2))
arrange(df, x)

# ========================================================
# Filtering
# ========================================================
# create flights dataset
flights <- nycflights13::flights
head(flights)

# filter flights data by month 
jan_data <- filter(flights, month == 1)
tail(jan_data)

# filter flights data by Date and Month.
Jan1 <- filter(flights,month==1,day==1)
head(Jan1)

# Filter all flights that departed in November or December:
nov_dec <- filter(flights, month == 11 | month == 12)
head(nov_dec)

#alternatively
nov_dec <- filter(flights, month %in% c(11, 12))
head(nov_dec)

# ========================================================
# Mutate
# ========================================================
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)

head(flights_sml)
head(mutate(flights_sml,gain=dep_delay-arr_delay,speed=distance/air_time*60))

head(mutate(flights_sml,
            gain = dep_delay - arr_delay,
            hours = air_time / 60,
            gain_per_hour = gain / hours
))

head(transmute(flights,
               gain = dep_delay - arr_delay,
               hours = air_time / 60,
               gain_per_hour = gain / hours
))

head(transmute(flights,
               dep_time,
               hour = dep_time %/% 60,
               minute = dep_time %% 60
))

# ========================================================
# Summarize
# ========================================================
# summarize - it collapses a data frame to a single row
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#Grouper Summaries with summarise
by_day <- group_by(flights, year, month, day)
head(summarise(by_day, delay = mean(dep_delay, na.rm = TRUE)))


by_dest <- group_by(flights, dest)

head(delay <- summarise(by_dest,
                        count = n(),
                        dist = mean(distance, na.rm = TRUE),
                        delay = mean(arr_delay, na.rm = TRUE)
))

# Combining multiple operation with the pipe
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

head(delays)

# ========================================================
# LESSON 4: Data Visualisation & Grammar of Graphics
# ========================================================

# 1. Load required libraries
library(ggplot2)
library(tidyr) # Required for cleaning the stats column

# 2. Prepare the Data (Must run before plotting)
player <- c('A', 'A', 'B', 'B', 'C', 'C')
df <- data.frame(
  player,
  year  = c(1, 2, 1, 2, 1, 2),
  stats = c('22-2', '29-3', '18-6', '11-8', '12-5', '19-2'),
  date  = c("27/01/2015", "23/02/2015", "31/03/2015", "20/01/2015", "23/02/2015", "31/01/2015")
)

# 3. Clean 'stats' so we have numerical values to plot
# We split '22-2' into 'points' and 'assists'
df_clean <- separate(df, col = stats, into = c('points', 'assists'), sep = '-')
df_clean$points <- as.numeric(df_clean$points)

# 4. The Layered Grammar of Graphics Plot
# We use df_clean to plot the actual points scored by each player
ggplot(data = df_clean) + 
  geom_bar(
    mapping = aes(x = player, y = points, fill = player), 
    stat = "identity",   # Use "identity" to plot the actual 'points' value
    position = "dodge"   # Places bars side-by-side
  ) +
  coord_flip() +         # Flips the axes for a horizontal bar chart
  facet_wrap(~year) +    # Creates two sub-plots: Year 1 and Year 2
  labs(
    title = "Player Performance Comparison",
    subtitle = "Separated by Year",
    x = "Player Name",
    y = "Total Points"
  ) +
  theme_minimal()        # Optional: Makes the chart look professional for your portfolio

# ========================================================
# Data Visualisation: Engine Size vs. Efficiency
# ========================================================

# 1. Load the library
library(ggplot2)

# 2. Access the mpg dataset (built into ggplot2)
data(mpg)

# 3. Create the Scatter Plot
# x = displ (Engine displacement/size in litres)
# y = hwy (Highway miles per gallon / Fuel efficiency)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  labs(
    title = "Engine Size vs. Highway Fuel Efficiency",
    x = "Engine Size (Litres)",
    y = "Highway MPG",
    color = "Vehicle Type"
  ) +
  theme_light()

# 1.Mapping class to the color aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# 2. Class to alpha aesthetic
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# 3. Class to size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# 4. Setting Aesthetic properties manually
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color='blue')

# 5. Displaying multiple geom in same plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+ geom_smooth(mapping = aes(x = displ, y = hwy))

# 6. Facets
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+facet_wrap(~class,nrow = 2) #Facet

#7.Facet Grid
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))+facet_grid(drv ~ cyl) # Facet Grid

# Statistical Transformation - Diamond Data set
# 1. Bar Plot
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut))

# 2. Bar Plot position adjustment
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut,color = cut))

# 3. With colour filling
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut,fill = cut))

# 4. Stacked bar
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut,fill = clarity))

# 5. Stacked bar
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut,fill = clarity),position = 'fill')

# 6. Position = dodge
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x=cut,fill = clarity),position = 'dodge')

# 7. Histogram
#Change colors
ggplot(mpg,aes(x=displ))+
  geom_histogram(color="black",fill="white")

# Coordinate Systems
# 8.Box Plot
ggplot(data = mpg,mapping = aes(x=class,y=hwy))+geom_boxplot()

# 9. Box Plot coordinate flip
ggplot(data = mpg,mapping = aes(x=class,y=hwy))+
  geom_boxplot() +coord

# 10. coord flip()
bar <- ggplot(data = diamonds)+
  geom_bar(mapping = aes (x=cut,fill=cut),
           show.legend = FALSE,
           width = 1) + theme(aspect.ratio = 1) + 
  labs( x = NULL, y = NULL)
bar + coord_flip()

# 11. coord polar()
bar <- ggplot(data = diamonds)+
  geom_bar(mapping = aes (x=cut,fill=cut),
           show.legend = FALSE,
           width = 1) + theme(aspect.ratio = 1) +
  labs( x = NULL, y = NULL)
bar + coord_polar()

#12. Theme Layer- element react function

ggplot(data = mtcars, aes(x=hp,y=mpg)) +
  geom_point() +facet_grid(.~cyl) + 
  theme(plot.background = element_rect(
    fill = 'yellow', colour = 'grey'))

#13. Theme Layer- element react function

ggplot(data = mtcars, aes(x=hp,y=mpg)) +
  geom_point() +facet_grid(am ~ cyl) + 
  theme_gray()



                                   