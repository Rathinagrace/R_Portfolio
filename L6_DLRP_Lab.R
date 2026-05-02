# =====================================================

# LESSON 6: Statistics Lab task
# Author: Rathina Grace Monica / Adjunct Lecturer

# =====================================================

# Print the mtcars data set
mtcars

# Use the question mark to get information about the data set

?mtcars

Data_Cars <- mtcars # create a variable of the mtcars data set for better organization

# Use dim() to find the dimension of the data set
dim(Data_Cars)

# Use names() to find the names of the variables from the data set
names(Data_Cars)

Data_Cars <- mtcars

#Use the rownames() function to get the name of each row in the first column,
#which is the name of each car:
rownames(Data_Cars)

#Print Variable Values
Data_Cars <- mtcars

Data_Cars$cyl

#Max Min
Data_Cars <- mtcars

max(Data_Cars$hp)
min(Data_Cars$hp)

# To find the index position
Data_Cars <- mtcars

which.max(Data_Cars$hp)
which.min(Data_Cars$hp)

# Combining which.max and which.min with row name
Data_Cars <- mtcars

rownames(Data_Cars)[which.max(Data_Cars$hp)]
rownames(Data_Cars)[which.min(Data_Cars$hp)]

# Mean, Median, Mode
# Find the average weight (wt) of a car:
Data_Cars <- mtcars

mean(Data_Cars$wt)

#Find the mid point value of weight (wt):
Data_Cars <- mtcars

median(Data_Cars$wt)

# Find the mode
Data_Cars <- mtcars

names(sort(-table(Data_Cars$wt)))[1]

#Percentile
Data_Cars <- mtcars

# c() specifies which percentile you want
quantile(Data_Cars$wt, c(0.75))

Data_Cars <- mtcars

quantile(Data_Cars$wt)









