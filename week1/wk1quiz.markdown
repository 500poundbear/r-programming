R was developed by statisticians working at

> The University of Auckland

The definition of free software consists of four freedoms (freedoms 0 through
3). Which of the following is NOT one of the freedoms that are part of the
definition? Select all that apply.

> The freedom to sell the software for any price.

> The freedom to prevent users from using the software for undesirable purposes.

> The freedom to restrict access to the source code for the software.

In R the following are all atomic data types EXCEPT: (Select all that apply)

> data frame, complex, table, list, array, matrix

If I execute the expression x <- 4L in R, what is the class of the object `x' as
determined by the `class()' function?

> integer

What is the class of the object defined by the expression x <- c(4, "a", TRUE)?

> character

If I have two vectors x <- c(1,3, 5) and y <- c(3, 2, 10), what is produced by
the expression cbind(x, y)?

> a matrix with 2 columns and 3 rows

A key property of vectors in R is that

> elements of a vector all must be of the same class

Suppose I have a list defined as x <- list(2, "a", "b", TRUE). What does x[[2]]
give me? Select all that apply.

> a character vector containing the letter "a".

> a character vector of length 1.

Suppose I have a vector x <- 1:4 and a vector y <- 2. What is produced by the
expression x + y?

> a numeric vector with elements 3, 4, 5, 6.

Suppose I have a vector x <- c(3, 5, 1, 10, 12, 6) and I want to set all
elements of this vector that are less than 6 to be equal to zero. What R code
achieves this? Select all that apply.

> x[x %in% 1:5] <- 0, x[x <= 5] <- 0, x[x < 6] <- 0

In the dataset provided for this Quiz, what are the column names of the dataset?

> Ozone, Solar.R, Wind, Temp, Month, Day

Extract the first 2 rows of the data frame and print them to the console. What
does the output look like?

>  Ozone Solar.R Wind Temp Month Day
   1    41     190  7.4   67     5   1
   2    36     118  8.0   72     5   2

How many observations (i.e. rows) are in this data frame?

> 153

 Extract the last 2 rows of the data frame and print them to the console. What
 does the output look like?

>    Ozone Solar.R Wind Temp Month Day
152    18     131  8.0   76     9  29
153    20     223 11.5   68     9  30

What is the value of Ozone in the 47th row?

> 21

How many missing values are in the Ozone column of this data frame?

> 37

What is the mean of the Ozone column in this dataset? Exclude missing values
(coded as NA) from this calculation.

> 42.1

Extract the subset of rows of the data frame where Ozone values are above 31 and
Temp values are above 90. What is the mean of Solar.R in this subset?

> 212.8

What is the mean of "Temp" when "Month" is equal to 6?

> 79.1


What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?

> 115



