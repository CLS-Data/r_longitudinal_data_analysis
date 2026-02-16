# Background on `R` {#sec-background_r} ----
knitr::opts_chunk$set(echo = TRUE)

## Vectorisation and Recycling ----
c(1, 2, 3, 4, 5) + c(2, 4, 6, 1, 2)

c(1, 2, 3) + c(1, 3)

## Data Structures ----
c(1, "two", TRUE)

typeof(1:3)
is.integer(1:3)
is.double(1:3)
is.numeric(1:3) # Both integer and double are 'numeric'

list(
  c(1, 2, 3),
  c("a", "b"),
  c(1, "two", TRUE), # Observe: coerced to a character vector
  list(1, "two", TRUE, c(4, 5, 6))
)

head(iris) # An example data.frame

## Subsetting ----
ages <- c("Gary" = 10, "Bertie" = 20, "Peter" = 30)
ages[c(3, 2, 1, 1)]
ages[c(TRUE, FALSE, TRUE)]
ages[c("Bertie", "Gary")]

my_list <- list(
  numbers = c(1, 2, 3),
  letters = c("a", "b", "c"),
  mixed = list(1, "two", TRUE)
)
my_list[c(3, 1)]

my_list[2] # Returns a list of length 1.
my_list[[2]] # Returns a vector
my_list$letters
my_list$lett # Partial matching.

my_df <- data.frame(
  name = c("Gary", "Bertie", "Peter"),
  age = c(10, 20, 30),
  score = c(90, 85, 101),
  nationality = factor(c("English", "Scottish", "English"))
)
my_df[[2]] # Extracts the 'age' column as a vector
my_df["age"] # New data.frame only containing 'age' column

my_df[3:2, c("name", "score")] # Extracts first two rows of 'name' and 'score' columns
my_df[, c("name", "score")] # Extracts all rows of 'name' and 'score' columns

library(tibble)
my_tibble <- as_tibble(my_df)
my_df[, "age"] # Returns a vector
my_tibble[, "age"] # Returns a tibble

my_list[["mixed"]][[2]]

variable_dict <- c(age = "Age of Participant", score = "Test Score")
paste("The Average", variable_dict["score"], "is", mean(my_df$score))

## Constructing Logical and Integer Vectors and the Missing Value ----
ages[ages == 25]
ages[ages >= 20]
ages[ages %in% c(10, 30)]

ages[ages >= 28 | (ages <= 23 & ages != 20)]

ages_na <- c(ages, Dave = NA)
ages_na > 20
ages_na[ages_na > 20]
ages_na[na.omit(ages_na > 20)] # Omits NAs
ages_na[ages_na %in% c(10, NA)]
ages_na[is.na(ages_na)]

which(ages >= 20)
match(ages, c(30, 10, 40))
seq(0, 10, by = 2)

### Task I ----
my_list$letters[c(3, 1)]

my_df[my_df$score == max(my_df$score), c("name", "age")]

## Attributes and Generic Functions ----
attr(my_df, "names") # Get column names
names(my_df) <- c("Participant_Name", "Participant_Age", "Test_Score", "Nationality") # Set column names
my_df

summary(my_df)
summary(lm(Test_Score ~ Participant_Age, data = my_df))

summary

summary.data.frame(my_df)

class(my_tibble)

## Packages, Functions and Environments ----
library(dplyr)
library(MASS) # Masks dplyr::select()
select(my_df, "Test_Score")
select <- dplyr::select # Ensures dplyr::select() is used in next line
select(my_df, "Test_Score")

x <- 3
y <- 15
add_1 <- function(z){
  x <- y + 1
  return(x)
}
add_1()
x

## Control Flow ----

mean_with_checks <- function(x){
  if(!is.numeric(x)){
    stop("x must be numeric")
  } else if(any(is.na(x))){
    warning("x contains missing values; these will be ignored")
    mean(x, na.rm = TRUE)
  } else {
    mean(x)
  }
}

mean_with_checks("not a number")
mean_with_checks(c(1, 3, NA))
mean_with_checks(c(1, 3, 4))

### Task II ----
y <- 1

add_y <- function(x){
  x + y
}

add_x_and_y <- function(){
  y <- 15
  add_y(x = 3)
}

y <- 1

add_y_and_z <- function(){
  add_z <- function(z){
    y + z
  }
  y <- 15
  add_z(z = 3)
}

mean_with_checks_v2 <- function(x){
  if(!is.numeric(x)){
    stop("x must be numeric")
  }
  if(any(is.na(x))){
    warning("x contains missing values; these will be ignored")
    mean(x, na.rm = TRUE)
  }
}

## `.Rproj` and Environment Variables ----
mcs_fld <- Sys.getenv("mcs_fld")
ncds_fld <- Sys.getenv("ncds_fld")

## Further Reading ----
