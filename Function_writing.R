# function that adds up number of birds and dogs
# defined function
birddog_sum <- function(bird, dog){
  pets <- bird + dog
  return(pets)
}

#use it

birddog_sum(bird = 2, dog = 5)
total_pets <- birddog_sum(2, 5) # same as above

# create a function to double values

double_it <- function(x) {
  (print(2 * x))
}

  double_it
  
# Quarter split warm up
  
  quarter_splits <- c(1,1.1,1.2,1.1,1.4,1.5,1.6,1.4)
   
  for (i in seq_along(quarter_splits)) {
   half_splits <- (quarter_splits[i] + quarter_splits[i + 1])
}
   print(half_splits)
}
# write a function with conditionals
# example is converting animals' ages
   
animal_age <- function(animal, age) {
if (animal == "dog") {
 print(age * 7)
} else if (animal == "goat") {
  print(age * 4.7)
} else {
  print("Unkown Animal")
}
}

# try using for a 8 year old dog

animal_age("dog", 8)

# try using for a cow (doesnt work cuz there is no cow)
animal_age("cow", 8)

#update animal age function with error messages

animal_age_stop <- function(animal, age) {
  
  if(!animal %in% c("dog", "goat")) {
    stop("Oops!, Animal must be a dog or goat")
  }
  
  if(is.numeric(age) == FALSE) {
    stop("The age must be a number.")
  }
  
  if(age <= 0 | age > 50) {
    warning("Are you sure about your animal's age?")
  }
}
}

animal_age_stop("dog", 100)
animal_age_stop("elephant", 10)

# Functions meet for loops!

# all the data frames in the function are called df --> argument df
df_means <- function(df) {
  for (i in 1:ncol(df)) {
    if(is.numeric(df[[i]])) {
    column_name <- colnames(df[i])
    col_mean <- mean(df[[i]], na.rm = TRUE)
    print(paste("The mean value of", column_name, "is", col_mean))
}
  }
}
df_means(df = palmerpenguins::penguins)
df_means()

## Logistic growth example
# Logistic growth equation
logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K-N0)/N0) * exp(-r * time))
  print(Nt)
}

# Check equation works
logistic_growth(100, 6000, 0.27, 40)

# Working on an example JUST dealing with iterations [i] of time
time_vec <- seq(from = 0, to = 35, by = 0.1)

# apply the logistic growth function to that vector 
pop_35 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

# combining time steps and population size into a dataframe
pop_time_35 <- data.frame(time_vec, pop_35)

# plot it!
library(ggplot2)
ggplot(data = pop_time_35, aes(x = time_vec, y = pop_35)) + 
  geom_line(size = 0.5)

# alternatively, with an internal for loop
# pre-allocated storage for output vector
pop_35_vec <- vector(mode = "numeric", length = length(time_vec))

# for loop for stepping throwgh time steps
for(i in seq_along(time_vec)){
population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
pop_time_35[i] <- population
}

# now, building to estimating cross growth rate
# creating a series of growth rates:
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

# creating a Matrix to store output values: 
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for(j in seq_along(r_seq)) {
  for(i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, 
                                r = r_seq[j], time = time_vec[i])
  out_matrix[i, j] <- population
}
}

# data wrangling to plot
# adding time as a variable
out_df <- data.frame(out_matrix, time = time_vec)

# update column names for growth rats
colnames(out_df) <- c(paste0("gr_", r_seq),"time")

# pivot longer to make it tidy
library(tidyverse)

out_df_long <- out_df %>% 
  pivot_longer(cols = -time, 
               names_to = "growth_rate",
               values_to = "population")
# plot it
ggplot(data = out_df_long, aes(x = time, y = population)) +
  geom_line(aes(color = growth_rate)) +
  theme_minimal()
