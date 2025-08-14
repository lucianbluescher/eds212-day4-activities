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
  