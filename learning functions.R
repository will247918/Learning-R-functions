library('tidyverse')


add_one <- function(input_data) {
  return(input_data + 1)
}
add_one(10)
number_series <- c(1,5,10)

add_one(number_series)

# Use the instructions above to complete the function below
variance <- function(input_data){
  square <- sqrt(input_data)
  ss <- sum(square)
  variance <- (ss/(length(input_data)-1))
  return(variance)
}

# Use vector (or make a new one) and try out your new function


variance(number_series)



say_hello <- function(){
  paste("Hello World") 
}

say_hello()


say_morning <- function(x){
  paste("Good morning", x)
}

#  what about this one?
say_morning("Phil")

say_morning_default <- function(name = "you"){
  paste("Good morning", name)
}

say_morning_default()



report_p <- function(p, digits = 3) {
  roundp <- round(p, digits)
  reported <-  paste("p =", roundp)
  
  return(reported)
}


report_p <- function(p, digits = 3) {
  
  if(!is.numeric(p)) stop("p must be a number")
  if(p<=0) warning("p-values cannot be less than 0")
  if(p>=1) warning("p-values cannot be greater than 1")
  
  
  
  reported <- if_else(p < 0.001,
                      paste("p < 0.001"),
                      paste("p=", round(p, digits)))
  
  return(reported)
}
report_p(0.32432)


function(input_data) {
  return(input_data + 1)
}

# Make some fake data into a tibble

vial <- (c((1:10),(1:10)))
sex <- (c(rep("male",10),rep("female", 10)))
weight_mg <- c(rnorm(10, mean=0.2, sd=0.02), rnorm(10, mean=0.21, sd=0.01))

dros_weight <- tibble(vial, sex, weight_mg)
dros_weight


heaviest_male <- function(dros_weight){
  
male_drosophilla <- filter(dros_weight,sex=='male')
 

heavy_male <- max(weight_mg) 
paste("heaviest male =", heavy_male)
}

heaviest_male(dros_weight)

heaviest_drosophilla <- function(dros_weight){
male_drosophilla <- filter(dros_weight, sex=='male')

heavy_male <- summarise(male_drosophilla,max(weight_mg))

female_drosophilla <- filter(dros_weight, sex=='female')
female_drosophilla %>%
  heavy_female <- max(weight_mg)
paste('Male and Female heaviest weights are:',c(heavy_male,heavy_female))
  
}
heaviest_drosophilla(dros_weight)





