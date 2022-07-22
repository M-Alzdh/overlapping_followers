library(tidyverse)

# Load first and last names to make usernames

random_names <- read_csv("names.csv")

first_names <- random_names %>% 
  select("First Name") %>% 
  unique()

last_name <- random_names %>% 
  select("Last Name") %>% 
  unique()

# convert tibbles to string vectors

v_first_names <- as_vector(first_names)
v_last_names <- as_vector(last_name)

#create IDs

IDs <- vector("character", 10000)

for(i in seq_along(IDs)){
  IDs[i] <- str_c(sample(v_first_names, 1), sample(v_last_names, 1), sample(1:10, 1), sep = "_")
}

#allocate followers to people

data <- list()

cut_off_10000 <- function(x){
  if(x>10000){
    10000
  } else{
    x
  }
}


for(i in 1:10){
  data[[i]] <- sample(IDs, cut_off_10000(rnorm(1, 5000, 1000)))
}

rm(list)