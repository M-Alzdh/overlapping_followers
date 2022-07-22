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
  data[[i]] <- sample(IDs, cut_off_10000(rnorm(1, 5000, 5000)))
}


# calculate overlap between different accounts


overlap <- matrix(nrow = 10, ncol = 10)

for(i in 1:10){
  for(j in 1:10){
    overlap[i, j] <- (length(intersect(data[[i]], data[[j]])))/(length(data[[i]]))
  }
}

# turning data into the long format

tb_overlap <- as_tibble(overlap) %>% 
  mutate(rowid = paste("inf", 1:10)) %>% 
  select(rowid, everything())
tb_overlap
tb_overlap_long <- pivot_longer(tb_overlap, cols= -rowid)

tb_overlap_long

ggplot(data = tb_overlap_long)+
  geom_tile(aes(rowid, name, fill = value))+
  scale_fill_fermenter()
