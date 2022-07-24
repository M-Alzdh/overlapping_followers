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
    abs(x)
  }
}

#????? this line does not work the first time around


for(i in 1:10){
  size <- as.integer(cut_off_10000(rnorm(1, 3000, 5000)))
  data[[i]] <- sample(IDs, size)
}


# calculate overlap between different accounts

#using just intersection

overlap <- matrix(nrow = 10, ncol = 10)

for(i in 1:10){
  for(j in 1:10){
    overlap[i, j] <- (length(intersect(data[[i]], data[[j]])))/(length(data[[i]]))
  }
}

#using jaccard index

jaccard_indx <- matrix(nrow = 10, ncol = 10)
  
  for(i in 1:10){
  for(j in 1:10){
    jaccard_indx[i, j] <- (length(intersect(data[[i]], data[[j]])))/(length(union(data[[i]], data[[j]])))
  }
}


#using sorenson-dice
sorenson_dice_indx <- matrix(nrow = 10, ncol = 10)

for(i in 1:10){
  for(j in 1:10){
    sorenson_dice_indx[i, j] <- 2*(length(intersect(data[[i]], data[[j]])))/(length(data[[i]]) + length(data[[j]]))
  }
}


# turning data into the long format

tb_overlap <- as_tibble(overlap) %>% 
  mutate(rowid = paste("inf", 1:10)) %>% 
  select(rowid, everything())

tb_overlap_long <- pivot_longer(tb_overlap, cols= -rowid)

tb_jaccard_indx <- as_tibble(jaccard_indx) %>% 
  mutate(rowid = paste("inf", 1:10)) %>% 
  select(rowid, everything())
tb_jaccard_indx <- pivot_longer(tb_jaccard_indx, cols= -rowid)

tb_sorenson_dice_indx <- as_tibble(sorenson_dice_indx) %>% 
  mutate(rowid = paste("inf", 1:10)) %>% 
  select(rowid, everything())
tb_sorenson_dice_indx <- pivot_longer(tb_sorenson_dice_indx, cols= -rowid)


# plotting

ggplot(data = tb_overlap_long)+
  geom_tile(aes(rowid, name, fill = value))

ggplot(data = tb_jaccard_indx)+
  geom_tile(aes(name, rowid, fill = value))

ggplot(data = tb_sorenson_dice_indx)+
  geom_tile(aes(name, rowid, fill = value))
