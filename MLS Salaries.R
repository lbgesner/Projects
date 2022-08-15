#Install necessary packages
install.packages("fs")
library(tidyverse)
library(scales)
library(readr)
library(fs)
library(dplyr)
#read in file paths
file_paths <- fs::dir_ls("C:/Users/Liam/Documents/GitHub/Projects/Projects/MLS")
file_paths
#for loop to read in csv files to a list
file_contents <- list()

for(i in seq_along(file_paths)){
  file_contents[[i]] <- read.csv(
    file = file_paths[[i]]
  )
}
#add year column to each file

for(i in seq_along(file_contents)){
  Year <- 2006
  Year <- Year +i
  file_contents[[i]]$Year <- Year
}
#join the list of dataframes into a single dataframe
MLS_Salaries <- bind_rows(file_contents)
