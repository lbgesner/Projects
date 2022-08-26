#Install necessary packages
install.packages("fs")
library(tidyverse)
library(scales)
library(readr)
library(fs)
library(dplyr)
library(ggplot2)
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
#group for minimum and maximum salary for each year
Min_Max_Salary_By_Year <- MLS_Salaries %>%
  group_by(`Year`) %>%
  summarise(
    minimum_Salary = paste0("$",formatC(min(`guaranteed_compensation`, na.rm = T), format = "f", digits = 2, big.mark = ",")),
    maximum_Salary = paste0("$",formatC(max(`guaranteed_compensation`, na.rm = T), format = "f", digits = 2, big.mark = ","))
  )
#group for number of clubs each year
Clubs_by_Year <- MLS_Salaries %>%
  group_by(`Year`) %>%
  summarise(
    number_of_clubs = length(unique(`club`))
  )
#group for league salary each year
Salary_By_Year <- MLS_Salaries %>%
  group_by(`Year`) %>%
  summarise(
    mean_Salary = paste0("$",formatC(mean(`guaranteed_compensation`, na.rm = T), format = "f", digits = 2, big.mark = ",")),
    total_Salary = paste0("$",formatC(sum(`guaranteed_compensation`, na.rm = T), format = "f", digits = 2, big.mark = ",")),
    number_of_players = n()
  )
#salary by club each year
C_Salary_By_Year <- MLS_Salaries %>%
  group_by(`Year`, `club`) %>%
  summarise(
    number_of_players = n(),
    median_Salary = paste0("$",formatC(median(`guaranteed_compensation`, na.rm = T), format = "f", digits = 2, big.mark = ","))
  ) 
C_Salary_By_Year <- C_Salary_By_Year %>% filter(number_of_players > 9)
#salary by position each year
P_Salary_By_Year <- MLS_Salaries %>%
  group_by(`Year`, `position`) %>%
  summarise(
    number_of_players = n(),
    median_Salary = paste0("$",formatC(median(`guaranteed_compensation`, na.rm = T), format = "f", digits = 2, big.mark = ",")),
    mean_Salary = paste0("$",formatC(mean(`guaranteed_compensation`, na.rm = T), format = "f", digits = 2, big.mark = ","))
  ) 
P_Salary_By_Year <- P_Salary_By_Year %>% filter(number_of_players > 40)
#plotting interesting data that can be analyzed
p_club_growth <- ggplot(data = Clubs_by_Year, aes(x=as.character(`Year`), y=`number_of_clubs`, group = 1))+
  geom_bar(stat = "identity", color = "blue", fill = "white")+
  labs(x= "Year", y = "Clubs in MLS")+
  geom_text(aes(label = `number_of_clubs`)) +
  theme_classic()
plot(p_club_growth, type = "l")
p_min_max <- ggplot(data = Min_Max_Salary_By_Year, aes(x=as.character(`Year`), y=`minimum_Salary`, group = 1))+
  geom_bar(stat = "identity", fill = "black", color="lightgreen")+
  labs(x= "Year", y = "Minimum Salary")+
  theme_classic()
plot(p_min_max, type = "l")
p_position_salary <- ggplot(data = P_Salary_By_Year, aes(x=as.character(`Year`), y=parse_number(`median_Salary`),color = `position`, group = `position`))+
  geom_line(size = 2)+
  labs(x= "Year", y = "Median Salary")+
  theme_classic()
plot(p_position_salary, type = "l")
