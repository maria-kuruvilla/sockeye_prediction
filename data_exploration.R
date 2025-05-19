# goal -  plot Fraser data



library(ggplot2)
library(tidyverse)
library(here)


# read data ---------------------------------------------------------------

fraser_brood <- read.csv(here("data", "Fraser_Brood_Table.csv"))

fraser_return <- read.csv(here("data", "Fraser_Return_Table.csv"))<- read.csv(here("data", "Fraser_Return_Table.csv"))

glimpse(fraser_brood) #starts 1948  # recruits

glimpse(fraser_return) # starts 1951 #spawners


#plot returns

ggplot(fraser_return) + 
  geom_line(aes(x = ReturnYear, y = Total_Returns, group = River, 
                color = River), 
            alpha = 0.5, 
            size = 1) +
  theme_classic()


# combine to single data frame 


fraser_combined <- fraser_brood %>% 
  left_join(fraser_return, by = c("System","River", "ReturnYear"))




