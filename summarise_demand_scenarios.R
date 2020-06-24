setwd('D:\\Dropbox (FEEM)\\Meat Africa\\Repo\\meatSSA\\Data')

data <- read.csv('all_projections_2050_SSAfrica.csv')

library(tidyverse)
lol = data %>% mutate(pccons = Total_kg/Population) %>% filter(Year==2050) %>% group_by(Type) %>% summarise(min = min(pccons), median = median(pccons), max=max(pccons))

lol2 = data %>% filter(Year==2050) %>% group_by(Type) %>% summarise(median = median(Total_kg/1000000000))
