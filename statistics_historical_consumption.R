library(tidyverse)

meat <- read.csv("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/Data/per-capita-meat-consumption-by-type-kilograms-per-year.csv")

meat$all <- rowSums(meat[4:7], na.rm = T)

meat <- dplyr::filter(meat, Year==1961) %>% select(all, Entity)

pop <- wbstats::wb(country = "all", startdate = 1961, enddate = 1961, indicator = "SP.POP.TOTL")

meat$iso2c = countrycode::countrycode(meat$Entity, "country.name", "iso2c")

pop <- merge(pop, meat, by="iso2c")

pop$all = pop$all * pop$value

pop$region <- countrycode::countrycode(pop$iso3c, 'iso3c', 'region')

pop <- filter(pop, grepl("Africa", pop$region))
pop <- filter(pop, !grepl("Northern Africa", pop$region))

pop <- filter(pop, iso2c!="ZA")

pop <- dplyr::summarise(pop, all=sum(all, na.rm = T), value=sum(value, na.rm = T))

pop$all / pop$value
