######MEATSSA###########
#A model for projecting meat consumption scenarios
#and related environmental impact in Sub-Saharan Africa#
########################
##Giacomo Falchetta, Michel Noussan
#Any question should be addressed to giacomo.falchetta@feem.it
### Version: 15/05/19 ###

library(tidyverse)

# csv input (value in kg)
consumption <- read_csv("consumption/output.csv") %>% select(-X1)

#generate impacts matrix
impacts <- crossing(Type=c("beef","mutton","pork","poultry"),Impact=c("GHG","Land","Energy","Water"),Median=0,Low=0,High=0) 

# beef is the only product for which an additional assumption needs to be done on diary vs beef distribution
diary_share <- 0.25

#yearly efficiency improvement factors 
reduction_factor_GHG <- 0.005
reduction_factor_water <- 0.005
reduction_factor_land <- 0.005
reduction_factor_energy <- 0.005


## GHG Emissions (kg CO2eq/FU, IPCC 2013 incl. CC feedbacks)

# beef

impacts$Median[impacts$Type=="beef" & impacts$Impact=="GHG"] <- diary_share*34.1 + (1-diary_share)*60.4 
impacts$Low[impacts$Type=="beef" & impacts$Impact=="GHG"] <- diary_share*17.9 + (1-diary_share)*40.4 
impacts$High[impacts$Type=="beef" & impacts$Impact=="GHG"] <- diary_share*50.9 +(1-diary_share)*209.9 

# mutton
impacts$Median[impacts$Type=="mutton" & impacts$Impact=="GHG"] <- 40.6
impacts$Low[impacts$Type=="mutton" & impacts$Impact=="GHG"] <- 24.5
impacts$High[impacts$Type=="mutton" & impacts$Impact=="GHG"] <- 54.4

# pork
impacts$Median[impacts$Type=="pork" & impacts$Impact=="GHG"] <- 10.6
impacts$Low[impacts$Type=="pork" & impacts$Impact=="GHG"] <- 7.4
impacts$High[impacts$Type=="pork" & impacts$Impact=="GHG"] <- 22.3

# poultry
impacts$Median[impacts$Type=="poultry" & impacts$Impact=="GHG"] <- 7.5
impacts$Low[impacts$Type=="poultry" & impacts$Impact=="GHG"] <- 4.2
impacts$High[impacts$Type=="poultry" & impacts$Impact=="GHG"] <- 20.1


## Land Use (m2/FU)

# beef

impacts$Median[impacts$Type=="beef" & impacts$Impact=="Land"] <- diary_share*25.9 + (1-diary_share)*170.4 
impacts$Low[impacts$Type=="beef" & impacts$Impact=="Land"] <- diary_share*14.4 + (1-diary_share)*82.8 
impacts$High[impacts$Type=="beef" & impacts$Impact=="Land"] <- diary_share*64.1 + (1-diary_share)*735.1 

# mutton
impacts$Median[impacts$Type=="mutton" & impacts$Impact=="Land"] <- 127.4
impacts$Low[impacts$Type=="mutton" & impacts$Impact=="Land"] <- 60.1
impacts$High[impacts$Type=="mutton" & impacts$Impact=="Land"] <- 442.3

# pork
impacts$Median[impacts$Type=="pork" & impacts$Impact=="Land"] <- 13.4
impacts$Low[impacts$Type=="pork" & impacts$Impact=="Land"] <- 7.8
impacts$High[impacts$Type=="pork" & impacts$Impact=="Land"] <- 31.1

# poultry
impacts$Median[impacts$Type=="poultry" & impacts$Impact=="Land"] <- 11.0
impacts$Low[impacts$Type=="poultry" & impacts$Impact=="Land"] <- 6.7
impacts$High[impacts$Type=="poultry" & impacts$Impact=="Land"] <- 16.0


## Freshwater Withdrawals (L/FU)

# beef

impacts$Median[impacts$Type=="beef" & impacts$Impact=="Water"] <- diary_share*2614 + (1-diary_share)*740 
impacts$Low[impacts$Type=="beef" & impacts$Impact=="Water"] <- diary_share*192 + (1-diary_share)*269 
impacts$High[impacts$Type=="beef" & impacts$Impact=="Water"] <- diary_share*5799 + (1-diary_share)*2586 

# mutton
impacts$Median[impacts$Type=="mutton" & impacts$Impact=="Water"] <- 461
impacts$Low[impacts$Type=="mutton" & impacts$Impact=="Water"] <- 98
impacts$High[impacts$Type=="mutton" & impacts$Impact=="Water"] <- 7133

# pork
impacts$Median[impacts$Type=="pork" & impacts$Impact=="Water"] <- 1810
impacts$Low[impacts$Type=="pork" & impacts$Impact=="Water"] <- 88
impacts$High[impacts$Type=="pork" & impacts$Impact=="Water"] <- 3315

# poultry
impacts$Median[impacts$Type=="poultry" & impacts$Impact=="Water"] <- 370
impacts$Low[impacts$Type=="poultry" & impacts$Impact=="Water"] <- 19
impacts$High[impacts$Type=="poultry" & impacts$Impact=="Water"] <- 1662


# explore consumption patterns

ggplot(consumption)+
  geom_line(aes(x=Year,y=total,color=Reference_country))+
  facet_wrap(~Type)

##calculate impacts

impacts2 <- full_join(consumption,impacts,by="Type") %>%
  mutate(Median = Median*total,Low=Low*total,High=High*total)

##plot impacts

ggplot(subset(impacts2,Impact != "Energy"))+
  geom_ribbon(aes(x=Year,ymin=Low,ymax=High,fill=Reference_country),alpha=0.25)+
  geom_line(aes(x=Year,y=Median,color=Reference_country),size=1)+
  facet_wrap(Impact~Type,scales = "free_y")+
  theme(legend.position = "bottom")

ggplot(subset(impacts2,Impact == "GHG"))+
  geom_ribbon(aes(x=Year,ymin=Low,ymax=High,fill=Reference_country),alpha=0.25)+
  geom_line(aes(x=Year,y=Median,color=Reference_country),size=1)+
  facet_wrap(~Type)+
  theme(legend.position = "bottom")


#GHG
#Plot (static in 2050)

#Plot (cumulative 2020-2050 accounting for adjustment factors)



#land use
#Plot (static in 2050)



#water
#Plot (static in 2050)

#Plot (cumulative 2020-2050 accounting for adjustment factors)


