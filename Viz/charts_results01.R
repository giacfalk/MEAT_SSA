library(tidyverse)

#Set wd depending on which computer you are working
setwd("~/GitHub/MEAT_SSA/Viz")
rm(list=ls())


# read data from INPUT-OUTPUT results
# Note: the CSV has been manually processed for an easier import
dataset <- read_csv("Agg_res.csv")

dataset %>% pivot_longer(cols = -c(Impact,Scenario,SSP,Production),names_to = "Year",values_to = "Value") -> dataset

dataset %>% mutate(Impact = recode(Impact,Energy="Primary Energy Consumption (EJ)",GHG="GHG Emissions (Mt CO2eq)",
                                   Land = "Land use (Mkm2)",Water = "Blue water use (Gm3)")) -> dataset

# Plot figure of main results

p1 <- ggplot(dataset)+
  geom_point(aes(x=Value,y=Year,color=Production),size=5,alpha=0.5)+
  geom_point(data = dataset %>% group_by(Year,Impact) %>% summarize(median=median(Value)) %>% ungroup(),
             aes(x=median,y=Year,shape="Median"),color="Black",size=5)+
  scale_shape_manual(values = 3,name="")+
  scale_color_manual(name="",values=c("orange","dodgerblue","darkorchid"))+
  facet_wrap(~Impact,scales = "free_x")+
  labs(x="",y="")+
  #scale_y_continuous(limits = c(0,2),breaks = 1)+
  theme(legend.position="top")

ggsave("figure04a.png",p1,device="png",width = 3*2.2, height = 2*2.2,dpi=300)


# Analysis on CO2 emissions

datasetGHG <- read_csv("Agg_resCO2.csv")

datasetGHG %>% pivot_longer(cols = -c(Impact,Scenario,SSP,Production),names_to = "Year",values_to = "Value") -> datasetGHG


ggplot(datasetGHG %>% subset(Year>2020))+
  geom_boxplot(aes(x=Year,y=Value,fill=Impact))

ggplot(datasetGHG %>% subset(Year>2020))+
  geom_bar(aes(x=Year,y=Value,color=Scenario,fill=Production,alpha=SSP),stat="identity",position = "dodge")
