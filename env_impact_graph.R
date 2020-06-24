library(tidyverse)
library(scatterpie)

setwd('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo')

dataset <- read_csv("Final_results_new_v2.csv")

dataset <- tidyr::gather(dataset, "Year", "Value", 4:7)

dataset = subset(dataset, dataset$Impact!="Water Cons. Green [BCM]")

dataset$Region[dataset$Region=="Median"] <- "Baseline"

ggplot(dataset)+
  geom_point(aes(x=Value,y=Year,color=Region),size=5,alpha=0.6)+
  geom_point(data = dataset %>% group_by(Year,Impact) %>% summarize(median=median(Value)) %>% ungroup(),
             aes(x=median,y=Year, shape="Median"),color="Black",size=5, shape=3)+
  facet_wrap(~Impact,scales = "free_x")+
  scale_color_discrete(name="Demand and tech. convergence variant")+
  labs(x="",y="")+
  theme(legend.position="top")

ggsave("impacts.png", last_plot(), device = "png", scale=2)

ggplot(dataset)+
  geom_boxplot(aes(x = Year, y = Value, fill=Region))+
  facet_wrap(~Impact,scales = "free_y")+
  scale_fill_discrete(name="Demand and tech. convergence variant")+
  labs(x="",y="")+
  theme(legend.position="top")
  

ggsave("impacts2.png", last_plot(), device = "png", scale=2)

