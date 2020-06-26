library(tidyverse)

rm(list=ls())
setwd('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA')
#setwd("C:/Users/miche/FONDAZIONE ENI ENRICO MATTEI/Giacomo Falchetta - MEAT/Repo/meatSSA")

dataset <- tibble()

for(i in c("tot","cow","pig","pou")) {

temp <- read_csv(paste0("Results/Final_results_",i,".csv"))
names(temp)[c(1:4)] <- c("Impact","Region","SSP","Approach")
temp <- gather(temp, "Year", "Value", 5:8) %>% filter(Approach=="baseline") %>% select(-Approach) %>% mutate(Type=i)

dataset <- bind_rows(dataset,temp)
}


dataset <- filter(dataset, dataset$Impact!="Water Cons. Green [BCM]")

dataset$Region[dataset$Region=="Median"] <- "Baseline"

dataset$Impact <- factor(dataset$Impact,
                         levels = c("Land [Mkm2]","Water Cons. Blue [BCM]","Fossil Fuels [EJ]","Electricity [TWh]",
                                    "GHG [GtonCO2_eq]","Eutrop. [MtonPO4_eq]"),
                         labels = c("Land~(Mkm^{2})","Blue~Water~Consumption~(Gm^{3})","Fossil~Fuels~(EJ)",
                                    "Electricity~(TWh)","GHG~(Gt[CO2_eq])","Eutrophication~(Mt[PO4_eq])"))


p1 <- ggplot(dataset %>% filter(Impact != "Electricity~(TWh)" & Type=="tot"))+
  geom_point(aes(y=Value,x=Year,color=Region),size=5,alpha=0.6,shape=18)+
  geom_line(aes(y=Value,x=Year,color=Region,group=interaction(SSP,Region)),size=5,alpha=0.1)+
  #geom_point(data = dataset %>%  filter(Impact != "Electricity [TWh]") %>% group_by(Year,Impact) %>%
  #             summarize(median=median(Value)) %>% ungroup(),
  #           aes(y=median,x=Year, shape="Median"),color="Black",size=5)+
  geom_line(data = dataset %>%  filter(Impact != "Electricity~(TWh)" & Type=="tot") %>% group_by(Year,Impact) %>%
               summarize(median=median(Value)) %>% ungroup(),
             aes(y=median,x=Year,group=Impact,linetype="Median"),color="Black",size=1)+
  facet_wrap(~Impact,scales = "free_y",labeller = label_parsed)+
  scale_color_brewer(name="Demand and tech.\nconvergence variant",palette = "Set1")+
  scale_linetype_manual(name="",values = "solid")+
  labs(x="",y="")+
  theme(legend.position = c(0.85, 0.2))

ggsave("impacts.png", p1, device = "png", dpi = 300, width = 3,height = 2, scale=3)


ggplot(dataset %>% filter(Impact == "Fossil~Fuels~(EJ)" & Type!="tot"))+
  geom_bar(aes(y=Value,x=Year,fill=Type),stat="identity")+
  facet_wrap(~Region)+
  scale_fill_brewer(name="Meat Type",palette = "Set1")+
  labs(x="",y="",title="Fossil fuels use (EJ)")+
  theme(legend.position = c(0.85, 0.2))

ggsave("impacts.png", p1, device = "png", dpi = 300, width = 3,height = 2, scale=3)




p2 <- ggplot(dataset %>% filter(Impact != "Electricity [TWh]"))+
  geom_boxplot(aes(x = Year, y = Value, fill=Region))+
  facet_wrap(~Impact,scales = "free_y")+
  scale_fill_brewer(name="Demand and tech.\nconvergence variant",palette = "Set1")+
  labs(x="",y="")+
  theme(legend.position = c(0.85, 0.2))


ggsave("impacts2.png", p2, device = "png", dpi = 300, width = 3,height = 2, scale=3)

p3 <- ggplot(dataset %>% filter(Impact != "Electricity [TWh]"))+
  geom_line(aes(x = Year, y = Value, color=Region,group=interaction(SSP,Region)),alpha=0.5)+
  geom_line(data = dataset %>% filter(Impact != "Electricity [TWh]") %>% group_by(Impact,Year,Region) %>% 
              summarize(Value=median(Value)) %>% ungroup(),
            aes(x = Year, y = Value, color=Region,group=Region))+
  facet_wrap(~Impact,scales = "free_y")+
  scale_color_brewer(name="Demand and tech.\nconvergence variant",palette = "Set1")+
  labs(x="",y="")+
  theme(legend.position = c(0.85, 0.2))


ggsave("impacts3.png", p3, device = "png", dpi = 300, width = 3,height = 2, scale=3)
