library(tidyverse)

ghg <- read.csv("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/Results/GHG detail for 2050-SSP1-North Africa and Middle East-baseline.csv")

ghg <- ghg[-1,]

levels(ghg$X) <- c("CH4", "CO2 biogenic", "CO2 fuels", "GHG", "N2O")

ggplot(ghg)+
  geom_bar(aes(x=X, y=X2050, fill=X), stat="identity")+
  ylab("GtonCO2_eq / year")+
  xlab("")+
  scale_fill_brewer(palette = "Set1", name="GHG")+
  ggtitle("CO2 equiv. emissions in 2050 by GHG, median scenario")

ggsave("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/ghg_split.png",last_plot(),  device = "png", scale=1)

##

fuels <- read.csv("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/Results/Use_of_FF_2050-SSP2-Median-baseline/Use_of_FF_plot1.csv")

fuels = fuels %>% dplyr::select(-CBA, -PBA)

ggplot(fuels)+
  geom_bar(aes(x=X, y=IBA/1000000000, fill=X.1, group=X), stat="identity")+
  ylab("EJ / year")+
  xlab("")+
  scale_fill_brewer(palette = "Set1", name="Sector")+
  ggtitle("Sectoral fossil fuels consumption in 2050, median scenario")

ggsave("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/fuels_split.png",last_plot(),  device = "png", scale=1)

