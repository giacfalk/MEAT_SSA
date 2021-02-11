library(tidyverse)
setwd('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA')

temp <- read_csv(paste0("Results/Final_results_tot.csv"))
names(temp)[c(1:3)] <- c("Impact","Region","SSP")
temp <- gather(temp, "Year", "Value", 4:7) %>% mutate(Type="tot")

ghg <- filter(temp, temp$Impact=="CO2_f [GtonCO2_eq]" | temp$Impact=="CH4 [GtonCO2_eq]" | temp$Impact=="N2O [GtonCO2_eq]" | temp$Impact=="CO2_b [GtonCO2_eq]")

ghg <- filter(ghg, Year==2050) %>% group_by(Impact) %>% summarise(Value=median(Value))

ghg$Impact <- c("CH4", "CO2 biogenic", "CO2 fuels", "N2O")

ggplot(ghg)+
  geom_bar(aes(x=Impact, y=Value, fill=Impact), stat="identity")+
  ylab("GtonCO2_eq / year")+
  xlab("")+
  scale_fill_brewer(palette = "Set1", name="GHG")+
  ggtitle("CO2 equiv. emissions in 2050 by GHG, median scenario")

ggsave("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/ghg_split.png",last_plot(),  device = "png", scale=1, width = 6)

##

fuels <- read.csv("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/MRIO/Results/Allocation of total Fossil Fuel [GJ] - Case 2050_SSP4_East Asia.csv")

fuels = fuels %>% dplyr::select(-CBA, -PBA)

ggplot(fuels)+
  geom_bar(aes(x=X, y=IBA/1000000000, fill=X.1, group=X), stat="identity")+
  ylab("EJ / year")+
  xlab("")+
  scale_fill_brewer(palette = "Set1", name="Sector")+
  ggtitle("Sectoral fossil fuels consumption in 2050, median scenario")

ggsave("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/fuels_split.png",last_plot(),  device = "png", scale=1, width = 6)

