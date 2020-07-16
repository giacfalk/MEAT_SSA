setwd('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/Meat substitutes')

scenarios <- readxl::read_excel("Data.xlsx", sheet = "Scenarios")
impacts <- readxl::read_excel("Data.xlsx", sheet = "Data")


# extract impact percentiles from impacts csv of meat-specific results

library(tidyverse)
library(scatterpie)

setwd('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA')

dataset <- read_csv("Meat substitutes/Final_results_tot.csv")

dataset <- tidyr::gather(dataset, "Year", "Value", 4:7)

dataset = subset(dataset, dataset$Impact!="Water Cons. Green [BCM]")

dataset$Region[dataset$Region=="Median"] <- "Baseline"

dataset <- dataset %>% filter(Year==2050)

# merge with consumption numbers

consumption = read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo//meatSSA//Data//all_projections_2050_SSAfrica.csv') %>% filter(Year==2050) %>% dplyr::select(SSP_Scenario, Scenario_region, Type, Total_kg) 

consumption$SSP_Scenario = as.character(consumption$SSP_Scenario )
consumption$Scenario_region = as.character(consumption$Scenario_region)

dataset = merge(dataset, consumption, by.x=c("SSP", "Region"), by.y=c("SSP_Scenario", "Scenario_region"))

# reshape

library(broom)
dataset2 = dataset %>% filter(Year==2050) %>% group_by(Impact) %>% do( tidy(t(quantile(.$Value))) )

dataset3 = dataset %>% filter(Year==2050) %>% group_by(Type) %>% do( tidy(t(quantile(.$Total_kg))) )

dataset2 = gather(dataset2, "percentile", "value", 2:6)  

dataset2$percentile = gsub("X", "", dataset2$percentile)
dataset2$percentile = as.numeric(sub("[.]$", "", dataset2$percentile))

dataset3 = gather(dataset3, "percentile", "value", 2:6)  

dataset3$percentile = gsub("X", "", dataset3$percentile)
dataset3$percentile = as.numeric(sub("[.]$", "", dataset3$percentile))

# merge impacts with scenarios 

dataset2 = merge(dataset2, scenarios, by.x="percentile", by.y="Reference")

dataset = merge(dataset2, dataset3, by="percentile", all.x = T)

# simulate penetration and impacts

dataset$Type = as.character(dataset$Type)

dataset$value_alt = NA

#water#
dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==10 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.9 + (mean(impacts$LCA_l_bluewater_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==25 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.75 + (mean(impacts$LCA_l_bluewater_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==50 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.5 + (mean(impacts$LCA_l_bluewater_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==10 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.9 + (mean(impacts$LCA_l_bluewater_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==25 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.75 + (mean(impacts$LCA_l_bluewater_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==50 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.5 + (mean(impacts$LCA_l_bluewater_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==10 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.9 + (mean(impacts$LCA_l_bluewater_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==25 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.75 + (mean(impacts$LCA_l_bluewater_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==50 & dataset$Impact=="Water Cons. Blue [BCM]", dataset$value.x*0.5 + (mean(impacts$LCA_l_bluewater_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


#land#
dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==10 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.9 + (mean(impacts$LCA_m2_y_per_kg[2:7])*mean(dataset$value.y)*mean(impacts$prot_equiv_factor[2:7])*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==25 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.75 + (mean(impacts$LCA_m2_y_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==50 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.5 + (mean(impacts$LCA_m2_y_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==10 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.9 + (mean(impacts$LCA_m2_y_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==25 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.75 + (mean(impacts$LCA_m2_y_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==50 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.5 + (mean(impacts$LCA_m2_y_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==10 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.9 + (mean(impacts$LCA_m2_y_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==25 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.75 + (mean(impacts$LCA_m2_y_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==50 & dataset$Impact=="Land [Mkm2]", dataset$value.x*0.5 + (mean(impacts$LCA_m2_y_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)

#emissions#
dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==10 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.9 + (mean(impacts$LCA_kg_CO2eq_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==25 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.75 + (mean(impacts$LCA_kg_CO2eq_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==50 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.5 + (mean(impacts$LCA_kg_CO2eq_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==10 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.9 + (mean(impacts$LCA_kg_CO2eq_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==25 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.75 + (mean(impacts$LCA_kg_CO2eq_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==50 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.5 + (mean(impacts$LCA_kg_CO2eq_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==10 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.9 + (mean(impacts$LCA_kg_CO2eq_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==25 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.75 + (mean(impacts$LCA_kg_CO2eq_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==50 & dataset$Impact=="GHG [GtonCO2_eq]", dataset$value.x*0.5 + (mean(impacts$LCA_kg_CO2eq_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


#energy#
dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==10 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.9 + (mean(impacts$LCA_MJ_ton_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==25 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.75 + (mean(impacts$LCA_MJ_ton_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==50 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.5 + (mean(impacts$LCA_MJ_ton_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==10 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.9 + (mean(impacts$LCA_MJ_ton_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==25 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.75 + (mean(impacts$LCA_MJ_ton_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==50 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.5 + (mean(impacts$LCA_MJ_ton_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==10 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.9 + (mean(impacts$LCA_MJ_ton_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==25 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.75 + (mean(impacts$LCA_MJ_ton_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==50 & dataset$Impact=="Fossil Fuels [EJ]", dataset$value.x*0.5 + (mean(impacts$LCA_MJ_ton_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)

#eutrophication#
dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==10 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.9 + (mean(impacts$LCA_g_PO4equiv_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==25 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.75 + (mean(impacts$LCA_g_PO4equiv_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "beef" & dataset$Adoption==50 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.5 + (mean(impacts$LCA_g_PO4equiv_per_kg[2:7])*mean(impacts$prot_equiv_factor[2:7])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==10 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.9 + (mean(impacts$LCA_g_PO4equiv_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==25 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.75 + (mean(impacts$LCA_g_PO4equiv_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "poultry" & dataset$Adoption==50 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.5 + (mean(impacts$LCA_g_PO4equiv_per_kg[1])*mean(impacts$prot_equiv_factor[1])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==10 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.9 + (mean(impacts$LCA_g_PO4equiv_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.1)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==25 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.75 + (mean(impacts$LCA_g_PO4equiv_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.25)*1e-12, dataset$value_alt)

dataset$value_alt = ifelse(dataset$Type == "pork" & dataset$Adoption==50 & dataset$Impact=="Eutrop. [MtonPO4_eq]", dataset$value.x*0.5 + (mean(impacts$LCA_g_PO4equiv_per_kg[8])*mean(impacts$prot_equiv_factor[8])*mean(dataset$value.y)*0.5)*1e-12, dataset$value_alt)


# group all meat types
dataset_summarised = dataset %>% group_by(percentile, Impact, Adoption) %>% summarise(value_alt=median(value_alt, na.rm = T), value.x =median(value.x, na.rm = T))
  

#############

# plot #

library(reshape2)
dataset_summarised = gather(dataset_summarised, "type", "value_impact", c(4,5))

dataset_summarised$type <- ifelse(dataset_summarised$type=="value.x", "Baseline", "Subst. adoption")

dataset_summarised = subset(dataset_summarised, dataset_summarised$Impact!="Electricity [TWh]")

ggplot(dataset_summarised, aes(x=as.factor(Adoption/100), y=value_impact, fill=type))+
  geom_boxplot(alpha=0.75)+ 
  facet_wrap(~Impact, scales = "free_y")+
  xlab("Substitutes adoption rate")+
  ylab("Impact specfic unit")+
  scale_x_discrete(labels = c("10%", "25%", "50%"))+
  ggtitle("Environmental benefit of meat substitutes adoption (grams of protein equivalent, year 2050)")+
  scale_fill_discrete(name="Scenario")+
  theme(legend.position = c(0.85, 0.2))

ggsave("substitutes.png", last_plot(), device = "png", scale=1.25)

dataset_summarised %>% filter(Adoption==50 & percentile==50) %>% group_by(Impact) %>% summarise(value=value_impact[1]/value_impact[2])

### 

# ### WARNING: need meat-specific environmental impact results to plot this ###
# 
# library(reshape2)
# dataset_bytype = gather(dataset, "scenario", "value_impact", c(3,8))
# 
# dataset_bytype = subset(dataset_bytype, dataset$Type!="mutton")
# 
# dataset_bytype$scenario <- ifelse(dataset_bytype$scenario=="value.x", "Baseline", "Subst. adoption")
# dataset_bytype$Type <- ifelse(dataset_bytype$Type=="beef", "Beef",  ifelse(dataset_bytype$Type=="pork", "Pork", "Poultry"))
# 
# ggplot(dataset_bytype, aes(x=as.factor(Adoption/100), y=value_impact, fill=scenario))+
#   geom_boxplot(alpha=0.75)+ 
#   facet_wrap(Type~Impact, scales = "free_y")+
#   xlab("Substitutes adoption rate")+
#   ylab("Impact specfic unit")+
#   scale_x_discrete(labels = c("10%", "25%", "50%"))+
#   ggtitle("Environmental benefit of meat substitutes adoption")+
#   scale_fill_discrete(name="Category")
# 
# ggsave("substitutes_SI.png", last_plot(), device = "png", scale=1.5)
