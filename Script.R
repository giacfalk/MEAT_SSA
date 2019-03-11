######MEATSSA###########
#A simple model for projecting meat consumption scenarios
#and related environmental impact in Sub-Saharan Africa#
########################

### Version: 11/03/19 ###

#Load required libraries
library(ggplot2)
library(dplyr)
library(countrycode)
library(reshape2)
library(padr)
library(cowplot)
library(scales)
library(imputeTS)
library(tidyr)

#Set wd depending on which computer you are working
setwd("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\Data")
setwd("D:\Dropbox (FEEM)\Meat Africa\Data\\Meat Africa\\Data")

#################
#1) Produce some descriptive statistics and graphs
#1.1. on meat consumption
pc_cons<-read.csv("meat-consumption-vs-gdp-per-capita.csv")
p = pc_cons %>% dplyr::group_by(Year, Entity) %>% dplyr::summarise(pc_cons = mean(Meat.consumption.per.capita..kilograms.per.year.))
pc_cons_regional<-subset(pc_cons, Entity== "Africa" | Entity== "Asia"| Entity== "Northern America"| Entity== "South America"| Entity== "Australia & New Zealand"| Entity== "Europe")
pc_cons_regional$Entity=as.character(pc_cons_regional$Entity)
pc_cons_regional$Entity[pc_cons_regional$Entity == "Northern America"] <- "North America"
pc_cons_regional$Entity[pc_cons_regional$Entity == "Australia & New Zealand"] <- "Oceania"

fig1a = ggplot(pc_cons_regional, aes(x=Year, y=Meat.consumption.per.capita..kilograms.per.year., colour=Entity, group=Entity)) + 
  geom_line(size=1)+
  ylab("Meat consumption in kg/capita/year")+
  labs(color = "Regions\n")+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

#ggsave("figure1a.png", plot = fig1a, device = "png", width = 20, height = 15, units = "cm", scale=0.6)

pc_cons_bytipe<-read.csv("per-capita-meat-consumption-by-type-kilograms-per-year.csv")

pc_cons_bytipe_regional<-subset(pc_cons_bytipe, Entity== "Africa" | Entity== "Asia"| Entity== "Americas"| Entity== "Australia & New Zealand"| Entity== "Europe")
pc_cons_bytipe_regional$Entity=as.character(pc_cons_bytipe_regional$Entity)
pc_cons_bytipe_regional$Entity[pc_cons_bytipe_regional$Entity == "Northern America"] <- "North America"
pc_cons_bytipe_regional$Entity[pc_cons_bytipe_regional$Entity == "Australia & New Zealand"] <- "Oceania"

a<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
geom_line(aes(y=Beef.and.buffalo..kg.), size=1) + 
ylab("Beef and buffalo, kg/capita/year")+
scale_color_discrete(name="Region")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))


b<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
  geom_line(aes(y=Poultry..kg.), size=1) + 
  ylab("Poultry, kg/capita/year")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))


c<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
  geom_line(aes(y=Pigmeat..kg.), size=1) + 
  ylab("Pork, kg/capita/year")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))


d<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
  geom_line(aes(y=Mutton...goat..kg.), size=1) + 
  ylab("Mutton and goat, kg/capita/year")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

combo_type<-plot_grid((a+theme(legend.position="none")), (b+theme(legend.position="none")), (c+theme(legend.position="none")), (d+theme(legend.position="none")), labels = "AUTO", label_size = 9, hjust= 0)
legend <- get_legend(a)
p <- plot_grid(combo_type, legend, ncol = 2, rel_widths = c(0.6, .2))
#ggsave("combo_type.png", plot = p, device = "png", width = 26, height = 18, units = "cm", scale=0.7)

pc_cons_bytipe_regional2 = pc_cons_bytipe_regional %>% dplyr::select(-Code)
pc_cons_bytipe_regional2 = melt(pc_cons_bytipe_regional2, id.vars=c("Entity", "Year"), measure.vars= c("Mutton...goat..kg.", "Beef.and.buffalo..kg.", "Pigmeat..kg.", "Poultry..kg."))

a = ggplot(subset(pc_cons_bytipe_regional2, pc_cons_bytipe_regional2$Year == 1961),aes(x = Entity, y = value ,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  ggtitle("1961")+
  xlab("Region")+
  ylab("Share of consumption (%)")+
  scale_fill_discrete(name="Meat Type", labels=c("Mutton", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

b = ggplot(subset(pc_cons_bytipe_regional2, pc_cons_bytipe_regional2$Year == 1980),aes(x = Entity, y = value ,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  ggtitle("1980")+
  xlab("Region")+
  ylab("Share of consumption (%)")+
  scale_fill_discrete(name="Meat Type", labels=c("Mutton", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

c = ggplot(subset(pc_cons_bytipe_regional2, pc_cons_bytipe_regional2$Year == 2000),aes(x = Entity, y = value ,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  ggtitle("2000")+
  xlab("Region")+
  ylab("Share of consumption (%)")+
  scale_fill_discrete(name="Meat Type", labels=c("Mutton", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

d = ggplot(subset(pc_cons_bytipe_regional2, pc_cons_bytipe_regional2$Year == 2013),aes(x = Entity, y = value ,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  ggtitle("2013")+
  xlab("Region")+
  ylab("Share of consumption (%)")+
  scale_fill_discrete(name="Meat Type", labels=c("Mutton", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

library(cowplot)
combo_type<-plot_grid((a+theme(legend.position="none")), (b+theme(legend.position="none")), (c+theme(legend.position="none")), (d+theme(legend.position="none")))
legend <- get_legend(a)
fig1b <- plot_grid(combo_type, legend)

#ggsave("figure1b.png", plot = fig1b, device = "png", width = 40, height = 18, units = "cm", scale=0.9)

#Further examplificative plots
pc_gdp_pc<-read.csv("meat-consumption-vs-gdp-per-capita.csv")
pc_gdp_pc_regional<-subset(pc_gdp_pc, Entity== "China" | Entity== "India"| Entity== "Germany"| Entity== "Italy"| Entity== "Australia"| Entity== "United States")
pc_gdp_pc_regional<-subset(pc_gdp_pc_regional, Year > 1900)

#Trends in dplyr::selected countries of the world
world<-ggplot(pc_gdp_pc_regional, aes(x=cgdppc, y=Meat.consumption.per.capita..kilograms.per.year., colour=Entity)) + 
  geom_point(size=1)+
  stat_smooth(se=TRUE, fill=NA,colour="red")+
  ylab("Meat consumption: kg/capita/year")+
  xlab("PPP per-capita GDP")+
  facet_wrap(~Entity, scales="free")+
  labs(color = "Regions\n")+
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom", axis.text=element_text(size=8))

#Trends in dplyr::selected countries of SSA
pc_gdp_pc_regional_Africa<-subset(pc_gdp_pc, Entity== "Kenya" | Entity== "South Africa"| Entity== "Nigeria"| Entity== "Ghana"| Entity== "Botswana"| Entity== "Ethiopia")
pc_gdp_pc_regional_Africa<-subset(pc_gdp_pc_regional_Africa, Year > 1960)

africa<-ggplot(pc_gdp_pc_regional_Africa, aes(x=cgdppc, y=Meat.consumption.per.capita..kilograms.per.year., colour=Year)) + 
  geom_point(size=1)+
  stat_smooth(se=TRUE, fill=NA,colour="red")+
  ylab("Meat consumption: kg/capita/year")+
  xlab("PPP per-capita GDP")+
  facet_wrap(~Entity, scales="free")+
  labs(color = "Regions\n")+
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom", axis.text=element_text(size=8))

#1.2. Figures on meat-industry emissions
setwd("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\Data")

GHG<-readxl::read_excel("Total_livestock_emissions_GLEAM_2017.xlsx")

GHG_plot = ggplot(data=GHG, aes(x=Region, y=MilliontonnesCO2eq/1000)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))+
  ylab(expression("Gigatons " *"CO"["2"]^{"equiv"}))

#ggsave("figureghg.png", plot = GHG_plot, device = "png", width = 25, height = 18, units = "cm", scale=0.75)

#1.3 Figures on historical land-use change
#sr <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
lc_1700 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1700.asc")
#proj4string(lc_1700) <- CRS("+init=epsg:4326") 
#lc_1700 <- projectRaster(lc_1700, crs = sr)
lc_1700 = as.data.frame(tapply(area(lc_1700), lc_1700[], sum), colnames="1700")

lc_1750 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1750.asc")
lc_1750 = as.data.frame(tapply(area(lc_1750), lc_1750[], sum), colnames="1750")

lc_1800 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1800.asc")
lc_1800 = as.data.frame(tapply(area(lc_1800), lc_1800[], sum), colnames="1800")

lc_1850 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1850.asc")
lc_1850 = as.data.frame(tapply(area(lc_1850), lc_1850[], sum), colnames="1850")

lc_1900 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1900.asc")
lc_1900 = as.data.frame(tapply(area(lc_1900), lc_1900[], sum), colnames="1900")

lc_1950 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1950.asc")
lc_1950 = as.data.frame(tapply(area(lc_1950), lc_1950[], sum), colnames="1950")

lc_1970 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1970.asc")
lc_1970 = as.data.frame(tapply(area(lc_1970), lc_1970[], sum), colnames="1970")

lc_1990 = raster("C:/Users/Falchetta/Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1990.asc")
lc_1990 = as.data.frame(tapply(area(lc_1990), lc_1990[], sum), colnames="1990")

lc = cbind(lc_1700, lc_1750, lc_1800, lc_1850, lc_1900, lc_1950, lc_1970, lc_1990)
colnames(lc)=c(1700, 1750, 1800, 1850, 1900, 1950, 1970, 1990)
lc = subset(lc, rownames(lc) == "1" | rownames(lc) == "2")
rownames(lc)=c("Cultivated land", "Land used for grazing")
lc$landtype=c("Cultivated land", "Land used for grazing")

lc = data.table(lc)

lc = melt(lc, id.vars="landtype", value.name="area", variable.name = "year")

landusechangeplot = ggplot(lc, aes(x=year, y=area, group=landtype, colour=landtype))+
  geom_line(size=1.2)+
  xlab("Year")+
  ylab("Area (km2)")+
  scale_colour_discrete(name="Legend")
  
#ggsave("figureluc.png", plot = landusechangeplot, device = "png", width = 25, height = 18, units = "cm", scale=0.75)

##2) Regress GDP and meat consumption (demand-side) to derive coefficients for scenario projection
library(dplyr)
library(countrycode)

pc_cons<-read.csv("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\Data\\meat-consumption-vs-gdp-per-capita.csv")
maddison = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\Data\\Maddison_historical.xlsx")
pc_cons = merge(pc_cons, maddison, by=c("Code", "Year"))
pc_cons = pc_cons[!nchar(as.character(pc_cons$Code)) > 3, ]
pc_cons = pc_cons %>% dplyr::select(Code, Year, Meat.consumption.per.capita..kilograms.per.year., cgdppc)
pc_cons$Code=as.character(pc_cons$Code)
pc_cons_bytipe<-read.csv("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\Data\\per-capita-meat-consumption-by-type-kilograms-per-year.csv")
pc_cons_bytipe = pc_cons_bytipe %>% dplyr::select(-Entity)
pc_cons_bytipe$Code=as.character(pc_cons_bytipe$Code)

pc_cons_bytipe = subset(pc_cons_bytipe, pc_cons_bytipe$Code !="")
pc_cons = subset(pc_cons, pc_cons$Code !="")

y = merge(pc_cons, pc_cons_bytipe, by=c("Code", "Year"))

url = "https://raw.githubusercontent.com/vincentarelbundock/countrycode/master/data/extra/globalburdenofdisease.csv"
state_dict = read.csv(url, stringsAsFactors=FALSE)
y$continent=countrycode(y$Code, 'iso3c', 'gbd_region', custom_dict=state_dict, origin_regex=TRUE)
y$macrocontinent=countrycode(y$Code, 'iso3c', 'continent')

####
#Specify region to regress on

region_regression="High-income North America"

#Beef 
global = subset(y, y$continent == region_regression)
formula = "Beef.and.buffalo..kg. ~ cgdppc + I(cgdppc^2) + factor(Code) + factor(Year) + Poultry..kg.*Pigmeat..kg. +  Mutton...goat..kg.*Pigmeat..kg. + Mutton...goat..kg.*Poultry..kg. +0"
global = lm(formula, data = global)
summary(global)

#Pig
global = subset(y, y$continent == region_regression)
formula = "Pigmeat..kg. ~ cgdppc + I(cgdppc^2) + factor(Code) + factor(Year) + Poultry..kg.*Beef.and.buffalo..kg. +  Mutton...goat..kg.*Beef.and.buffalo..kg. + Mutton...goat..kg.*Poultry..kg. +0"
global = lm(formula, data = global)
summary(global)

#Poultry
global = subset(y, y$continent == region_regression)
formula = "Poultry..kg. ~ cgdppc + I(cgdppc^2) + factor(Code) + factor(Year) + Pigmeat..kg.*Beef.and.buffalo..kg. +  Mutton...goat..kg.*Beef.and.buffalo..kg. + Mutton...goat..kg.*Pigmeat..kg. +0"
global = lm(formula, data = global)
summary(global)

#Mutton-goat
global = subset(y, y$continent == region_regression)
formula = "Mutton...goat..kg. ~ cgdppc + I(cgdppc^2) + factor(Code) + factor(Year) + Poultry..kg.*Beef.and.buffalo..kg. +  Pigmeat..kg.*Beef.and.buffalo..kg. + Pigmeat..kg.*Poultry..kg. +0"
global = lm(formula, data = global)
summary(global)

#Produce figure of quadratic trends for all types of meat
require(scales)
histev<-ggplot(y, aes(x=cgdppc, y=Meat.consumption.per.capita..kilograms.per.year., colour=macrocontinent)) + 
  geom_point(size=1, alpha=0.5)+
  scale_x_continuous(labels = comma)+
  scale_color_discrete(name="Region")+
  theme_light()+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  ylab("Meat consumption: kg/capita/year")+
  xlab("PPP per-capita GDP")

#ggsave("histev.png", plot = histev, device = "png", width = 18, height = 12, units = "cm", scale=0.9)


#################
#3) Design scenarios projecting the SSPs
#import the SSPs
ssps = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db.xlsx")
ssps_countrylevel = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_countrylevel.xlsx")
ssps_countrylevel = ssps_countrylevel %>% mutate_if(is.numeric, funs(.*1000))


#adjust unit of dollars (from 2005 to 2011 PPP per-capita GDP)
library(wbstats)
adjfactor = wb(indicator = "PA.NUS.PPP", startdate = 2005, enddate = 2011)
adjfactor = adjfactor %>% dplyr::select(iso3c, date, value)
adjfactor = adjfactor %>% subset(date == 2011 | date == 2005) %>% group_by(iso3c) %>% summarise(value= value[1]/value[2])  
ssps_countrylevel = merge(ssps_countrylevel, adjfactor, by.x="Region", by.y="iso3c")
ssps_countrylevel = ssps_countrylevel %>% mutate_if(is.numeric, funs(.*value))

##
ssps_countrylevel$continent=countrycode(ssps_countrylevel$Region, 'iso3c', 'continent')
ssps_countrylevel = ssps_countrylevel[complete.cases(ssps_countrylevel[ , 26]),]
ssps_countrylevel = subset(ssps_countrylevel, ssps_countrylevel$continent == "Africa")
ssps_countrylevel=subset(ssps_countrylevel, Region != "ATF" & Region != "EGY" & Region != "ESH"& Region != "ESP" & Region != "LBY" & Region != "MAR" & Region != "MYT" & Region != "SYC" & Region != "COM" & Region != "YEM" & Region != "TUN" & Region != "DZA" & Region != "SHN" & Region != "DJI" & Region != "STP")
pr = ssps_countrylevel %>% dplyr::select(-c(Model, Variable, Unit, Notes, continent))
ssps_countrylevel = melt(pr, id.vars=c("Region", "Scenario"))
ssps_countrylevel <- split(ssps_countrylevel, ssps_countrylevel$Scenario)

#Define the intercept terms, which is given by today's average level of consumption
#Beef
africa = subset(y, y$continent =="Western Sub-Saharan Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Beef.and.buffalo..kg., na.rm = TRUE))

#Pork
africa = subset(y, y$continent =="Western Sub-Saharan Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Pigmeat..kg., na.rm = TRUE))

#Poultry
africa = subset(y, y$continent =="Western Sub-Saharan Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Poultry..kg., na.rm = TRUE))

#Mutton
africa = subset(y, y$continent =="Western Sub-Saharan Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Mutton...goat..kg., na.rm = TRUE))


###SSP 1 - Beef
#Scenario 1: the Global average path
SSP1 = as.data.frame(ssps_countrylevel$SSP1)
SSP1 = SSP1 %>% group_by(variable) %>% mutate(value=median(value))
provaccia = SSP1 %>% group_by(variable) %>% mutate(meatcons =  1.353e-03*value  +  -1.357e-08*value*value  + 3.94) 
provaccia = provaccia %>% group_by(variable) %>% summarise(meatcons = median(meatcons))
#africa = subset(pc_gdp_pc_regional, pc_gdp_pc_regional$continent =="Africa")
africa_edit = africa %>% group_by(Year) %>% summarise(meatcons=median(Beef.and.buffalo..kg., na.rm = TRUE))
provaccia$Year = as.numeric(substr(provaccia$variable, 1, 4))
provaccia = dplyr::select(provaccia, meatcons, Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Actual", "Projected")
prova = na.omit(prova)

SSP1GA_pc = ggplot(prova, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita meat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP1 - global-average-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")

ssps_pop = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP1")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
prova = merge(prova, ssps_pop, by.x="Year", by.y="variable")
prova$pop = prova$value*1000000
prova$total = prova$meatcons*prova$pop

SSP1GA_aggregate = ggplot(prova, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total meat consumption in SSA (Mt)")+
  ggtitle("SSP1 - global-average-like pathway")+
  scale_colour_discrete(name="Legend \n")

#Scenario 2: the Asian path
SSP1 = as.data.frame(ssps_countrylevel$SSP1)
SSP1 = SSP1 %>% group_by(variable) %>% mutate(value=median(value))
provaccia = SSP1 %>% group_by(variable) %>% mutate(meatcons =  2.347e-04*value + -1.548e-09*value*value  + 16.04) 
provaccia = provaccia %>% group_by(variable) %>% summarise(meatcons = median(meatcons))
africa = subset(pc_gdp_pc_regional, pc_gdp_pc_regional$continent =="Africa")
africa_edit = africa %>% group_by(Year) %>% summarise(meatcons=median(Meat.consumption.per.capita..kilograms.per.year., na.rm = TRUE))
provaccia$Year = as.numeric(substr(provaccia$variable, 1, 4))
provaccia = dplyr::select(provaccia, meatcons, Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Actual", "Projected")
prova = na.omit(prova)

SSP1AS_pc = ggplot(prova, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita beef consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP1 - Asia-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")

ssps_pop = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP1")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
prova = merge(prova, ssps_pop, by.x="Year", by.y="variable")
prova$pop = prova$value.y*1000000
prova$total = prova$meatcons*prova$pop

SSP1AS_aggregate = ggplot(prova, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total meat consumption in SSA (Mt)")+
  ggtitle("SSP1 - Asia-like pathway")+
  scale_colour_discrete(name="Legend \n")

#Scenario 3: cultural patterns will evolve towards an EU-like path
SSP1 = as.data.frame(ssps_countrylevel$SSP1)
SSP1 = SSP1 %>% group_by(variable) %>% mutate(value=median(value))
provaccia = SSP1 %>% group_by(variable) %>% mutate(meatcons =  -7.772e-04*value  +  1.959e-08*value*value  + -1.156e-13*value*value*value + 16.04) 
provaccia = provaccia %>% group_by(variable) %>% summarise(meatcons = median(meatcons))
africa = subset(pc_gdp_pc_regional, pc_gdp_pc_regional$continent =="Africa")
africa_edit = africa %>% group_by(Year) %>% summarise(meatcons=median(Meat.consumption.per.capita..kilograms.per.year., na.rm = TRUE))
provaccia$Year = as.numeric(substr(provaccia$variable, 1, 4))
provaccia = dplyr::select(provaccia, meatcons, Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Actual", "Projected")
prova = na.omit(prova)

SSP1EU_pc = ggplot(prova, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita meat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP1 - EU-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")

ssps_pop = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP1")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
prova = merge(prova, ssps_pop, by.x="Year", by.y="variable")
prova$pop = prova$value.y*1000000
prova$total = prova$meatcons*prova$pop

SSP1EU_aggregate = ggplot(prova, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total meat consumption in SSA (Mt)")+
  ggtitle("SSP1 - EU-like pathway")+
  scale_colour_discrete(name="Legend \n")


#Scenario 4: the American path


###SSP 1 - Poultry
#Scenario 1: the Global average path
SSP1 = as.data.frame(ssps_countrylevel$SSP1)
SSP1 = SSP1 %>% group_by(variable) %>% mutate(value=median(value))
provaccia = SSP1 %>% group_by(variable) %>% mutate(meatcons =   2.204e-08*value*value  + 3.07) 
provaccia = provaccia %>% group_by(variable) %>% summarise(meatcons = median(meatcons))
#africa = subset(pc_gdp_pc_regional, pc_gdp_pc_regional$continent =="Africa")
africa_edit = africa %>% group_by(Year) %>% summarise(meatcons=median(Poultry..kg., na.rm = TRUE))
provaccia$Year = as.numeric(substr(provaccia$variable, 1, 4))
provaccia = dplyr::select(provaccia, meatcons, Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Actual", "Projected")
prova = na.omit(prova)

SSP1GA_pc = ggplot(prova, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita poultry consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP1 - global-average-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")

ssps_pop = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP1")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
prova = merge(prova, ssps_pop, by.x="Year", by.y="variable")
prova$pop = prova$value*1000000
prova$total = prova$meatcons*prova$pop

SSP1GA_aggregate = ggplot(prova, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total meat consumption in SSA (Mt)")+
  ggtitle("SSP1 - global-average-like pathway")+
  scale_colour_discrete(name="Legend \n")



##SSP2



##SSP3



##SSP4



##SSP5
#Scenario 1: the Global average path
SSP5 = as.data.frame(ssps_countrylevel$SSP5)
SSP5 = SSP5 %>% dplyr::group_by(variable) %>% dplyr::mutate(value=median(value))
provaccia = SSP5 %>% dplyr::group_by(variable) %>% dplyr::mutate(meatcons =  2.347e-04*value  +  -1.548e-09*value*value  + 3.94) 
provaccia = provaccia %>% dplyr::group_by(variable) %>% dplyr::summarise(meatcons = median(meatcons))
#africa = subset(pc_gdp_pc_regional, pc_gdp_pc_regional$continent =="Africa")
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Beef.and.buffalo..kg., na.rm = TRUE))
provaccia$Year = as.numeric(substr(provaccia$variable, 1, 4))
provaccia = dplyr::select(provaccia, meatcons, Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Actual", "Projected")
prova = na.omit(prova)

SSP5GA_pc = ggplot(prova, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita meat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP5 - global-average-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")

ssps_pop = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP5")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
prova = merge(prova, ssps_pop, by.x="Year", by.y="variable")
prova$pop = prova$value*1000000
prova$total = prova$meatcons*prova$pop

SSP5GA_aggregate = ggplot(prova, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total meat consumption in SSA (Mt)")+
  ggtitle("SSP5 - global-average-like pathway")+
  scale_colour_discrete(name="Legend \n")

#Scenario 2: the Asian path
SSP5 = as.data.frame(ssps_countrylevel$SSP5)
SSP5 = SSP5 %>% group_by(variable) %>% mutate(value=median(value))
provaccia = SSP5 %>% group_by(variable) %>% mutate(meatcons =  2.080e-03*value  +  -1.179e-08*value*value  + 16.04) 
provaccia = provaccia %>% group_by(variable) %>% summarise(meatcons = median(meatcons))
africa = subset(pc_gdp_pc_regional, pc_gdp_pc_regional$continent =="Africa")
africa_edit = africa %>% group_by(Year) %>% summarise(meatcons=median(Meat.consumption.per.capita..kilograms.per.year., na.rm = TRUE))
provaccia$Year = as.numeric(substr(provaccia$variable, 1, 4))
provaccia = dplyr::select(provaccia, meatcons, Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Actual", "Projected")
prova = na.omit(prova)

SSP5AS_pc = ggplot(prova, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita meat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP5 - Asia-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")

ssps_pop = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP5")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
prova = merge(prova, ssps_pop, by.x="Year", by.y="variable")
prova$pop = prova$value.y*1000000
prova$total = prova$meatcons*prova$pop

SSP5AS_aggregate = ggplot(prova, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total meat consumption in SSA (Mt)")+
  ggtitle("SSP5 - Asia-like pathway")+
  scale_colour_discrete(name="Legend \n")

#3) EU
SSP5 = as.data.frame(ssps_countrylevel$SSP5)
SSP5 = SSP5 %>% group_by(variable) %>% mutate(value=median(value))
provaccia = SSP5 %>% group_by(variable) %>% mutate(meatcons =  2.472e-03*value  +  -4.500e-08*value*value  + 2.591e-13*value*value*value + 16.04) 
provaccia = provaccia %>% group_by(variable) %>% summarise(meatcons = median(meatcons))
africa = subset(pc_gdp_pc_regional, pc_gdp_pc_regional$continent =="Africa")
africa_edit = africa %>% group_by(Year) %>% summarise(meatcons=median(Meat.consumption.per.capita..kilograms.per.year., na.rm = TRUE))
provaccia$Year = as.numeric(substr(provaccia$variable, 1, 4))
provaccia = dplyr::select(provaccia, meatcons, Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Actual", "Projected")
prova = na.omit(prova)

SSP5EU_pc = ggplot(prova, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita meat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP5 - EU-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")

ssps_pop = readxl::read_excel("C:\\Users\\Falchetta\\Dropbox (FEEM)\\Meat Africa\\iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP5")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
prova = merge(prova, ssps_pop, by.x="Year", by.y="variable")
prova$pop = prova$value.y*1000000
prova$total = prova$meatcons*prova$pop

SSP5EU_aggregate = ggplot(prova, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total meat consumption in SSA (Mt)")+
  ggtitle("SSP5 - EU-like pathway")+
  scale_colour_discrete(name="Legend \n")

#############
#Create combined plots for dplyr::selected scenarios
library(cowplot)
plot_grid(SSP1AS_pc, SSP1GA_pc, SSP5AS_pc, SSP5GA_pc, SSP1EU_pc, SSP5EU_pc)
plot_grid(SSP1AS_aggregate, SSP1GA_aggregate, SSP5AS_aggregate, SSP5GA_aggregate, SSP1EU_aggregate, SSP5EU_aggregate)


###############
##4) Analyse also the production-side (EXTRA, probably not for paper)
meat_prod_tonnes<-read.csv("meat-production-tonnes.csv")
meat_prod_tonnes<-subset(meat_prod_tonnes, Year > 1900)

consumption = pc_gdp_pc %>% dplyr::select(Meat.consumption.per.capita..kilograms.per.year., Entity, Year)

tot = merge(meat_prod_tonnes, consumption, by=c("Entity", "Year"))

library(wbstats)

gdp = wb(indicator = "NY.GDP.PCAP.PP.KD", startdate = 1961, enddate = 2013)
pop = wb(indicator = "SP.POP.TOTL", startdate = 1961, enddate = 2013)
tot2 = merge(gdp, pop, by=c("iso3c", "date"))
tot = merge(tot, tot2, by.x=c("Code", "Year"), by.y=c("iso3c", "date"))

tot$gdp = tot$value.x

tot = tot %>% group_by(Year) %>% mutate(globpop = sum(pop), avglobgdp = mean(gdp))

formula = "log(Livestock.Primary..Meat..Total...Production...tonnes..FAO..2017....tonnes.)  ~ log(gdp) + log(pop) + log(avglobgdp)"
ols1 = lm(formula, data = tot)
summary(ols1)

formula = "log(Livestock.Primary..Meat..Total...Production...tonnes..FAO..2017....tonnes.)  ~ log(gdp) + log(pop) + log(avglobgdp) + factor(Entity) "
ols2 = lm(formula, data = tot)
summary(ols2)

formula = "log(Livestock.Primary..Meat..Total...Production...tonnes..FAO..2017....tonnes.)  ~ log(gdp) + log(pop) + log(avglobgdp) + factor(Year) + factor(Entity) "
ols3 = lm(formula, data = tot)
summary(ols3)

#stargazer(ols1, ols2, ols3, type = "latex", dep.var.labels   = "Log of meat production", add.lines = list(c("Country fixed effects", "No", "Yes", "Yes"), c("Year fixed effects", "No", "No", "Yes")))

#####################################
#5) calculate environmental impacts
#5.1) GHG EMISSIONS
#5.1.1) import emission factors per kg by meat type, type of production, and region



#5.1.2) estimating total related emissions




#5.2) LAND
#5.2.1)import land-need per kg by different types of productions



#5.2.2)estimating land-needs




#5.3) ENERGY REQUIREMENTS
#5.3.1)import energy consumption per kg by meat type and region


#5.3.2)estimating related emissions





#5.4) WATER REQUIREMENTS
#5.5.1)import water consumption per kg by meat type and region


#5.5.2)estimating water consumption

