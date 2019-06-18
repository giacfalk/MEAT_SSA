######MEATSSA###########
#A model for projecting meat consumption scenarios
#and related environmental impact in Sub-Saharan Africa#
########################
##Giacomo Falchetta, Michel Noussan
#Any question should be addressed to giacomo.falchetta@feem.it
### Version: 12/05/19 ###

#Load required libraries
library(ggplot2)
library(dplyr)
library(lmtest)
library(systemfit)
library(raster)
library(texreg)
library(countrycode)
library(wbstats)
library(reshape2)
library(padr)
library(cowplot)
library(scales)
library(imputeTS)
library(tidyr)

#Set wd depending on which computer you are working
setwd("D:\\Dropbox (FEEM)\\Meat Africa\\Repo\\meatSSA\\Data")

###############
##Index##
###############
#1) Descriptive statistics and graphs
#2) Regress GDP and meat consumption (demand-side) to derive coefficients for scenario projection
#3) Design scenarios projecting the SSPs
#4) Create combined plots for selected scenarios
#5) Export scenarios
##############


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
  ylab("Total meat consumption \n (kg/capita/year)")+
  labs(color = "Regions\n")+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

#ggsave("figure1a.pdf", plot = fig1a, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

pc_cons_bytipe<-read.csv("per-capita-meat-consumption-by-type-kilograms-per-year.csv")

pc_cons_bytipe_regional<-subset(pc_cons_bytipe, Entity== "Africa" | Entity== "Asia"| Entity== "Americas"| Entity== "Australia & New Zealand"| Entity== "Europe")
pc_cons_bytipe_regional$Entity=as.character(pc_cons_bytipe_regional$Entity)
pc_cons_bytipe_regional$Entity[pc_cons_bytipe_regional$Entity == "Northern America"] <- "North America"
pc_cons_bytipe_regional$Entity[pc_cons_bytipe_regional$Entity == "Australia & New Zealand"] <- "Oceania"

a<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
geom_line(aes(y=Beef.and.buffalo..kg.), size=1) + 
ylab("Beef and buffalo \n (kg/capita/year)")+
scale_color_discrete(name="Region")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))


b<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
  geom_line(aes(y=Poultry..kg.), size=1) + 
  ylab("Poultry \n (kg/capita/year)")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))


c<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
  geom_line(aes(y=Pigmeat..kg.), size=1) + 
  ylab("Pork \n (kg/capita/year)")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))


d<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
  geom_line(aes(y=Mutton...goat..kg.), size=1) + 
  ylab("Mutton and goat \n (kg/capita/year)")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

combo_type<-plot_grid((a+theme(legend.position="none")), (b+theme(legend.position="none")), (c+theme(legend.position="none")), (d+theme(legend.position="none")), labels = "AUTO", label_size = 9, hjust= 0)
legend <- get_legend(a)
p <- plot_grid(combo_type, legend, ncol = 2, rel_widths = c(0.6, .2))
#ggsave("combo_type.pdf", plot = p, device = "pdf", width = 26, height = 18, units = "cm", scale=0.7)

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
  scale_fill_discrete(name="Meat Type", labels=c("Mutton and goat", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

b = ggplot(subset(pc_cons_bytipe_regional2, pc_cons_bytipe_regional2$Year == 1980),aes(x = Entity, y = value ,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  ggtitle("1980")+
  xlab("Region")+
  ylab("Share of consumption (%)")+
  scale_fill_discrete(name="Meat Type", labels=c("Mutton and goat", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

c = ggplot(subset(pc_cons_bytipe_regional2, pc_cons_bytipe_regional2$Year == 2000),aes(x = Entity, y = value ,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  ggtitle("2000")+
  xlab("Region")+
  ylab("Share of consumption (%)")+
  scale_fill_discrete(name="Meat Type", labels=c("Mutton and goat", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

d = ggplot(subset(pc_cons_bytipe_regional2, pc_cons_bytipe_regional2$Year == 2013),aes(x = Entity, y = value ,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = percent_format())+
  ggtitle("2013")+
  xlab("Region")+
  ylab("Share of consumption (%)")+
  scale_fill_discrete(name="Meat Type", labels=c("Mutton and goat", "Beef and buffalo", "Pigmeat", "Poultry"))+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

combo_type<-plot_grid((a+theme(legend.position="none")), (b+theme(legend.position="none")), (c+theme(legend.position="none")), (d+theme(legend.position="none")))
legend <- get_legend(a)
fig1b <- plot_grid(combo_type, legend, ncol = 2, rel_widths = c(0.6, .1))

#ggsave("figure1b.pdf", plot = fig1b, device = "pdf", width = 26, height = 18, units = "cm", scale=0.9)

#Further examplificative plots
pc_gdp_pc<-read.csv("meat-consumption-vs-gdp-per-capita.csv")
pc_gdp_pc_regional<-subset(pc_gdp_pc, Entity== "China" | Entity== "India"| Entity== "France"| Entity== "Italy"| Entity== "South Africa"| Entity== "Japan")
pc_gdp_pc_regional<-subset(pc_gdp_pc_regional, Year > 1900)

#Trends in dplyr::selected countries of the world
world<-ggplot(pc_gdp_pc_regional, aes(x=GDP.per.capita..constant.2011.international..., y=Meat.consumption.per.capita..kilograms.per.year., colour=Entity)) + 
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
GHG<-readxl::read_excel("D:\\Dropbox (FEEM)\\Meat Africa\\Resources\\Total_livestock_emissions_GLEAM_2017.xlsx")

GHG_plot = ggplot(data=GHG, aes(x=Region, y=MilliontonnesCO2eq/1000)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))+
  ylab(expression("Gigatons " *"CO"["2"]^{"equiv"}))

#ggsave("figureghg.pdf", plot = GHG_plot, device = "pdf", width = 25, height = 18, units = "cm", scale=0.75)

#1.3 Figures on historical land-use change
#sr <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
lc_1700 = raster("Data/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1700.asc")
proj4string(lc_1700) <- CRS("+init=epsg:4326") 
lc_1700 <- projectRaster(lc_1700, crs = sr)
lc_1700 = as.data.frame(tapply(area(lc_1700), lc_1700[], sum), colnames="1700")

lc_1750 = raster("D:\\Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1750.asc")
lc_1750 = as.data.frame(tapply(area(lc_1750), lc_1750[], sum), colnames="1750")

lc_1800 = raster("D:\\Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1800.asc")
lc_1800 = as.data.frame(tapply(area(lc_1800), lc_1800[], sum), colnames="1800")

lc_1850 = raster("D:\\Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1850.asc")
lc_1850 = as.data.frame(tapply(area(lc_1850), lc_1850[], sum), colnames="1850")

lc_1900 = raster("D:\\Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1900.asc")
lc_1900 = as.data.frame(tapply(area(lc_1900), lc_1900[], sum), colnames="1900")

lc_1950 = raster("D:\\Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1950.asc")
lc_1950 = as.data.frame(tapply(area(lc_1950), lc_1950[], sum), colnames="1950")

lc_1970 = raster("D:\\Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1970.asc")
lc_1970 = as.data.frame(tapply(area(lc_1970), lc_1970[], sum), colnames="1970")

lc_1990 = raster("D:\\Dropbox (FEEM)/Meat Africa/Data/historical landcover change/ISLSCP_II_HLANDCOVER_967/data/historic_landcover_hdeg/historic_landcover_hd_1990.asc")
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
  
#ggsave("figureluc.pdf", plot = landusechangeplot, device = "pdf", width = 25, height = 18, units = "cm", scale=0.75)

#2) Regress GDP and meat consumption (demand-side) to derive coefficients for scenario projection
pc_cons<-read.csv("meat-consumption-vs-gdp-per-capita.csv")
maddison = readxl::read_excel("Maddison_historical.xlsx")
pc_cons = merge(pc_cons, maddison, by=c("Code", "Year"))
pc_cons = pc_cons[!nchar(as.character(pc_cons$Code)) > 3, ]
pc_cons = pc_cons %>% dplyr::select(Code, Year, Meat.consumption.per.capita..kilograms.per.year., cgdppc)
pc_cons$Code=as.character(pc_cons$Code)
pc_cons_bytipe<-read.csv("per-capita-meat-consumption-by-type-kilograms-per-year.csv")
pc_cons_bytipe = pc_cons_bytipe %>% dplyr::select(-Entity)
pc_cons_bytipe$Code=as.character(pc_cons_bytipe$Code)

pc_cons_bytipe = subset(pc_cons_bytipe, pc_cons_bytipe$Code !="")
pc_cons = subset(pc_cons, pc_cons$Code !="")

y = merge(pc_cons, pc_cons_bytipe, by=c("Code", "Year"))

url = "https://raw.githubusercontent.com/vincentarelbundock/countrycode/master/data/extra/globalburdenofdisease.csv"
state_dict = read.csv(url, stringsAsFactors=FALSE)
y$continent=countrycode(y$Code, 'iso3c', 'gbd_region', custom_dict=state_dict, origin_regex=TRUE)
y$macrocontinent=countrycode(y$Code, 'iso3c', 'continent')

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

#ggsave("histev.pdf", plot = histev, device = "pdf", width = 18, height = 12, units = "cm", scale=0.9)

####
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

#Specify country to regress on

region_regression="CHN"

y = y %>% group_by(Year) %>% mutate(globcons_Pigmeat..kg.=mean(Pigmeat..kg., na.rm=TRUE)) %>% ungroup()
y = y %>% group_by(Year) %>% mutate(globcons_Poultry..kg.=mean(Poultry..kg., na.rm=TRUE)) %>% ungroup()
y = y %>% group_by(Year) %>% mutate(globcons_Mutton...goat..kg.=mean(Mutton...goat..kg., na.rm=TRUE)) %>% ungroup()
y = y %>% group_by(Year) %>% mutate(globcons_Beef.and.buffalo..kg.=mean(Beef.and.buffalo..kg., na.rm=TRUE)) %>% ungroup()

y = y %>% group_by(Year) %>% mutate(globcons_Beef.and.buffalo..kg._5= globcons_Beef.and.buffalo..kg. - shift(globcons_Beef.and.buffalo..kg., -5)) %>% ungroup()


global = subset(y, y$Code == region_regression)

global$Pigmeat..kg._1 <- shift(global$Pigmeat..kg., -5)
global$Pigmeat..kg._delta = global$Pigmeat..kg. - global$Pigmeat..kg._1

global$Poultry..kg._1 <- shift(global$Poultry..kg., -5)
global$Poultry..kg._delta = global$Poultry..kg. - global$Poultry..kg._1

global$Mutton...goat..kg._1 <- shift(global$Mutton...goat..kg., -5)
global$Mutton...goat..kg._delta = global$Mutton...goat..kg. - global$Mutton...goat..kg._1

global$Beef.and.buffalo..kg._1 <- shift(global$Beef.and.buffalo..kg., -5)
global$Beef.and.buffalo..kg._delta = global$Beef.and.buffalo..kg. - global$Beef.and.buffalo..kg._1

#Beef 
formula1 = Beef.and.buffalo..kg. ~ cgdppc + I(cgdppc^2) + Pigmeat..kg._delta + Mutton...goat..kg._delta + Poultry..kg._delta + globcons_Beef.and.buffalo..kg.
CH_beef = lm(formula1, data = global)
summary(CH_beef)

#Granger test
grangertest(global$cgdppc, global$Beef.and.buffalo..kg., order = 5)
grangertest(global$Beef.and.buffalo..kg., global$cgdppc, order = 5)

#Pig
formula2 = Pigmeat..kg. ~ cgdppc + I(cgdppc^2) + Mutton...goat..kg._delta + Poultry..kg._delta + Beef.and.buffalo..kg._delta + globcons_Pigmeat..kg.
CH_pork = lm(formula2, data = global)
summary(CH_pork)

#Granger test
grangertest(global$cgdppc, global$Pigmeat..kg., order = 5)
grangertest(global$Pigmeat..kg., global$cgdppc, order = 5)

#Poultry
formula3 = Poultry..kg. ~ cgdppc + I(cgdppc^2)  + Mutton...goat..kg._delta + Pigmeat..kg._delta + Beef.and.buffalo..kg._delta + globcons_Poultry..kg.
CH_poultry = lm(formula3, data = global)
summary(CH_poultry)


#Granger test
grangertest(x=global$cgdppc, y=global$Poultry..kg., order = 5)
grangertest(x=global$Poultry..kg., y=global$cgdppc, order = 5)

#Mutton-goat
formula4 = Mutton...goat..kg. ~ cgdppc + I(cgdppc^2) + Poultry..kg._delta + Pigmeat..kg._delta + Beef.and.buffalo..kg._delta + globcons_Mutton...goat..kg.
CH_mutton = lm(formula4, data = global)
summary(CH_mutton)


#Granger test
grangertest(global$cgdppc, global$Mutton...goat..kg., order = 5)
grangertest(global$Mutton...goat..kg., global$cgdppc, order = 5)

formula = list(formula1, formula2, formula3, formula4)
CH_sur = systemfit(formula, method = "SUR", data=global, maxiter=500)
summary(CH_sur)

global$effect_beef = (CH_sur[["coefficients"]][["eq1_cgdppc"]]) + (2 * CH_sur[["coefficients"]][["eq1_I(cgdppc^2)"]] * global$cgdppc)
global$effect_pork = (CH_sur[["coefficients"]][["eq2_cgdppc"]]) + (2 * CH_sur[["coefficients"]][["eq2_I(cgdppc^2)"]] * global$cgdppc)
global$effect_poultry = (CH_sur[["coefficients"]][["eq3_cgdppc"]]) + (2 * CH_sur[["coefficients"]][["eq3_I(cgdppc^2)"]] * global$cgdppc)
global$effect_mutton = (CH_sur[["coefficients"]][["eq4_cgdppc"]]) + (2 * CH_sur[["coefficients"]][["eq4_I(cgdppc^2)"]] * global$cgdppc)

effect_CH = ggplot()+
  theme_grey()+
  scale_colour_discrete(name="Meat type")+
  geom_line(data=global, aes(x=cgdppc, y=effect_beef*1000, colour="Beef & buffalo"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_pork*1000, colour="Pork"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_poultry*1000, colour="Poultry"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_mutton*1000, colour="Goat & mutton"))+
  xlab("PPP real per-capita GDP")+
  ylab("Marginal effect \n on meat consumption (grams/capita)")+
  scale_y_continuous(limits = c(0, 8))+
  ggtitle("China")

#chres <- systemfit(formula, method = "SUR", data=global, maxiter=500)
#texreg(list(chres))

##

region_regression="BRA"

global = subset(y, y$Code == region_regression)

global$Pigmeat..kg._1 <- shift(global$Pigmeat..kg., -5)
global$Pigmeat..kg._delta = global$Pigmeat..kg. - global$Pigmeat..kg._1

global$Poultry..kg._1 <- shift(global$Poultry..kg., -5)
global$Poultry..kg._delta = global$Poultry..kg. - global$Poultry..kg._1

global$Mutton...goat..kg._1 <- shift(global$Mutton...goat..kg., -5)
global$Mutton...goat..kg._delta = global$Mutton...goat..kg. - global$Mutton...goat..kg._1

global$Beef.and.buffalo..kg._1 <- shift(global$Beef.and.buffalo..kg., -5)
global$Beef.and.buffalo..kg._delta = global$Beef.and.buffalo..kg. - global$Beef.and.buffalo..kg._1

grangertest(global$cgdppc, global$Beef.and.buffalo..kg., order = 5)
grangertest(global$Beef.and.buffalo..kg., global$cgdppc, order = 5)

formula = list(formula1, formula2, formula3, formula4)
BR_sur = systemfit(formula, method = "SUR", data=global, maxiter=500)
summary(BR_sur)

global$effect_beef = (BR_sur[["coefficients"]][["eq1_cgdppc"]]) + (2 * BR_sur[["coefficients"]][["eq1_I(cgdppc^2)"]] * global$cgdppc)
global$effect_pork = (BR_sur[["coefficients"]][["eq2_cgdppc"]]) + (2 * BR_sur[["coefficients"]][["eq2_I(cgdppc^2)"]] * global$cgdppc)
global$effect_poultry = (BR_sur[["coefficients"]][["eq3_cgdppc"]]) + (2 * BR_sur[["coefficients"]][["eq3_I(cgdppc^2)"]] * global$cgdppc)
global$effect_mutton = (BR_sur[["coefficients"]][["eq4_cgdppc"]]) + (2 * BR_sur[["coefficients"]][["eq4_I(cgdppc^2)"]] * global$cgdppc)

effect_BR = ggplot()+
  theme_grey()+
  scale_colour_discrete(name="Meat type")+
  geom_line(data=global, aes(x=cgdppc, y=effect_beef*1000, colour="Beef & buffalo"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_pork*1000, colour="Pork"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_poultry*1000, colour="Poultry"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_mutton*1000, colour="Goat & mutton"))+
  xlab("PPP real per-capita GDP")+
  ylab("Marginal effect \n on meat consumption (grams/capita)")+
  scale_y_continuous(limits = c(0, 8))+
  ggtitle("Brazil")


###
region_regression="EGY"

global = subset(y, y$Code == region_regression)

global$Pigmeat..kg._1 <- shift(global$Pigmeat..kg., -5)
global$Pigmeat..kg._delta = global$Pigmeat..kg. - global$Pigmeat..kg._1

global$Poultry..kg._1 <- shift(global$Poultry..kg., -5)
global$Poultry..kg._delta = global$Poultry..kg. - global$Poultry..kg._1

global$Mutton...goat..kg._1 <- shift(global$Mutton...goat..kg., -5)
global$Mutton...goat..kg._delta = global$Mutton...goat..kg. - global$Mutton...goat..kg._1

global$Beef.and.buffalo..kg._1 <- shift(global$Beef.and.buffalo..kg., -5)
global$Beef.and.buffalo..kg._delta = global$Beef.and.buffalo..kg. - global$Beef.and.buffalo..kg._1

grangertest(global$cgdppc, global$Beef.and.buffalo..kg., order = 5)
grangertest(global$Beef.and.buffalo..kg., global$cgdppc, order = 5)

formula = list(formula1, formula2, formula3, formula4)
EG_sur = systemfit(formula, method = "SUR", data=global, maxiter=500)
summary(EG_sur)

global$effect_beef = (EG_sur[["coefficients"]][["eq1_cgdppc"]]) + (2 * EG_sur[["coefficients"]][["eq1_I(cgdppc^2)"]] * global$cgdppc)
global$effect_pork = (EG_sur[["coefficients"]][["eq2_cgdppc"]]) + (2 * EG_sur[["coefficients"]][["eq2_I(cgdppc^2)"]] * global$cgdppc)
global$effect_poultry = (EG_sur[["coefficients"]][["eq3_cgdppc"]]) + (2 * EG_sur[["coefficients"]][["eq3_I(cgdppc^2)"]] * global$cgdppc)
global$effect_mutton = (EG_sur[["coefficients"]][["eq4_cgdppc"]]) + (2 * EG_sur[["coefficients"]][["eq4_I(cgdppc^2)"]] * global$cgdppc)

effect_EG = ggplot()+
  theme_grey()+
  scale_colour_discrete(name="Meat type")+
  geom_line(data=global, aes(x=cgdppc, y=effect_beef*1000, colour="Beef & buffalo"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_pork*1000, colour="Pork"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_poultry*1000, colour="Poultry"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_mutton*1000, colour="Goat & mutton"))+
  xlab("PPP real per-capita GDP")+
  ylab("Marginal effect \n on meat consumption (grams/capita)")+
  scale_y_continuous(limits = c(0, 8))+
  ggtitle("Egypt")

##
region_regression="VNM"

global = subset(y, y$Code == region_regression)

global$Pigmeat..kg._1 <- shift(global$Pigmeat..kg., -5)
global$Pigmeat..kg._delta = global$Pigmeat..kg. - global$Pigmeat..kg._1

global$Poultry..kg._1 <- shift(global$Poultry..kg., -5)
global$Poultry..kg._delta = global$Poultry..kg. - global$Poultry..kg._1

global$Mutton...goat..kg._1 <- shift(global$Mutton...goat..kg., -5)
global$Mutton...goat..kg._delta = global$Mutton...goat..kg. - global$Mutton...goat..kg._1

global$Beef.and.buffalo..kg._1 <- shift(global$Beef.and.buffalo..kg., -5)
global$Beef.and.buffalo..kg._delta = global$Beef.and.buffalo..kg. - global$Beef.and.buffalo..kg._1

grangertest(global$cgdppc, global$Beef.and.buffalo..kg., order = 5)
grangertest(global$Beef.and.buffalo..kg., global$cgdppc, order = 5)

formula = list(formula1, formula2, formula3, formula4)
VN_sur = systemfit(formula, method = "SUR", data=global, maxiter=500)
summary(VN_sur)

global$effect_beef = (VN_sur[["coefficients"]][["eq1_cgdppc"]]) + (2 * VN_sur[["coefficients"]][["eq1_I(cgdppc^2)"]] * global$cgdppc)
global$effect_pork = (VN_sur[["coefficients"]][["eq2_cgdppc"]]) + (2 * VN_sur[["coefficients"]][["eq2_I(cgdppc^2)"]] * global$cgdppc)
global$effect_poultry = (VN_sur[["coefficients"]][["eq3_cgdppc"]]) + (2 * VN_sur[["coefficients"]][["eq3_I(cgdppc^2)"]] * global$cgdppc)
global$effect_mutton = (VN_sur[["coefficients"]][["eq4_cgdppc"]]) + (2 * VN_sur[["coefficients"]][["eq4_I(cgdppc^2)"]] * global$cgdppc)

effect_VN = ggplot()+
  theme_grey()+
  scale_colour_discrete(name="Meat type")+
  geom_line(data=global, aes(x=cgdppc, y=effect_beef*1000, colour="Beef & buffalo"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_pork*1000, colour="Pork"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_poultry*1000, colour="Poultry"))+
  geom_line(data=global, aes(x=cgdppc, y=effect_mutton*1000, colour="Goat & mutton"))+
  xlab("PPP real per-capita GDP")+
  ylab("Marginal effect \n on meat consumption (grams/capita)")+
  scale_y_continuous(limits = c(0, 8))+
  ggtitle("Viet Nam")

effects <-plot_grid(effect_CH, effect_EG, effect_BR, effect_VN, cols = 2, rows = 2)

ggsave("effects.pdf", plot = effects, device = "pdf", width = 50, height = 30, units = "cm", scale=0.6)

###

meat_prod_tonnes<-read.csv("meat-production-tonnes.csv")
meat_prod_tonnes = merge(meat_prod_tonnes, y, by=c("Code", "Year"))
pop = wb(indicator = "SP.POP.TOTL", startdate = 1961, enddate = 2013)
meat_prod_tonnes = merge(meat_prod_tonnes, pop, by.x=c("Code", "Year"), by.y=c("iso3c", "date"))
meat_prod_tonnes$prodpc=meat_prod_tonnes$Livestock.Primary..Meat..Total...Production...tonnes..FAO..2017....tonnes. / meat_prod_tonnes$value
meat_prod_tonnes = subset(meat_prod_tonnes, meat_prod_tonnes$Code =="CHN")

cor(meat_prod_tonnes$prodpc, meat_prod_tonnes$Meat.consumption.per.capita..kilograms.per.year.)

grangertest(meat_prod_tonnes$cgdppc, meat_prod_tonnes$prodpc, order = 5)
grangertest(meat_prod_tonnes$prodpc, meat_prod_tonnes$cgdppc, order = 5)

##

meat_prod_tonnes<-read.csv("meat-production-tonnes.csv")
meat_prod_tonnes = merge(meat_prod_tonnes, y, by=c("Code", "Year"))
pop = wb(indicator = "SP.POP.TOTL", startdate = 1961, enddate = 2013)
meat_prod_tonnes = merge(meat_prod_tonnes, pop, by.x=c("Code", "Year"), by.y=c("iso3c", "date"))
meat_prod_tonnes$prodpc=meat_prod_tonnes$Livestock.Primary..Meat..Total...Production...tonnes..FAO..2017....tonnes. / meat_prod_tonnes$value
meat_prod_tonnes = subset(meat_prod_tonnes, meat_prod_tonnes$Code =="BRA")

cor(meat_prod_tonnes$prodpc, meat_prod_tonnes$Meat.consumption.per.capita..kilograms.per.year.)

grangertest(meat_prod_tonnes$cgdppc, meat_prod_tonnes$prodpc, order = 6)
grangertest(meat_prod_tonnes$prodpc, meat_prod_tonnes$cgdppc, order = 6)


#################
#3) Design scenarios projecting the SSPs
#3.1 import the SSPs
ssps = readxl::read_excel("iamc_db.xlsx")
ssps = ssps %>% group_by(Scenario, Region) %>% summarise_if(is.numeric, funs(sum(.*1000)))
ssps = melt(ssps)

ssps = ssps %>% rename(pcgdp=value)

#
ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario, Region) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = ssps_pop %>%  group_by(Scenario, variable) %>% mutate(share = value / sum(value))

ssps_pop = ssps_pop %>% rename(pop=value)

ssps = merge(ssps, ssps_pop, by=c("Scenario", "Region", "variable"))

ssps = ssps %>% group_by(Scenario, variable) %>% summarise(pcgdp=sum(pcgdp*share))

#3.2 adjust unit of dollars (from 2005 to 2011 PPP per-capita GDP)
adjfactor = wb(indicator = "PA.NUS.PPP", startdate = 2005, enddate = 2011)
gdp = wb(indicator = "NY.GDP.MKTP.KD", startdate = 2005, enddate = 2011)

##
#Calculate adj. factor for SSA weighted by the size of natioanal GDPs
gdp$continent=countrycode(gdp$iso3c, 'iso3c', 'continent')
gdp = gdp[complete.cases(gdp$continent),]
gdp = subset(gdp, gdp$continent == "Africa")
gdp=subset(gdp, iso3c != "ATF" & iso3c != "EGY" & iso3c != "ESH"& iso3c != "ESP" & iso3c != "LBY" & iso3c != "MAR" & iso3c != "MYT" & iso3c != "SYC" & iso3c != "COM" & iso3c != "YEM" & iso3c != "TUN" & iso3c != "DZA" & iso3c != "SHN" & iso3c != "DJI" & iso3c != "STP")
gdp = gdp %>%  group_by(date) %>% mutate(share = value / sum(value)) %>% ungroup()
gdp = gdp %>% group_by(iso3c) %>% summarise(share=mean(share))

####

adjfactor = adjfactor %>% dplyr::select(iso3c, date, value) %>% rename(adj=value)
adjfactor = adjfactor %>% subset(date == 2011 | date == 2005) %>% group_by(iso3c) %>% summarise(adj= adj[1]/adj[2])  
gdp = merge(gdp, adjfactor, by=c("iso3c"))
gdp = as.numeric(gdp %>% summarise(adjf = sum(adj*share)))

ssps = ssps %>% mutate(pcgdp=pcgdp*gdp)


#3.4 Define the intercept terms for each type of meat, which is given by today's average level of consumption
#Drop MENA and South Africa
#Beef
africa = subset(y, y$macrocontinent =="Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP" & Code != "ZAF")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Beef.and.buffalo..kg., na.rm = TRUE))

#Pork
africa = subset(y, y$macrocontinent =="Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP" & Code != "ZAF")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Pigmeat..kg., na.rm = TRUE))

#Poultry
africa = subset(y, y$macrocontinent =="Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP" & Code != "ZAF")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Poultry..kg., na.rm = TRUE))

#Mutton
africa = subset(y, y$macrocontinent =="Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP" & Code != "ZAF")
subset(africa, africa$Year==2013) %>% dplyr::summarise(median=median(Mutton...goat..kg., na.rm = TRUE))

##########
#Design the scenarios
##########

#Load historical population
pop = wb(indicator = "SP.POP.TOTL", startdate = 1961, enddate = 2013, country = "ZG")
pop = pop %>% dplyr::select(date, value)
pop$Scenario="SSP2"
colnames(pop) <- c("variable", "value", "Scenario")
pop$value=(pop$value * 0.94) / 1000000
pop = subset(pop, pop$variable < 2010)
pop = pop[,c(3,1,2)]
pop$variable=as.numeric(pop$variable)
pop <- pop[order(pop$variable),]

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

### BEEF ### 
## SSP2 ##
# Scenario 1: China # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  1.02057e-03*pcgdp  +  -4.34416e-08*pcgdp*pcgdp + 5.15) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Beef.and.buffalo..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2CH_pc_beef=prova

SSP2CH_pc_beef_plot = ggplot(SSP2CH_pc_beef, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n beef/buffalo consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - China-like pathway")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 100))+
  scale_colour_discrete(name="Legend \n")+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2CH_pc_beef_plot.pdf", plot = SSP2CH_pc_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2CH_pc_beef = merge(SSP2CH_pc_beef, ssps_pop, by.x="Year", by.y="variable")
SSP2CH_pc_beef$pop = SSP2CH_pc_beef$value*1000000
SSP2CH_pc_beef$total = SSP2CH_pc_beef$meatcons*SSP2CH_pc_beef$pop

SSP2CH_aggregate_beef_plot = ggplot(SSP2CH_pc_beef, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total beef/buffalo \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - China-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2CH_aggregate_beef_plot.pdf", plot = SSP2CH_aggregate_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

# Scenario 2: Brazil # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  5.88535e-03*pcgdp  +  -2.05959e-07*pcgdp*pcgdp + 5.15) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Beef.and.buffalo..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2BR_pc_beef=prova

SSP2BR_pc_beef_plot = ggplot(SSP2BR_pc_beef, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n beef/buffalo consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 100))+
  scale_colour_discrete(name="Legend \n")+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_pc_beef_plot.pdf", plot = SSP2BR_pc_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2BR_pc_beef = merge(SSP2BR_pc_beef, ssps_pop, by.x="Year", by.y="variable")
SSP2BR_pc_beef$pop = SSP2BR_pc_beef$value*1000000
SSP2BR_pc_beef$total = SSP2BR_pc_beef$meatcons*SSP2BR_pc_beef$pop

SSP2BR_aggregate_beef_plot = ggplot(SSP2BR_pc_beef, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total beef/buffalo \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_aggregate_beef_plot.pdf", plot = SSP2BR_aggregate_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

# Scenario 2: Egypt # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  1.50978e-03*pcgdp  +  -5.71910e-08*pcgdp*pcgdp + 5.15) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Beef.and.buffalo..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2EG_pc_beef=prova

SSP2EG_pc_beef_plot = ggplot(SSP2EG_pc_beef, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n beef/buffalo consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 100))+
  scale_colour_discrete(name="Legend \n")+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2EG_pc_beef_plot.pdf", plot = SSP2EG_pc_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2EG_pc_beef = merge(SSP2EG_pc_beef, ssps_pop, by.x="Year", by.y="variable")
SSP2EG_pc_beef$pop = SSP2EG_pc_beef$value*1000000
SSP2EG_pc_beef$total = SSP2EG_pc_beef$meatcons*SSP2EG_pc_beef$pop

SSP2EG_aggregate_beef_plot = ggplot(SSP2EG_pc_beef, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total beef/buffalo \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2EG_aggregate_beef_plot.pdf", plot = SSP2EG_aggregate_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


##
# Scenario 2: Viet Nam # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  -1.67765e-03*pcgdp  +  4.42647e-07*pcgdp*pcgdp + 5.15) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Beef.and.buffalo..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2VN_pc_beef=prova

SSP2VN_pc_beef_plot = ggplot(SSP2VN_pc_beef, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n beef/buffalo consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 100))+
  scale_colour_discrete(name="Legend \n")+
  theme(plot.title = element_text(size=9))

s = ggplot_build(SSP2VN_pc_beef_plot)

#ggsave("SSP2VN_pc_beef_plot.pdf", plot = SSP2VN_pc_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2VN_pc_beef = merge(SSP2VN_pc_beef, ssps_pop, by.x="Year", by.y="variable")
SSP2VN_pc_beef$pop = SSP2VN_pc_beef$value*1000000
SSP2VN_pc_beef$total = SSP2VN_pc_beef$meatcons*SSP2VN_pc_beef$pop

SSP2VN_aggregate_beef_plot = ggplot(SSP2VN_pc_beef, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total beef/buffalo \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2VN_aggregate_beef_plot.pdf", plot = SSP2VN_aggregate_beef_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

### POULTRY ### 
## SSP2 ##

# Scenario 1: China # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  2.88126e-03*pcgdp  +  -1.30258e-07*pcgdp*pcgdp + 3.14) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Poultry..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2CH_pc_poultry = prova

SSP2CH_pc_poultry_plot = ggplot(SSP2CH_pc_poultry, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n poultry consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - China-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 100))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2CH_pc_poultry_plot.pdf", plot = SSP2CH_pc_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2CH_pc_poultry = merge(SSP2CH_pc_poultry, ssps_pop, by.x="Year", by.y="variable")
SSP2CH_pc_poultry$pop = SSP2CH_pc_poultry$value*1000000
SSP2CH_pc_poultry$total = SSP2CH_pc_poultry$meatcons*SSP2CH_pc_poultry$pop

SSP2CH_aggregate_poultry_plot = ggplot(SSP2CH_pc_poultry, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total poultry \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - China-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2CH_aggregate_poultry_plot.pdf", plot = SSP2CH_aggregate_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


# Scenario 2: Brazil # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  2.18081e-03*pcgdp  + 3.14) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Poultry..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2BR_pc_poultry = prova

SSP2BR_pc_poultry_plot = ggplot(SSP2BR_pc_poultry, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n poultry consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 100))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_pc_poultry_plot.pdf", plot = SSP2BR_pc_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2BR_pc_poultry = merge(SSP2BR_pc_poultry, ssps_pop, by.x="Year", by.y="variable")
SSP2BR_pc_poultry$pop = SSP2BR_pc_poultry$value*1000000
SSP2BR_pc_poultry$total = SSP2BR_pc_poultry$meatcons*SSP2BR_pc_poultry$pop

SSP2BR_aggregate_poultry_plot = ggplot(SSP2BR_pc_poultry, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total poultry \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_aggregate_poultry_plot.pdf", plot = SSP2BR_aggregate_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

# Scenario 1: Egypt # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  -1.57081e-03*pcgdp  + 1.02501e-07*pcgdp*pcgdp +3.14) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Poultry..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2EG_pc_poultry = prova

SSP2EG_pc_poultry_plot = ggplot(SSP2EG_pc_poultry, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n poultry consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(-5, 100))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2EG_pc_poultry_plot.pdf", plot = SSP2EG_pc_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2EG_pc_poultry = merge(SSP2EG_pc_poultry, ssps_pop, by.x="Year", by.y="variable")
SSP2EG_pc_poultry$pop = SSP2EG_pc_poultry$value*1000000
SSP2EG_pc_poultry$total = SSP2EG_pc_poultry$meatcons*SSP2EG_pc_poultry$pop

SSP2EG_aggregate_poultry_plot = ggplot(SSP2EG_pc_poultry, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total poultry \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2EG_aggregate_poultry_plot.pdf", plot = SSP2EG_aggregate_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


##
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  2.79517e-03*pcgdp  + -3.30934e-07*pcgdp*pcgdp +3.14) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Poultry..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2VN_pc_poultry = prova

SSP2VN_pc_poultry_plot = ggplot(SSP2VN_pc_poultry, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n poultry consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 100))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2VN_pc_poultry_plot.pdf", plot = SSP2VN_pc_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2VN_pc_poultry = merge(SSP2VN_pc_poultry, ssps_pop, by.x="Year", by.y="variable")
SSP2VN_pc_poultry$pop = SSP2VN_pc_poultry$value*1000000
SSP2VN_pc_poultry$total = SSP2VN_pc_poultry$meatcons*SSP2VN_pc_poultry$pop

SSP2VN_aggregate_poultry_plot = ggplot(SSP2VN_pc_poultry, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total poultry \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_y_continuous(limits = c(0, 60000))+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2VN_aggregate_poultry_plot.pdf", plot = SSP2VN_aggregate_poultry_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

### PORK ### 
## SSP2 ##
# Scenario 1: China # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  7.98464e-03*pcgdp  + -4.48797e-07*pcgdp*pcgdp +1.62) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Pigmeat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2CH_pc_pork = prova

SSP2CH_pc_pork_plot = ggplot(SSP2CH_pc_pork, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n pork consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - China-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2CH_pc_pork_plot.pdf", plot = SSP2CH_pc_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2CH_pc_pork = merge(SSP2CH_pc_pork, ssps_pop, by.x="Year", by.y="variable")
SSP2CH_pc_pork$pop = SSP2CH_pc_pork$value*1000000
SSP2CH_pc_pork$total = SSP2CH_pc_pork$meatcons*SSP2CH_pc_pork$pop

SSP2CH_aggregate_pork_plot = ggplot(SSP2CH_pc_pork, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total pork \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - China-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2CH_aggregate_pork_plot.pdf", plot = SSP2CH_aggregate_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


# Scenario 2: Brazil # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  2.11823e-03*pcgdp  + -7.03871e-08*pcgdp*pcgdp +1.62) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Pigmeat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2BR_pc_pork = prova

SSP2BR_pc_pork_plot = ggplot(SSP2BR_pc_pork, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n pork consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_pc_pork_plot.pdf", plot = SSP2BR_pc_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2BR_pc_pork = merge(SSP2BR_pc_pork, ssps_pop, by.x="Year", by.y="variable")
SSP2BR_pc_pork$pop = SSP2BR_pc_pork$value*1000000
SSP2BR_pc_pork$total = SSP2BR_pc_pork$meatcons*SSP2BR_pc_pork$pop

SSP2BR_aggregate_pork_plot = ggplot(SSP2BR_pc_pork, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total pork \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_aggregate_pork_plot.pdf", plot = SSP2BR_aggregate_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

# Scenario 1: Egypt # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  1.62) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Pigmeat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)


SSP2EG_pc_pork = prova

SSP2EG_pc_pork_plot = ggplot(SSP2EG_pc_pork, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n pork consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2EG_pc_pork_plot.pdf", plot = SSP2EG_pc_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2EG_pc_pork = merge(SSP2EG_pc_pork, ssps_pop, by.x="Year", by.y="variable")
SSP2EG_pc_pork$pop = SSP2EG_pc_pork$value*1000000
SSP2EG_pc_pork$total = SSP2EG_pc_pork$meatcons*SSP2EG_pc_pork$pop

SSP2EG_aggregate_pork_plot = ggplot(SSP2EG_pc_pork, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total pork \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2EG_aggregate_pork_plot.pdf", plot = SSP2EG_aggregate_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


##
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  1.03485e-02*pcgdp + -8.51746e-07*pcgdp*pcgdp + 1.62) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Pigmeat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2VN_pc_pork = prova

SSP2VN_pc_pork_plot = ggplot(SSP2VN_pc_pork, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n pork consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2VN_pc_pork_plot.pdf", plot = SSP2VN_pc_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2VN_pc_pork = merge(SSP2VN_pc_pork, ssps_pop, by.x="Year", by.y="variable")
SSP2VN_pc_pork$pop = SSP2VN_pc_pork$value*1000000
SSP2VN_pc_pork$total = SSP2VN_pc_pork$meatcons*SSP2VN_pc_pork$pop

SSP2VN_aggregate_pork_plot = ggplot(SSP2VN_pc_pork, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total pork \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2VN_aggregate_pork_plot.pdf", plot = SSP2VN_aggregate_pork_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

### MUTTON ### 
## SSP2 ##
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  7.31639e-04*pcgdp + -3.52518e-08*pcgdp*pcgdp + 1.6) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Mutton...goat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2CH_pc_mutton = prova

SSP2CH_pc_mutton_plot = ggplot(SSP2CH_pc_mutton, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n mutton/goat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - China-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2CH_pc_mutton_plot.pdf", plot = SSP2CH_pc_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2CH_pc_mutton = merge(SSP2CH_pc_mutton, ssps_pop, by.x="Year", by.y="variable")
SSP2CH_pc_mutton$pop = SSP2CH_pc_mutton$value*1000000
SSP2CH_pc_mutton$total = SSP2CH_pc_mutton$meatcons*SSP2CH_pc_mutton$pop

SSP2CH_aggregate_mutton_plot = ggplot(SSP2CH_pc_mutton, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total mutton/goat \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - China-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))


#ggsave("SSP2CH_aggregate_mutton_plot.pdf", plot = SSP2CH_aggregate_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


# Scenario 2: Brazil # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  1.6) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Mutton...goat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2BR_pc_mutton = prova

SSP2BR_pc_mutton_plot = ggplot(SSP2BR_pc_mutton, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n mutton/goat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_pc_mutton_plot.pdf", plot = SSP2BR_pc_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2BR_pc_mutton = merge(SSP2BR_pc_mutton, ssps_pop, by.x="Year", by.y="variable")
SSP2BR_pc_mutton$pop = SSP2BR_pc_mutton$value*1000000
SSP2BR_pc_mutton$total = SSP2BR_pc_mutton$meatcons*SSP2BR_pc_mutton$pop

SSP2BR_aggregate_mutton_plot = ggplot(SSP2BR_pc_mutton, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total mutton/goat \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Brazil-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2BR_aggregate_mutton_plot.pdf", plot = SSP2BR_aggregate_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

#
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons = 1.6) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Mutton...goat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2EG_pc_mutton = prova

SSP2EG_pc_mutton_plot = ggplot(SSP2EG_pc_mutton, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n mutton/goat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2EG_pc_mutton_plot.pdf", plot = SSP2EG_pc_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)

ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2EG_pc_mutton = merge(SSP2EG_pc_mutton, ssps_pop, by.x="Year", by.y="variable")
SSP2EG_pc_mutton$pop = SSP2EG_pc_mutton$value*1000000
SSP2EG_pc_mutton$total = SSP2EG_pc_mutton$meatcons*SSP2EG_pc_mutton$pop

SSP2EG_aggregate_mutton_plot = ggplot(SSP2EG_pc_mutton, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total mutton/goat \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Egypt-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))


#ggsave("SSP2EG_aggregate_mutton_plot.pdf", plot = SSP2EG_aggregate_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


##
# Scenario 2: Viet Nam # 
SSP2 = subset(ssps, ssps$Scenario == "SSP2")
provaccia = SSP2 %>% group_by(variable) %>% mutate(meatcons =  -3.78165e-05*pcgdp + 1.27915e-08*pcgdp*pcgdp + 1.6) %>% ungroup()
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(meatcons=median(Mutton...goat..kg., na.rm = TRUE)) %>% ungroup()
provaccia = provaccia %>% rename(Year=variable)
provaccia = provaccia %>% dplyr::select(meatcons, Year)
provaccia$Year = as.numeric.factor(provaccia$Year)
provaccia = subset(provaccia, provaccia$Year > 2014)
provaccia = provaccia %>% dplyr::select(Year, meatcons)
provaccia$source="SSP"
africa_edit$source="Real"
prova = rbind(provaccia, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)

SSP2VN_pc_mutton = prova

SSP2VN_pc_mutton_plot = ggplot(SSP2VN_pc_mutton, aes(x=Year, y=meatcons))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  ylab("Estimated median per-capita \n mutton/goat consumption in SSA")+
  geom_vline(xintercept = 2015, colour = "red")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_y_continuous(limits = c(0,100))+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2VN_pc_mutton_plot.pdf", plot = SSP2VN_pc_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)
ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop = subset(ssps_pop, ssps_pop$Scenario=="SSP2")
ssps_pop$variable<-as.POSIXct(ssps_pop$variable,format="%Y")
ssps_pop = pad(ssps_pop, interval = "year")
ssps_pop$variable = substr(ssps_pop$variable, 1, 4) 
ssps_pop<- na.interpolation(ssps_pop, option = "linear")
ssps_pop = rbind(pop, ssps_pop)
SSP2VN_pc_mutton = merge(SSP2VN_pc_mutton, ssps_pop, by.x="Year", by.y="variable")
SSP2VN_pc_mutton$pop = SSP2VN_pc_mutton$value*1000000
SSP2VN_pc_mutton$total = SSP2VN_pc_mutton$meatcons*SSP2VN_pc_mutton$pop

SSP2VN_aggregate_mutton_plot = ggplot(SSP2VN_pc_mutton, aes(x=Year, y=total/1000000))+
  theme_light()+
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 5, raw=TRUE),colour="grey", alpha=0.5)+
  geom_point(aes(colour=predicted))+
  geom_vline(xintercept = 2015, colour = "red")+
  geom_point(aes(colour=predicted))+
  ylab("Estimated total mutton/goat \n consumption in SSA (Mt)")+
  ggtitle("SSP2 - Viet Nam-like pathway")+
  scale_colour_discrete(name="Legend \n")+
  scale_x_continuous(limits = c(1961, 2050))+
  scale_y_continuous(limits = c(0, 60000))+
  theme(plot.title = element_text(size=9))

#ggsave("SSP2VN_aggregate_mutton_plot.pdf", plot = SSP2VN_aggregate_mutton_plot, device = "pdf", width = 20, height = 15, units = "cm", scale=0.6)


#############
#4) Create combined plots for selected scenarios
SSP2_beef_pc <- plot_grid(SSP2CH_pc_beef_plot + theme(legend.position="none"), SSP2BR_pc_beef_plot + theme(legend.position="none"),  SSP2EG_pc_beef_plot + theme(legend.position="none"), SSP2VN_pc_beef_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_pc_beef_plot)
SSP2_beef_pc <- plot_grid(SSP2_beef_pc, legend, ncol = 2, rel_widths = c(0.4, .1))

SSP2_beef_agg <- plot_grid(SSP2CH_aggregate_beef_plot + theme(legend.position="none"), SSP2BR_aggregate_beef_plot + theme(legend.position="none"), SSP2EG_aggregate_beef_plot + theme(legend.position="none"),  SSP2VN_aggregate_beef_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_aggregate_beef_plot)
SSP2_beef_agg <- plot_grid(SSP2_beef_agg, legend, ncol = 2, rel_widths = c(0.4, .1))

ggsave("SSP2_beef_pc.pdf", plot = SSP2_beef_pc, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)
ggsave("SSP2_beef_agg.pdf", plot = SSP2_beef_agg, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)

SSP2_poultry_pc <- plot_grid(SSP2CH_pc_poultry_plot + theme(legend.position="none"), SSP2BR_pc_poultry_plot + theme(legend.position="none"), SSP2EG_pc_poultry_plot + theme(legend.position="none"),  SSP2VN_pc_poultry_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_pc_poultry_plot)
SSP2_poultry_pc <- plot_grid(SSP2_poultry_pc, legend, ncol = 2, rel_widths = c(0.4, .1))

SSP2_poultry_agg <- plot_grid(SSP2CH_aggregate_poultry_plot + theme(legend.position="none"), SSP2BR_aggregate_poultry_plot + theme(legend.position="none"), SSP2EG_aggregate_poultry_plot + theme(legend.position="none"),  SSP2VN_aggregate_poultry_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_aggregate_poultry_plot)
SSP2_poultry_agg <- plot_grid(SSP2_poultry_agg, legend, ncol = 2, rel_widths = c(0.4, .1))

ggsave("SSP2_poultry_pc.pdf", plot = SSP2_poultry_pc, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)
ggsave("SSP2_poultry_agg.pdf", plot = SSP2_poultry_agg, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)

SSP2_pork_pc <- plot_grid(SSP2CH_pc_pork_plot + theme(legend.position="none"), SSP2BR_pc_pork_plot + theme(legend.position="none"), SSP2EG_pc_pork_plot + theme(legend.position="none"),  SSP2VN_pc_pork_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_pc_pork_plot)
SSP2_pork_pc <- plot_grid(SSP2_pork_pc, legend, ncol = 2, rel_widths = c(0.4, .1))


SSP2_pork_agg <- plot_grid(SSP2CH_aggregate_pork_plot + theme(legend.position="none"), SSP2BR_aggregate_pork_plot + theme(legend.position="none"), SSP2EG_aggregate_pork_plot + theme(legend.position="none"),  SSP2VN_aggregate_pork_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_aggregate_pork_plot)
SSP2_pork_agg <- plot_grid(SSP2_pork_agg, legend, ncol = 2, rel_widths = c(0.4, .1))

ggsave("SSP2_pork_pc.pdf", plot = SSP2_pork_pc, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)
ggsave("SSP2_pork_agg.pdf", plot = SSP2_pork_agg, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)

SSP2_mutton_pc <- plot_grid(SSP2CH_pc_mutton_plot + theme(legend.position="none"), SSP2BR_pc_mutton_plot + theme(legend.position="none"),  SSP2EG_pc_mutton_plot + theme(legend.position="none"), SSP2VN_pc_mutton_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_pc_mutton_plot)
SSP2_mutton_pc <- plot_grid(SSP2_mutton_pc, legend, ncol = 2, rel_widths = c(0.4, .1))

SSP2_mutton_agg <- plot_grid(SSP2CH_aggregate_mutton_plot + theme(legend.position="none"), SSP2BR_aggregate_mutton_plot + theme(legend.position="none"), SSP2EG_aggregate_mutton_plot + theme(legend.position="none"),  SSP2VN_aggregate_mutton_plot + theme(legend.position="none"), ncol = 2)
legend <- get_legend(SSP2CH_aggregate_mutton_plot)
SSP2_mutton_agg <- plot_grid(SSP2_mutton_agg, legend, ncol = 2, rel_widths = c(0.4, .1))

ggsave("SSP2_mutton_pc.pdf", plot = SSP2_mutton_pc, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)
ggsave("SSP2_mutton_agg.pdf", plot = SSP2_mutton_agg, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)

#5) Export scenarios
SSP2CH_pc_beef$Reference_country="CH"
SSP2CH_pc_beef$Type="beef"
SSP2CH_pc_beef = merge(SSP2CH_pc_beef, data.frame(ggplot_build(SSP2CH_aggregate_beef_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2CH_aggregate_beef_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2CH_aggregate_beef_plot....data.....1.....y......, Year = round.ggplot_build.SSP2CH_aggregate_beef_plot....data.....1.....x....), by="Year")

SSP2BR_pc_beef$Reference_country="BR"
SSP2BR_pc_beef$Type="beef"
SSP2BR_pc_beef = merge(SSP2BR_pc_beef, data.frame(ggplot_build(SSP2BR_aggregate_beef_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2BR_aggregate_beef_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2BR_aggregate_beef_plot....data.....1.....y......, Year = round.ggplot_build.SSP2BR_aggregate_beef_plot....data.....1.....x....), by="Year")

SSP2EG_pc_beef$Reference_country="EG"
SSP2EG_pc_beef$Type="beef"
SSP2EG_pc_beef = merge(SSP2EG_pc_beef, data.frame(ggplot_build(SSP2EG_aggregate_beef_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2EG_aggregate_beef_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2EG_aggregate_beef_plot....data.....1.....y......, Year = round.ggplot_build.SSP2EG_aggregate_beef_plot....data.....1.....x....), by="Year")


SSP2VN_pc_beef$Reference_country="VN"
SSP2VN_pc_beef$Type="beef"
SSP2VN_pc_beef = merge(SSP2VN_pc_beef, data.frame(ggplot_build(SSP2VN_aggregate_beef_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2VN_aggregate_beef_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2VN_aggregate_beef_plot....data.....1.....y......, Year = round.ggplot_build.SSP2VN_aggregate_beef_plot....data.....1.....x....), by="Year")

SSP2CH_pc_pork$Reference_country="CH"
SSP2CH_pc_pork$Type="pork"
SSP2CH_pc_pork = merge(SSP2CH_pc_pork, data.frame(ggplot_build(SSP2CH_aggregate_pork_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2CH_aggregate_pork_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2CH_aggregate_pork_plot....data.....1.....y......, Year = round.ggplot_build.SSP2CH_aggregate_pork_plot....data.....1.....x....), by="Year")

SSP2BR_pc_pork$Reference_country="BR"
SSP2BR_pc_pork$Type="pork"
SSP2BR_pc_pork = merge(SSP2BR_pc_pork, data.frame(ggplot_build(SSP2BR_aggregate_pork_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2BR_aggregate_pork_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2BR_aggregate_pork_plot....data.....1.....y......, Year = round.ggplot_build.SSP2BR_aggregate_pork_plot....data.....1.....x....), by="Year")

SSP2EG_pc_pork$Reference_country="EG"
SSP2EG_pc_pork$Type="pork"
SSP2EG_pc_pork = merge(SSP2EG_pc_pork, data.frame(ggplot_build(SSP2EG_aggregate_pork_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2EG_aggregate_pork_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2EG_aggregate_pork_plot....data.....1.....y......, Year = round.ggplot_build.SSP2EG_aggregate_pork_plot....data.....1.....x....), by="Year")


SSP2VN_pc_pork$Reference_country="VN"
SSP2VN_pc_pork$Type="pork"
SSP2VN_pc_pork = merge(SSP2VN_pc_pork, data.frame(ggplot_build(SSP2VN_aggregate_pork_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2VN_aggregate_pork_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2VN_aggregate_pork_plot....data.....1.....y......, Year = round.ggplot_build.SSP2VN_aggregate_pork_plot....data.....1.....x....), by="Year")


SSP2CH_pc_poultry$Reference_country="CH"
SSP2CH_pc_poultry$Type="poultry"
SSP2CH_pc_poultry = merge(SSP2CH_pc_poultry, data.frame(ggplot_build(SSP2CH_aggregate_poultry_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2CH_aggregate_poultry_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2CH_aggregate_poultry_plot....data.....1.....y......, Year = round.ggplot_build.SSP2CH_aggregate_poultry_plot....data.....1.....x....), by="Year")


SSP2BR_pc_poultry$Reference_country="BR"
SSP2BR_pc_poultry$Type="poultry"
SSP2BR_pc_poultry = merge(SSP2BR_pc_poultry, data.frame(ggplot_build(SSP2BR_aggregate_poultry_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2BR_aggregate_poultry_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2BR_aggregate_poultry_plot....data.....1.....y......, Year = round.ggplot_build.SSP2BR_aggregate_poultry_plot....data.....1.....x....), by="Year")

SSP2EG_pc_poultry$Reference_country="EG"
SSP2EG_pc_poultry$Type="poultry"
SSP2EG_pc_poultry = merge(SSP2EG_pc_poultry, data.frame(ggplot_build(SSP2EG_aggregate_poultry_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2EG_aggregate_poultry_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2EG_aggregate_poultry_plot....data.....1.....y......, Year = round.ggplot_build.SSP2EG_aggregate_poultry_plot....data.....1.....x....), by="Year")

SSP2VN_pc_poultry$Reference_country="VN"
SSP2VN_pc_poultry$Type="poultry"
SSP2VN_pc_poultry = merge(SSP2VN_pc_poultry, data.frame(ggplot_build(SSP2VN_aggregate_poultry_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2VN_aggregate_poultry_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2VN_aggregate_poultry_plot....data.....1.....y......, Year = round.ggplot_build.SSP2VN_aggregate_poultry_plot....data.....1.....x....), by="Year")

SSP2CH_pc_mutton$Reference_country="CH"
SSP2CH_pc_mutton$Type="mutton"
SSP2CH_pc_mutton = merge(SSP2CH_pc_mutton, data.frame(ggplot_build(SSP2CH_aggregate_mutton_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2CH_aggregate_mutton_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2CH_aggregate_mutton_plot....data.....1.....y......, Year = round.ggplot_build.SSP2CH_aggregate_mutton_plot....data.....1.....x....), by="Year")

SSP2BR_pc_mutton$Reference_country="BR"
SSP2BR_pc_mutton$Type="mutton"
SSP2BR_pc_mutton = merge(SSP2BR_pc_mutton, data.frame(ggplot_build(SSP2BR_aggregate_mutton_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2BR_aggregate_mutton_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2BR_aggregate_mutton_plot....data.....1.....y......, Year = round.ggplot_build.SSP2BR_aggregate_mutton_plot....data.....1.....x....), by="Year")


SSP2EG_pc_mutton$Reference_country="EG"
SSP2EG_pc_mutton$Type="mutton"
SSP2EG_pc_mutton = merge(SSP2EG_pc_mutton, data.frame(ggplot_build(SSP2EG_aggregate_mutton_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2EG_aggregate_mutton_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2EG_aggregate_mutton_plot....data.....1.....y......, Year = round.ggplot_build.SSP2EG_aggregate_mutton_plot....data.....1.....x....), by="Year")

SSP2VN_pc_mutton$Reference_country="VN"
SSP2VN_pc_mutton$Type="mutton"
SSP2VN_pc_mutton = merge(SSP2VN_pc_mutton, data.frame(ggplot_build(SSP2VN_aggregate_mutton_plot)[["data"]][[1]][["y"]]*1000000, round(ggplot_build(SSP2VN_aggregate_mutton_plot)[["data"]][[1]][["x"]])) %>% rename(smoothed_cons = ggplot_build.SSP2VN_aggregate_mutton_plot....data.....1.....y......, Year = round.ggplot_build.SSP2VN_aggregate_mutton_plot....data.....1.....x....), by="Year")

output = do.call("rbind", list(SSP2CH_pc_beef, SSP2BR_pc_beef, SSP2EG_pc_beef, SSP2VN_pc_beef,SSP2CH_pc_pork, SSP2BR_pc_pork, SSP2EG_pc_pork, SSP2VN_pc_pork,SSP2CH_pc_poultry, SSP2BR_pc_poultry, SSP2EG_pc_poultry, SSP2VN_pc_poultry, SSP2CH_pc_mutton, SSP2BR_pc_mutton, SSP2EG_pc_mutton, SSP2VN_pc_mutton))

output = output %>% dplyr::select(Year, Reference_country, Scenario, Type, total, smoothed_cons)

output = subset(output, output$Year>2014 & output$Year<2051)

output$smoothed_cons_Mt= output$smoothed_cons / 1000000

write.csv(output, "D:/Dropbox (FEEM)/Meat Africa/Repo/meatSSA/Consumption/output.csv")

total_output_smoothed_SSP2 = ggplot(output)+
  theme_gray()+
  ggtitle('SSP2, meat consumption in sub-Saharan Africa')+
  geom_line(aes(x=Year,y=smoothed_cons/1000000,color=Reference_country), size=1)+
  facet_wrap(~Type, labeller = as_labeller(meat_names))+
  ylab('Total consumption (Mt)')+
  scale_colour_discrete(name="Reference country")

ggsave("SSP2_all.pdf", plot = total_output_smoothed_SSP2, device = "pdf", width = 40, height = 30, units = "cm", scale=0.4)

#ALL SSPs in a unique figure
#plot_grid(total_output_smoothed_SSP1, total_output_smoothed_SSP2, total_output_smoothed_SSP3, total_output_smoothed_SSP4, total_output_smoothed_SSP, nrow = 2, ncol = 2)
