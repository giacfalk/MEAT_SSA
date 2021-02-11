######MEATSSA###########
#A model for projecting meat consumption scenarios
#and related environmental impact in Sub-Saharan Africa#
########################
##Giacomo Falchetta, Michel Noussan
#Any question should be addressed to giacomo.falchetta@feem.it
### Version: 04/01/21 ###

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
setwd("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/Data")

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
  theme_classic()+
  geom_line(size=1)+
  ylab("Total meat consumption \n (kg/capita/year)")+
  labs(color = "Regions\n")+
  scale_color_brewer(palette="Set1")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10))

pc_cons_bytipe<-read.csv("per-capita-meat-consumption-by-type-kilograms-per-year.csv")

pc_cons_bytipe_regional<-subset(pc_cons_bytipe, Entity== "Africa" | Entity== "Asia"| Entity== "Americas"| Entity== "Australia & New Zealand"| Entity== "Europe")
pc_cons_bytipe_regional$Entity=as.character(pc_cons_bytipe_regional$Entity)
pc_cons_bytipe_regional$Entity[pc_cons_bytipe_regional$Entity == "Northern America"] <- "North America"
pc_cons_bytipe_regional$Entity[pc_cons_bytipe_regional$Entity == "Australia & New Zealand"] <- "Oceania"

a<-ggplot(pc_cons_bytipe_regional, aes(x=Year, colour=Entity))+
geom_line(aes(y=Beef.and.buffalo..kg.), size=1) + 
  theme_classic()+
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
  scale_fill_brewer(name="Meat Type", labels=c("Sheep and mutton", "Beef and buffalo", "Pigmeat", "Poultry"), palette = "Set1")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=8, angle = 45), axis.text.y = element_text(size=10))+theme(legend.direction="horizontal")

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
  scale_fill_brewer(name="Meat Type", labels=c("Mutton and goat", "Beef and buffalo", "Pigmeat", "Poultry"), palette = "Set1")+
  theme(text = element_text(size = 10), axis.text.x = element_text(size=8, angle = 45), axis.text.y = element_text(size=10))

combo_type<-plot_grid(a+theme(legend.position="none"), (d+theme(legend.position="none")))
legend <- get_legend(a)
fig1b <- plot_grid(combo_type, legend, ncol = 1, rel_heights = c(1, .1))

fig1 <- plot_grid(fig1a, fig1b, labels = "AUTO", ncol = 1)

#ggsave("figure1.png", plot = fig1, device = "png", width = 26, height = 30, units = "cm", scale=0.5)

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

##ggsave("figureghg.pdf", plot = GHG_plot, device = "pdf", width = 25, height = 18, units = "cm", scale=0.75)

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
  
##ggsave("figureluc.pdf", plot = landusechangeplot, device = "pdf", width = 25, height = 18, units = "cm", scale=0.75)

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

url = "https://raw.githubusercontent.com/vincentarelbundock/countrycode/a9f6c31fb52f35838af261c7ca4f1aca46b907d8/data/extra/globalburdenofdisease.csv"
state_dict = read.csv(url, stringsAsFactors=FALSE)
y$continent=countrycode(y$Code, 'iso3c', 'gbd_region', custom_dict=state_dict, origin_regex=TRUE)
y$macrocontinent=countrycode(y$Code, 'iso3c', 'continent')

#Produce figure of quadratic trends for all types of meat
require(scales)
histev<-ggplot(y, aes(x=cgdppc, y=Meat.consumption.per.capita..kilograms.per.year., colour=macrocontinent)) + 
  geom_point(size=1, alpha=0.3)+
  theme_classic()+
  scale_x_continuous(labels = comma)+
  scale_color_brewer(name="Region", palette="Set1")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  ylab("Total meat consumption: kg/capita/year")+
  xlab("PPP per-capita GDP")

#ggsave("histev.png", plot = histev, device = "png", width = 18, height = 12, units = "cm", scale=0.8)

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

################
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
adjfactor = wb(indicator = "NY.GDP.DEFL.ZS", startdate = 2005, enddate = 2011, country = "USA")
gdp = wb(indicator = "NY.GDP.MKTP.KD", startdate = 2005, enddate = 2011)

##
#Calculate adj. factor for SSA weighted by the size of natioanal GDPs
gdp$continent=countrycode(gdp$iso3c, 'iso3c', 'continent')
gdp = gdp[complete.cases(gdp$continent),]
gdp = subset(gdp, gdp$continent == "Africa")
gdp=subset(gdp, iso3c != "ATF" & iso3c != "EGY" & iso3c != "ESH"& iso3c != "ESP" & iso3c != "LBY" & iso3c != "MAR" & iso3c != "MYT" & iso3c != "SYC" & iso3c != "COM" & iso3c != "YEM" & iso3c != "TUN" & iso3c != "DZA" & iso3c != "SHN" & iso3c != "DJI" & iso3c != "STP")
gdp = gdp %>%  group_by(date) %>% mutate(share = value / sum(value)) %>% ungroup()
gdp = gdp %>% group_by(iso3c) %>% dplyr::summarise(share=mean(share))

####

adjfactor = adjfactor %>% dplyr::select(iso3c, date, value) %>% rename(adj=value)
adjfactor = adjfactor %>% subset(date == 2011 | date == 2005) %>% group_by(iso3c) %>% summarise(adj= adj[1]/adj[2])  
gdp = gdp %>% dplyr::summarise(adjf = sum(adjfactor$adj*share, na.rm = TRUE)) 
gdp = gdp[1,1]
ssps = ssps %>% mutate(pcgdp=pcgdp*gdp$adjf)

## add future religion

religion_future <- read.csv("Religious_Composition_by_Country_2010-2050.csv", stringsAsFactors = F)

religion_future$ï..Year <-  as.POSIXct(as.character(religion_future$ï..Year), format="%Y")

library(padr)
religion_future<-(padr::pad(religion_future, interval="5 years"))

religion_future <- religion_future %>%
  arrange(ï..Year) %>%
  mutate(christians_share = na.approx(christians_share), muslims_share = na.approx(muslims_share), nonrelig_share = na.approx(nonrelig_share), hindus_share = na.approx(hindus_share), buddists_share = na.approx(buddists_share), folk_share = na.approx(folk_share), other_share = na.approx(other_share), jews_share = na.approx(jews_share))

religion_future$ï..Year <- lubridate::year(religion_future$ï..Year)

ssps = merge(ssps, religion_future, by.y="ï..Year", by.x="variable", all=TRUE)

colnames(ssps)[3] <- "cgdppc"

ssps <- filter(ssps, as.numeric(as.character(variable))<=2050)
ssps$Region=NULL

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


##########
## calculate effect controlling for gdp and religion and clustering by country
r <- read.csv("WRP national data.csv")

r$christians_share = r$chrstangpct+r$chrstcatpct+r$chrstgenpct+r$chrstorthpct+r$chrstothrpct+r$chrstprotpct

r$jews_share = r$judconspct+r$judgenpct+r$judorthpct+r$judothrpct+r$judrefpct

r$muslims_share = r$islmahmpct+r$islmalwpct+r$islmgenpct + r$islmibdpct + r$islmnatpct + r$islmothrpct + r$islmshipct + r$islmsunpct

r$nonrelig_share = r$nonreligpct

r$hindus_share = r$hindgenpct

r$buddists_share = r$budgenpct + r$budmahpct + r$budothrpct + r$budthrpct

r$folk_share = r$anmgenpct

r$other_share = r$zorogenpct + r$sikhgenpct + r$shntgenpct + r$bahgenpct + r$taogenpct + r$jaingenpct + r$confgenpct + r$syncgenpct

#
r$year <-  as.POSIXct(as.character(r$year), format="%Y")
r<-(padr::pad(r, interval="year", group = "name"))

r <- r %>%
  arrange(year) %>%
  group_by(name) %>% 
  mutate(christians_share = na.approx(christians_share), muslims_share = na.approx(muslims_share), nonrelig_share = na.approx(nonrelig_share), hindus_share = na.approx(hindus_share), buddists_share = na.approx(buddists_share), folk_share = na.approx(folk_share), other_share = na.approx(other_share), jews_share = na.approx(jews_share))

r$year<- lubridate::year(r$year)

y <- merge(y, r, by.x=c("Code", "Year"), by.y=c("name", "year"), all.x=T)

# add urbanisation

urbanisation <- readxl::read_xls("WUP2018-F02-Proportion_Urban.xls")

urbanisation <- reshape2::melt(urbanisation, 1, 2:22)
urbanisation$Code <- countrycode::countrycode(urbanisation$country, 'country.name', 'iso3c')

colnames(urbanisation) <- c("Country", "Year", "urbanisation", "Code")

urbanisation$Year <- as.numeric(as.character(urbanisation$Year))

urbanisation$Year <- as.POSIXct(as.character(urbanisation$Year), format="%Y")

library(padr)
urbanisation<-(padr::pad(urbanisation, interval="year", group = "Code"))

urbanisation <- urbanisation %>%
  group_by(Code) %>%
  arrange(Year) %>%
  mutate(urbanisation = na.approx(urbanisation))

urbanisation$Year <- lubridate::year(urbanisation$Year)


# 
urbssa <- readxl::read_xls("WUP2018-F02-Proportion_Urban.xls")
urbssa <- subset(urbssa, urbssa$country=="Sub-Saharan Africa")
urbssa <- reshape2::melt(urbssa, 1, 2:22) %>% dplyr::select(-country)
colnames(urbssa) <- c("variable", "urbanisation")

ssps = merge(ssps, urbssa, by.y="variable", by.x="variable", all.x=T)

y <- merge(y, urbanisation, by.x=c("Code", "Year"), by.y=c("Code", "Year"), all.x=TRUE)

## 

# meat prices historical

old_prices <- read.csv("prices/old_prices.csv", stringsAsFactors = F)
new_prices <- read.csv("prices/new_prices.csv", stringsAsFactors = F)

old_prices <- dplyr::select(old_prices, Country, Item, Year, Value)
new_prices <- dplyr::select(new_prices, Area, Item, Year, Value)

colnames(new_prices) <- colnames(old_prices)

prices <- rbind(old_prices, new_prices)

prices$region <- countrycode::countrycode(prices$Country, 'country.name', 'iso3c')

adjfactor = wb(indicator = "PA.NUS.ATLS", startdate = 1966, enddate = 2018)

prices <- merge(prices, adjfactor, by.x=c("region", "Year"), by.y=c("iso3c", "date"), all.x=T)

prices$deflated = prices$Value/prices$value

prices <- prices %>% dplyr::select(region, Item, Year, deflated)

prices$continent=countrycode::countrycode(prices$region, 'iso3c', 'gbd_region', custom_dict=state_dict, origin_regex=TRUE)

prices <- prices %>% 
  dplyr::group_by(continent, Item, Year) %>%
  summarise(deflated=median(deflated, na.rm = T))

prices <- prices %>%
  dplyr::group_by(continent, Item) %>%
  dplyr::mutate(deflated = (deflated/deflated[Year==2010])*100) 

prices <- spread(prices, key = 2, value = 4)

library(zoo)

prices <- prices[order(prices$continent, prices$Year),]

prices <- prices %>%
  group_by(continent) %>%
  mutate(`Meat live weight, cattle` = na.approx(`Meat live weight, cattle`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, chicken` = na.approx(`Meat live weight, chicken`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, goat` = na.approx(`Meat live weight, goat`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, pig` = na.approx(`Meat live weight, pig`, na.rm=FALSE, maxgap = Inf, rule = 2)) %>% 
  filter(!is.na(continent)) 

y <- merge(y, prices, by.x=c("continent", "Year"), by.y=c("continent", "Year"), all.x=TRUE)

# agricultural producer price index as a control variable

# past

old_prices <- read.csv("prices/PricesArchive_E_All_Data.csv", stringsAsFactors = F)
new_prices <- read.csv("prices/Prices_E_All_Data.csv", stringsAsFactors = F)

old_prices <- dplyr::select(old_prices, Country, Item, Year, Value)

new_prices <- reshape2::melt(new_prices, c(1:9), c(10:67))

new_prices <- filter(new_prices, Months == "Annual value" & Element=="Producer Price Index (2014-2016 = 100)")

new_prices <- dplyr::select(new_prices, Area, Item, variable, value)

colnames(new_prices) <- colnames(old_prices)

new_prices$Year <-as.character(new_prices$Year)

new_prices$Year <- gsub("Y", "", new_prices$Year)

prices <- rbind(old_prices, new_prices)

prices$region <- countrycode::countrycode(prices$Country, 'country.name', 'iso3c')

adjfactor = wb(indicator = "PA.NUS.ATLS", startdate = 1966, enddate = 2019)

prices$iso <- countrycode::countrycode(prices$Country, "country.name", "iso3c")

prices$Year <- as.numeric(prices$Year)

prices <- merge(prices, adjfactor, by.x=c("iso", "Year"), by.y=c("iso3c", "date"), all.x=T)

prices$deflated = as.numeric(prices$Value)/prices$value

prices_crops <- filter(prices, Item=="Cereals, Total" | Item=="Cereals, nes")

prices_crops$continent=countrycode::countrycode(prices_crops$region, 'iso3c', 'gbd_region', custom_dict=state_dict, origin_regex=TRUE)

prices <- prices_crops %>% 
  dplyr::group_by(continent, Item, Year) %>%
  summarise(deflated=median(deflated, na.rm = T))

prices <- prices[order(prices$continent, prices$Year),]

prices <- prices %>%
  group_by(continent) %>%
  mutate(deflated = na.approx(deflated, na.rm=FALSE, maxgap = Inf, rule = 2))  %>% 
  filter(!is.na(continent) & !is.na(Year) )

prices <- prices %>%
  dplyr::group_by(continent) %>%
  dplyr::mutate(deflated = (deflated/deflated[Year==2010])*100) 

y <- merge(y, prices, by.x=c("continent", "Year"), by.y=c("continent", "Year"), all.x=TRUE)

# also add future prices for prediction
# meat prices future

prices_fut <- read.csv("prices/FOFA2050CountryData_Market.csv", stringsAsFactors = F)

prices_fut <- prices_fut %>% dplyr::select(Indicator, Item, CountryCode, Scenario, Year, Value) %>% group_by(Year, CountryCode, Item, Scenario) %>% dplyr::summarise(Value[2]/Value[1])

prices_fut <- filter(prices_fut, CountryCode=="XSSA" & (Item=="Pigmeat" | Item=="Poultry meat" | Item=="Sheep and goat meat" | Item=="Beef and veal"))

prices_fut <- prices_fut %>% 
  group_by(Item, Scenario) %>%
  mutate(Value = (`Value[2]/Value[1]`/`Value[2]/Value[1]`[Year==2012])*100)

prices_fut <- prices_fut %>%
  group_by(Item, Year) %>%
  summarise(Value = mean(Value, na.rm=T))

prices_fut <- spread(prices_fut, key = 1, value = 3)
colnames(prices_fut) <- c("Year", "Meat live weight, cattle", "Meat live weight, pig", "Meat live weight, chicken", "Meat live weight, goat")

prices_fut$Year <-  as.POSIXct(as.character(prices_fut$Year ), format="%Y")

prices_fut<-(padr::pad(prices_fut, interval="1 year"))

prices_fut <- prices_fut %>%
  mutate(`Meat live weight, cattle` = na.approx(`Meat live weight, cattle`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, chicken` = na.approx(`Meat live weight, chicken`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, goat` = na.approx(`Meat live weight, goat`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, pig` = na.approx(`Meat live weight, pig`, na.rm=FALSE, maxgap = Inf, rule = 2))

prices_fut$Year  <- lubridate::year(prices_fut$Year )

ssps = merge(ssps, prices_fut, by.y="Year", by.x="variable", all.x=T)

ssps <- ssps %>%
  group_by(Scenario) %>%
  mutate(`Meat live weight, cattle` = na.approx(`Meat live weight, cattle`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, chicken` = na.approx(`Meat live weight, chicken`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, goat` = na.approx(`Meat live weight, goat`, na.rm=FALSE, maxgap = Inf, rule = 2), `Meat live weight, pig` = na.approx(`Meat live weight, pig`, na.rm=FALSE, maxgap = Inf, rule = 2)) 

# 

# library(tesseract)
# eng <- tesseract("eng")
# text <- tesseract::ocr("C:/Users/GIACOMO/Downloads/screencapture-app-powerbi-view-2020-12-14-15_15_41.pdf")
# 
# text2 <- strsplit(text, split = "\n")
# 
# text2 <- as.vector(do.call(rbind, text2))
# 
# write.csv(text2, "text.csv")

future_price_index <- readxl::read_xlsx("prices/futurepriceindex.xlsx")

future_price_index$`1` <- gsub("[^0-9.-]", "", future_price_index$`1`)

future_price_index$`1` <- as.numeric(future_price_index$`1` )
future_price_index$`1` <- ifelse(future_price_index$`1` >100, future_price_index$`1` /100, future_price_index$`1` )
future_price_index <- group_by(future_price_index, Armenia, `2012`) %>% summarise(`1`=mean(`1`, na.rm=T))

future_price_index$continent <- countrycode::countrycode(future_price_index$Armenia, 'country.name', 'continent')

future_price_index <- dplyr::group_by(future_price_index, continent, `2012`) %>% summarise(deflated=mean(`1`, na.rm=T))

future_price_index$variable = as.integer(future_price_index$`2012`)

future_price_index <- filter(future_price_index, continent=="Africa")

future_price_index$`2012` <-  as.POSIXct(as.character(future_price_index$`2012` ), format="%Y")

future_price_index<-(padr::pad(future_price_index, interval="1 year"))

future_price_index <- future_price_index %>%
  mutate(deflated = na.approx(deflated, na.rm=FALSE, maxgap = Inf, rule = 2))

future_price_index$variable  <- lubridate::year(future_price_index$`2012`)

future_price_index <- dplyr::select(future_price_index, variable, deflated)

future_price_index$continent=NULL

ssps = merge(ssps, future_price_index, by="variable", all.x=T)

######

y2 = y %>% dplyr::select(cgdppc, Beef.and.buffalo..kg., christians_share, jews_share, muslims_share, nonrelig_share, hindus_share, buddists_share, folk_share, other_share, Pigmeat..kg., Poultry..kg., Mutton...goat..kg., continent, urbanisation, colnames(y)[103:106], deflated, continent, Year) %>% as.data.frame()

# if all obs of that group are NA, then replace with global mean for that year

# for (i in unique(y2$Year)){
# y2 <- y2 %>%
#   group_by(continent) %>%
#   mutate(`Meat live weight, pig` := ifelse(sum(is.na(`Meat live weight, pig`))==n() & Year==i, mean(y2$`Meat live weight, pig`[y2$Year==i], na.rm=T), `Meat live weight, pig`)) %>% ungroup()
# }
# 

y2 <- y2 %>%
  group_by(Year) %>% 
  mutate_at(vars(-group_cols()), function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

y2 = y2[complete.cases(y2), ]

legend_continent <- unique(y2$continent)

y2 = y2 %>% group_by(continent) %>% mutate(random=runif(n()))

y2 = y2 %>% mutate(group=as.factor(ifelse(random < 0.7, 'TRAINING', 'TESTING')))

y2$continent = as.factor(y2$continent)
y2$Year=NULL

# Partition data
train.hex <- filter(y2, group=="TRAINING")
test.hex <- filter(y2, group=="TESTING")

train.hex$group=NULL
test.hex$group=NULL
train.hex$random=NULL
test.hex$random=NULL

train.hex <- as.data.frame(train.hex)
test.hex <- as.data.frame(test.hex)

library(randomForestSRC)
pr = rfsrc(Multivar(Beef.and.buffalo..kg.,Pigmeat..kg.,Poultry..kg., Mutton...goat..kg.)~.,data = train.hex, importance=T)

randomForestSRC::print.rfsrc(pr, outcome.target = "Beef.and.buffalo..kg.")
randomForestSRC::print.rfsrc(pr, outcome.target = "Pigmeat..kg.")
randomForestSRC::print.rfsrc(pr, outcome.target = "Poultry..kg.")
randomForestSRC::print.rfsrc(pr, outcome.target = "Mutton...goat..kg.")

#
png("beef_stats.png", width = 750, height = 480, units = "px", bg = "white")
plot(pr, m.target="Beef.and.buffalo..kg.")
dev.off()

png("pig_stats.png", width = 750, height = 480, units = "px", bg = "white")
plot(pr, m.target="Pigmeat..kg.")
dev.off()

png("poultry_stats.png", width = 750, height = 480, units = "px", bg = "white")
plot(pr, m.target="Poultry..kg.")
dev.off()

png("mutton_stats.png", width = 750, height = 480, units = "px", bg = "white")
plot(pr, m.target="Mutton...goat..kg.")
dev.off()

#


# plot elasticities
source("D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/method_ela.R")

meattypes <- c("Beef.and.buffalo..kg.", "Poultry..kg.", "Pigmeat..kg.", "Mutton...goat..kg.")

pp <- plot.variable.rfsrc(pr, xvar.names=c("cgdppc"), m.target=meattypes[1], partial = F, oob=T, sorted = TRUE)
method_ela("cgdppc", 100000, "PPP per-capita GDP (2011 USD)", "Estimated global income \nelasticity of beef demand", 75000, paste0("el1", sub("\\..*", "", meattypes[1]),  ".png"), sub("\\..*", "", meattypes[1]))

pp <- plot.variable.rfsrc(pr, xvar.names=c("cgdppc"), m.target=meattypes[2], partial = F, oob=T, sorted = TRUE)
method_ela("cgdppc", 100000, "PPP per-capita GDP (2011 USD)", "Estimated global income \nelasticity of poultry demand", 75000, paste0("el1", sub("\\..*", "", meattypes[2]),  ".png"), sub("\\..*", "", meattypes[2]))

pp <- plot.variable.rfsrc(pr, xvar.names=c("cgdppc"), m.target=meattypes[3], partial = F, oob=T, sorted = TRUE)
method_ela("cgdppc", 100000, "PPP per-capita GDP (2011 USD)", "Estimated global income \nelasticity of pork demand", 75000, paste0("el1", sub("\\..*", "", meattypes[3]),  ".png"), sub("\\..*", "", meattypes[3]))

pp <- plot.variable.rfsrc(pr, xvar.names=c("cgdppc"), m.target=meattypes[4], partial = F, oob=T, sorted = TRUE)
method_ela("cgdppc", 100000, "PPP per-capita GDP (2011 USD)", "Estimated global income \nelasticity of mutton demand", 75000, paste0("el1", sub("\\..*", "", meattypes[4]),  ".png"), sub("\\..*", "", meattypes[4]))

# Test model accuracy
prediction <- predict.rfsrc(pr, test.hex)

test.hex$Beef.and.buffalo..kg._forecasted = prediction$regrOutput$Beef.and.buffalo..kg.$predicted
test.hex$Pigmeat..kg._forecasted = prediction$regrOutput$Pigmeat..kg.$predicted
test.hex$Poultry..kg._forecasted = prediction$regrOutput$Poultry..kg.$predicted
test.hex$Mutton...goat..kg._forecasted = prediction$regrOutput$Mutton...goat..kg.$predicted

library(outreg)

# R2 for test (= test accuracy)
formula<-"Beef.and.buffalo..kg. ~ Beef.and.buffalo..kg._forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE)  

ols1 <- outreg(ols1, starred = 'se', pv = TRUE, tv = TRUE, se = FALSE)
write.csv(ols1, "ols1.csv")

formula<-"Pigmeat..kg. ~ Pigmeat..kg._forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE)  

ols1 <- outreg(ols1, starred = 'se', pv = TRUE, tv = TRUE, se = FALSE)
write.csv(ols1, "ols2.csv")

formula<-"Poultry..kg. ~ Poultry..kg._forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE) 

ols1 <- outreg(ols1, starred = 'se', pv = TRUE, tv = TRUE, se = FALSE)
write.csv(ols1, "ols3.csv")

formula<-"Mutton...goat..kg. ~ Mutton...goat..kg._forecasted"
ols1<-lm(formula,data=test.hex)
summary(ols1, robust=TRUE) 

ols1 <- outreg(ols1, starred = 'se', pv = TRUE, tv = TRUE, se = FALSE)
write.csv(ols1, "ols4.csv")

#############

# let's choose what regions to include
# 1 central europe
# 11 eastern asia
# 12 central Latin America
# 15 nafrica and mean

SSPS_names =c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")

funzione = function(X){
  ssp_store = list()
  for (i in SSPS_names){
  ssp2 = subset(ssps, ssps$Scenario==i)
  ssp2 = ssp2[complete.cases(ssp2), ]
  ssp2$continent = as.factor(X)
  prediction <- predict.rfsrc(pr, ssp2)
  ssp2$beef = prediction$regrOutput$Beef.and.buffalo..kg.$predicted
  ssp2$poultry = prediction$regrOutput$Poultry..kg.$predicted
  ssp2$mutton = prediction$regrOutput$Mutton...goat..kg.$predicted
  ssp2$pork = prediction$regrOutput$Pigmeat..kg.$predicted
  ssp2$Scenario_region = ssp2$continent[1]
  ssp2$Scenario = i
  ssp_store[[i]] = ssp2
  } 
  ssp_store
}

lista = lapply(c("Central Europe", "East Asia", "Central Latin America", "North Africa and Middle East"), funzione)

lista[[1]] = bind_rows(lista[[1]])
lista[[2]] = bind_rows(lista[[2]])
lista[[3]] = bind_rows(lista[[3]])
lista[[4]] = bind_rows(lista[[4]])
lista = bind_rows(lista)


lista = subset(lista, as.numeric(as.character(lista$variable))>2010)

###
lista = lista %>% rename(Year=variable)
lista = lista %>% dplyr::select(beef, mutton, poultry, pork, Year, Scenario_region, Scenario)
lista$Year = as.numeric.factor(lista$Year)
lista$source=lista$Scenario
africa = subset(y, y$macrocontinent =="Africa")
africa=subset(africa, Code != "ATF" & Code != "EGY" & Code != "ESH"& Code != "ESP" & Code != "LBY" & Code != "MAR" & Code != "MYT" & Code != "SYC" & Code != "COM" & Code != "YEM" & Code != "TUN" & Code != "DZA" & Code != "SHN" & Code != "DJI" & Code != "STP" & Code != "ZAF")
africa_edit = africa %>% dplyr::group_by(Year) %>% dplyr::summarise(beef=median(Beef.and.buffalo..kg., na.rm = TRUE), poultry=median(Poultry..kg., na.rm = TRUE), mutton=median(Mutton...goat..kg., na.rm = TRUE), pork=median(Pigmeat..kg., na.rm = TRUE)) %>% ungroup()
africa_edit$source="Real"
africa_edit$Scenario_region="SSA - historical"
lista$Scenario = NULL
prova = rbind(lista, africa_edit)
prova = prova[with(prova, order(Year)), ]
prova = subset(prova, prova$Year > 1961)
prova$predicted=ifelse(prova$source == "Real", "Historical", "Projected")
prova = na.omit(prova)


ssps_pop = readxl::read_excel("iamc_db_population_SSA.xlsx")
ssps_pop = ssps_pop %>% group_by(Scenario) %>% summarise_if(is.numeric, funs(sum(.)))
ssps_pop = melt(ssps_pop)
ssps_pop$variable = as.numeric.factor(ssps_pop$variable)
  
prova = merge(prova, ssps_pop, by.x=c("Year", "source"), by.y=c("variable", "Scenario"), all=TRUE)
prova$pop = prova$value*1000000

#
prova$beef_kg_tot = prova$beef*prova$pop
prova$poultry_kg_tot = prova$poultry*prova$pop
prova$mutton_kg_tot = prova$mutton*prova$pop
prova$pork_kg_tot = prova$pork*prova$pop

# Export version aggregate
prova2 = gather(prova, key="Type", value="Total_kg", beef_kg_tot, poultry_kg_tot, mutton_kg_tot, pork_kg_tot)
prova2 = prova2[complete.cases(prova2),]
prova2 = dplyr::select(prova2, Year, source, Scenario_region, pop, Type, Total_kg)
prova2$total_Kt= prova2$Total_kg / 1000000

colnames(prova2)[2] <- "SSP_Scenario"
colnames(prova2)[4] <- "Population"

prova2$Type = gsub('_kg', '', prova2$Type )

write.csv(prova2, "all_projections_2050_SSAfrica.csv", row.names = FALSE)

# Export version per-capita
prova3 = gather(prova, key="Type", value="Total_kg", beef, poultry, mutton, pork)
prova3 = prova3[complete.cases(prova3),]
prova3 = dplyr::select(prova3, Year, source, Scenario_region, pop, Type, Total_kg)
colnames(prova3)[2] <- "SSP_Scenario"
colnames(prova3)[4] <- "Population"

prova3 <- prova3 %>% group_by(Year, Type, SSP_Scenario) %>% dplyr::summarise(Total_kg=mean(Total_kg))

prova3$Type[prova3$Type=="beef"] <- "Beef and veal"
prova3$Type[prova3$Type=="mutton"] <- "Sheep and goat meat"
prova3$Type[prova3$Type=="poultry"] <- "Poultry meat"
prova3$Type[prova3$Type=="pork"] <- "Pigmeat"

per_capita = ggplot()+
  theme_gray()+
  ggtitle('Projected per-capita meat pathways')+
  geom_line(data= filter(prova3, Year>=2020), aes(x=Year,y=Total_kg,color=SSP_Scenario, group= SSP_Scenario), size=1, alpha=1)+
  facet_wrap(~Type)+
  ylab('Per-capita consumption (kg)')+
  scale_colour_brewer(name="SSP (ref. regions scenarios mean)", palette = "Set1")+
  scale_x_continuous(limits=c(2020, 2050))+
  theme(legend.position = "bottom", legend.direction = "horizontal")


prova2 <- prova2 %>% group_by(Year, Type, SSP_Scenario) %>% dplyr::summarise(total_Kt=mean(total_Kt))

prova2$Type[prova2$Type=="beef_tot"] <- "Beef and veal"
prova2$Type[prova2$Type=="mutton_tot"] <- "Sheep and goat meat"
prova2$Type[prova2$Type=="poultry_tot"] <- "Poultry meat"
prova2$Type[prova2$Type=="pork_tot"] <- "Pigmeat"

total = ggplot()+
  ggtitle('Projected total meat consumption pathways')+
  theme_gray()+
  geom_line(data= filter(prova2, Year>=2020), aes(x=Year,y=total_Kt,color=SSP_Scenario, group= SSP_Scenario), size=1, alpha=1)+
  facet_wrap(~Type)+
  ylab('Regional consumption (Kt)')+
  scale_colour_brewer(name="SSP (ref. regions mean)", palette = "Set1")+
  scale_x_continuous(limits=c(2020, 2050))+
  theme(legend.position = "bottom", legend.direction = "horizontal")


plot_bind <- plot_grid(per_capita, total, ncol = 1, labels = "AUTO")

ggsave(plot = plot_bind, device = "png", filename = "pathways.png", scale=1.5, width = 4, height = 5)

########
# Compare results with FAO prediction from year 2018 report
# Load FAO projections
projections = read.csv("FOFA2050CountryData_Market_q.csv") %>% filter(Indicator == "Commodity balances, volume" & (Item == "Beef and veal" | Item == "Poultry meat" | Item == "Pigmeat" | Item == "Sheep and goat meat") & Element =="Food use" & Region =="Sub-Saharan Africa")

projections = projections %>% group_by(Year, Scenario, Item) %>% summarise(Value=sum(Value))

projections$Source = "FAO"

# Load our projections
output <- read.csv("all_projections_2050_SSAfrica.csv", stringsAsFactors = F)

#
output$Total_kg=NULL

output$Scenario_region <- ifelse(output$Scenario_region=="Central Europe", "CEU", ifelse(output$Scenario_region=="East Asia", "EASIA", ifelse(output$Scenario_region=="Central Latin America", "CLAM", ifelse(output$Scenario_region=="North Africa and Middle East", "NMENA", NA ))))

output$SSP_Scenario = paste0(output$SSP_Scenario, output$Scenario_region)
output$Scenario_region = NULL

output$Source = "This study"

colnames(output) <- c("Year", "Scenario", "Population", "Item", "Value","Source")

output$Item = as.character(output$Item)
output$Scenario = as.character(output$Scenario)

output$Item[output$Item=="beef_tot"] <- "Beef and veal"
output$Item[output$Item=="mutton_tot"] <- "Sheep and goat meat"
output$Item[output$Item=="poultry_tot"] <- "Poultry meat"
output$Item[output$Item=="pork_tot"] <- "Pigmeat"

bind = dplyr::bind_rows(output, projections)

bind$Item[bind$Item=="Beef and veal"] <- "Beef, veal, and buffalo"
bind$Item[bind$Item=="Sheep and goat meat"] <- "Sheep, goat, and mutton"
bind$Item[bind$Item=="Poultry meat"] <- "Poultry"
bind$Item[bind$Item=="Pigmeat"] <- "Pigmeat"

proj_fao<-ggplot(projections, aes(x=Year, y=Value, colour=Scenario)) + 
  geom_line(size=1, alpha=0.5)+
  theme_classic()+
  scale_color_brewer(name="Scenario", palette="Set1")+
  ylab("Meat consumption in sub-Saharan Africa, Kt")+
  xlab("Year")+
  facet_wrap(~Item, scales = "fixed")

#ggsave("proj.png", plot = proj, device = "png", width = 18, height = 12, units = "cm", scale=0.9)

library(RColorBrewer)
n <- 23
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

bind$Year <-  as.POSIXct(as.character(bind$Year), format="%Y")

bind<-(padr::pad(bind, interval="1 year", group = c("Scenario", "Source", "Item")))

bind <- bind %>%
  group_by(Scenario, Source, Item) %>% 
  arrange(Year) %>%
  mutate(Value = na.approx(Value))

bind$Year  <- lubridate::year(bind$Year )

comparison = ggplot()+
  ggtitle('Consumption pathways comparison')+
  geom_line(data= filter(bind, Year>=2020), aes(x=Year,y=Value,color=Scenario, group=Scenario, linetype=Source), size=1, alpha=1)+
  facet_wrap(~Item)+
  ylab('Total consumption (Kt)')+
  scale_colour_manual(name="Scenario", values = col_vector)+
  scale_linetype_discrete(name="Source")+
  scale_x_continuous(limits=c(2020, 2050))+
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("comparison1.png", plot=comparison, device="png", height = 40, width = 40, scale=0.2)

#
bind$Scenario <- ifelse(grepl("SSP", bind$Scenario), substr(bind$Scenario, 5, nchar(bind$Scenario)), bind$Scenario)
#bind$Scenario <- ifelse(grepl("SSP", bind$Scenario), substr(bind$Scenario, 1, 4), bind$Scenario)

bind <- bind %>%
  group_by(Scenario, Item, Year, Source) %>% 
  summarise(Value = mean(Value))

comparison = ggplot()+
  ggtitle('Consumption pathways comparison')+
  geom_line(data= filter(bind, Year>=2020), aes(x=Year,y=Value,color=Scenario, group=Scenario, linetype=Source), size=1, alpha=1)+
  facet_wrap(~Item)+
  ylab('Total consumption (Kt)')+
  scale_colour_manual(name="Scenario (mean of SSPs 1-5 for each scenario)", values = col_vector[8:15])+
  scale_linetype_discrete(name="Source")+
  scale_x_continuous(limits=c(2020, 2050))+
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("comparison2.png", plot=comparison, device="png", height = 40, width = 40, scale=0.2)

#ggsave("SSP2_all.png", plot = total_output_smoothed_SSP2, device = "png", width = 40, height = 30, units = "cm", scale=0.4)

#ALL SSPs in a unique figure
#plot_grid(total_output_smoothed_SSP1, total_output_smoothed_SSP2, total_output_smoothed_SSP3, total_output_smoothed_SSP4, total_output_smoothed_SSP, nrow = 2, ncol = 2)

