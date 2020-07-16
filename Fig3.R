setwd('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/MEAT/Repo/meatSSA/Data')
# Figure3: SSPS and religion evolution for SSAfrica
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
gdp = gdp %>% dplyr::summarise(adjf = sum(adj*share, na.rm = TRUE)) 
gdp = gdp[1,1]
ssps = ssps %>% mutate(pcgdp=pcgdp*gdp)

## add future religion

religion_future <- read.csv("Religious_Composition_by_Country_2010-2050.csv")

religion_future$ï..Year

religion_future <- gather(religion_future, key="variable", value="value", 3:10)

ssps = merge(ssps, religion_future, by.y="ï..Year", by.x="variable", all=TRUE)

colnames(ssps)[3] <- "cgdppc"

###

ggplot(ssps)+
  geom_line(aes(x=as.numeric(as.character(variable)), y=cgdppc, group=Scenario, colour=Scenario))+
geom_line(aes(x=as.numeric(as.character(variable)), y=value*100000, , group=variable.y, colour=variable.y))+
  scale_x_continuous(limits = c(2010, 2050))

