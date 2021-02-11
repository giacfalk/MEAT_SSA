# plot data and produce descriptive statistics for SSA

y_ssa <- y %>% filter(grepl("Africa", continent) & !grepl("North Africa and Middle East", continent))

pop = wb(indicator = "SP.POP.TOTL", startdate = 1960, enddate = 2019)

y_ssa <- merge(y_ssa, pop, by.x=c("Code", "Year"), by.y=c("iso3c", "date"), all.x=T)

y_ssa <- y_ssa %>% group_by(Year) %>%  dplyr::summarise(Beef.and.buffalo..kg.=stats::weighted.mean(Beef.and.buffalo..kg., value, na.rm=T), Pigmeat..kg.=stats::weighted.mean(Pigmeat..kg., value, na.rm=T), Poultry..kg.=stats::weighted.mean(Poultry..kg., value, na.rm=T), Mutton...goat..kg.=stats::weighted.mean(Mutton...goat..kg., value, na.rm=T)) 

y_ssa <- melt(y_ssa, id.vars = "Year")

y_ssa$variable <- as.character(y_ssa$variable)

y_ssa$variable[y_ssa$variable=="Beef.and.buffalo..kg."] <- "Beef and buffalo"
y_ssa$variable[y_ssa$variable=="Pigmeat..kg."] <- "Pigmeat"
y_ssa$variable[y_ssa$variable=="Poultry..kg."] <- "Poultry"
y_ssa$variable[y_ssa$variable=="Mutton...goat..kg."] <- "Mutton and goat"

a <- ggplot(y_ssa)+
  theme_classic()+
  geom_line(aes(x=Year, y=value, colour=variable, group=variable))+
  ggtitle("Evolution of per-capita meat consumption in sub-Saharan Africa")+
  ylab("kg/person/year")+
  scale_colour_discrete(name="Meat type")+
  theme(legend.position = "none")

#

# plot data and produce descriptive statistics for SSA

y_ssa <- y %>% filter(grepl("Africa", continent) & !grepl("North Africa and Middle East", continent))

pop = wb(indicator = "SP.POP.TOTL", startdate = 1960, enddate = 2019)

y_ssa <- merge(y_ssa, pop, by.x=c("Code", "Year"), by.y=c("iso3c", "date"), all.x=T)

y_ssa <- y_ssa %>% group_by(Year) %>%  dplyr::summarise(Beef.and.buffalo..kg.= sum(Beef.and.buffalo..kg.*value, na.rm=T), Pigmeat..kg.=sum(Pigmeat..kg.*value, na.rm=T), Poultry..kg.=sum(Poultry..kg.*value, na.rm=T), Mutton...goat..kg.=sum(Mutton...goat..kg.*value, na.rm=T)) 

y_ssa <- melt(y_ssa, id.vars = "Year")

y_ssa$variable <- as.character(y_ssa$variable)

y_ssa$variable[y_ssa$variable=="Beef.and.buffalo..kg."] <- "Beef and buffalo"
y_ssa$variable[y_ssa$variable=="Pigmeat..kg."] <- "Pigmeat"
y_ssa$variable[y_ssa$variable=="Poultry..kg."] <- "Poultry"
y_ssa$variable[y_ssa$variable=="Mutton...goat..kg."] <- "Mutton and goat"

b <- ggplot(y_ssa)+
  theme_classic()+
  geom_line(aes(x=Year, y=value/1000000, colour=variable, group=variable))+
  ggtitle("Evolution of total meat consumption in sub-Saharan Africa")+
  ylab("Kt/year")+
  scale_colour_discrete(name="Meat type")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

y_ssa <- y %>% filter(grepl("Africa", continent) & !grepl("North Africa and Middle East", continent))

pop <- filter(pop, iso3c %in% unique(y_ssa$Code))

pop <- group_by(pop, date) %>% summarise(value=sum(value, na.rm = T)) %>% ungroup()

c <- ggplot(data=pop, aes(x=as.numeric(date), y=value/1000000))+
  theme_classic()+
  geom_line()+
  ggtitle("Evolution of population in sub-Saharan Africa")+
  ylab("Million people")+
  xlab("Year")

d <- plot_grid(a, c, b, rel_heights = c(1, 1, 1.15), labels="AUTO", ncol = 1)

ggsave(plot=d, device = "png", filename = "fig1.png", scale=2, width = 3)

###

write.csv(y, "data_historical.csv")
write.csv(ssps, "data_future.csv")
