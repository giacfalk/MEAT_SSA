dataset<-filter(dataset, Year==2050 & Type=="tot") %>% group_by(Impact) %>% summarise(value=median(Value, na.rm = T)) %>% ungroup()

dataset$Impact=as.character(dataset$Impact)

categories <- unique(dataset$Impact)

estimated_current<-c(2.3, 24, 0.75, NA, 2.3, NA)

df <- data.frame(categories, estimated_current, dataset$value)

df <- tidyr::gather(df, "type", "value", 2:3)

df$type = as.factor(df$type)

levels(df$type) <- c("Median additional \nimpact from meat \nconsumption in 2050", "Current yearly impact \nfrom the agricultural \nsector")

df = filter(df, df$categories!="Electricity~(TWh)" & df$categories!="Eutrophication~(Mt[PO4_eq])")

levels(df$categories) <- c("Bluewater (Gm3)", "Electricity (TWh)", "Bluewater (Gm3)", "Fossil fuels (EJ)", "GHG (Gt CO2eq)", "Land (Mkm2)")

library(ggplot2)

fig5 <- ggplot(df, aes(x=categories, y=value, group=type, fill=type))+
  geom_bar(position = "fill",stat = "identity")+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("Share of total impact")+
  xlab("Impact category")+
  scale_fill_brewer(palette = "Set1", name="Type")

ggsave("fig5.png", fig5, device = "png", scale=1.3)
