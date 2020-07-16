dataset<-filter(dataset, Year==2050 & Type=="tot") %>% group_by(Impact) %>% dplyr::summarise(value=median(Value, na.rm = T)) %>% ungroup()

dataset$Impact=as.character(dataset$Impact)

categories <- unique(dataset$Impact)

# CHECK ORDER and update value!
estimated_current<-c(6.9, 46.7, 0.96, NA, 0.8, NA)

df <- data.frame(categories[1:6], estimated_current, dataset$value[1:6])

df <- tidyr::gather(df, "type", "value", 2:3)

df$type = as.factor(df$type)

levels(df$type) <- c("Median additional \nimpact from meat \nconsumption in 2050", "Current yearly impact \nfrom the agricultural \nsector")

df$categories=as.character(df$categories)

df = filter(df, df$categories!="Electricity~(TWh)" & df$categories!="Eutrophication~(Mt[PO4_eq])")

df$categories <- c("Land (Mkm2)", "Bluewater (Gm3)", "Fossil fuels (EJ)", "GHG (Gt CO2eq)", "Land (Mkm2)", "Bluewater (Gm3)", "Fossil fuels (EJ)", "GHG (Gt CO2eq)")

library(ggplot2)

fig5 <- ggplot(df, aes(x=categories, y=value, group=type, fill=type))+
  geom_bar(position = "fill",stat = "identity")+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("Share of total impact")+
  xlab("Impact category")+
  scale_fill_brewer(palette = "Set1", name="Type")

ggsave("fig5.png", fig5, device = "png", scale=1.3, width = 6)
