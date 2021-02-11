
method_ela = function(xvar, xlim_1, xlab, ylab, xlim_2, filename, type){

ppa <- as.data.frame(cbind(pp$yhat, pp$xvar[xvar]))

colnames(ppa) <- c("V1", "V2")

p <- ggplot(ppa, aes(x=V2, y=V1))+
  geom_smooth()+
  xlim(c(5,xlim_1))+
  ylim(c(0, 80))

p

x <- ggplot_build(p)$data[[1]]$x
y <- ggplot_build(p)$data[[1]]$y

x = x[2:length(x)]
y = (diff(y)/y[2:length(y)]) / (diff(x)/x[2:length(x)])

df <- data.frame(x, y)

df <- df[1:nrow(df)-1,]

r <- readxl::read_xlsx("ppp_gdp_2011.xlsx")
r$x=-0.75

r <- subset(r, r$Country=="Kenya" |r$Country=="Germany" | r$Country=="South Africa" | r$Country=="United States"| r$Country=="United Kingdom" | r$Country=="Russian Federation"| r$Country=="India" | r$Country=="China")

r$Country <- countrycode::countrycode(r$Country, "country.name", 'iso3c')

p <- ggplot()+
  geom_line(data=df, aes(x=x, y=y))+
  theme_classic()+
  xlab(xlab)+
  ylab(ylab)+
  coord_cartesian(xlim=c(5, 50000), ylim = c(-1, 1.5))+
  geom_hline(yintercept=0, linetype='dotted', col = 'red')+
  geom_point(data=r, aes(y=x, x=GDP, label=Country), size=2.5, shape=25, fill="black")+
  geom_text(data=r, aes(y=x+0.15, x=GDP, label=Country), size=2.5, angle = 90)

ggsave(filename, device = "png", plot = p)
}