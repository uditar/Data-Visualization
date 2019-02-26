library(ggplot2)
library(dplyr)

Minard <- read.csv("minard-data.csv",header=TRUE,sep=",")

Troops <- select(Minard, long = LONP, lat = LATP, survivors = SURV, direction = DIR, division = DIV)
Cities <- select(Minard, long = LONC, lat = LATC, city = CITY)
Cities[complete.cases(Cities), ]

Temp <- select(Minard, long=LONT, temp=TEMP,MON,DAY)
Temp$date<-paste(Temp$MON, Temp$DAY)
Temp[complete.cases(Temp), ]

plot_troops <- ggplot(Troops, aes(long, lat)) 
geom_path(mapping = NULL, data = NULL, stat = "identity", position = "identity", lineend = "butt", linejoin = "round", linemitre = 1, arrow = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

plot_cities <- plot_troops + geom_path(aes(size = survivors, color = direction, group = division), lineend = "round", linejoin = "round")
plot_cities_labels <- plot_cities + geom_text(aes(label = city), size = 4.5, data = Cities) + geom_point(data = Cities)
v <- c(1, 2, 3) * 10^5
plot_march <- plot_cities_labels + scale_size("Survivors", range = c(1, 22), breaks = v, labels = comma(v)) +
  scale_color_manual("Direction", values = c("#DFC17E", "grey50")) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Napoleon's march to Russia")
plot_march

plot_temp <- ggplot(Temp, aes(long, temp)) +
  geom_path(color="grey", size=1.5) +
  geom_point(size=2) +
  geom_text(aes(label=date)) +
  xlab("Longitude") + ylab("Temperature") +
  coord_cartesian(xlim = c(24, 38)) + 
  theme_bw()

grid.arrange(plot_march, plot_temp, nrow=2, heights=c(3,2))
