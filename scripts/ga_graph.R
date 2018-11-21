rm(list=ls())

library(tidyverse) ## load tidyverse package

## set data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

## import data from Sweet et al 2017, grid pt 31.5, 278.5 just inland in McIntosh
ga <- read.csv(file.path(datadir, "data/ga-slr-noaa17.csv"), header=T)

## add text description column for scenarios
ga <- mutate(ga, scenario.t = rep(c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High", "Extreme"), 
                                  each = 42))

ga2 <- filter(ga, scenario.t != "Low")

## define plot/legend visual aids
clr<- (c("red", "orange", "green", "yellow", "blue"))
lbl <- (c("Extreme", "High", "Intermediate-High", "Intermediate", "Intermediate-Low"))
ln <- c("twodash", "longdash", "dotdash", "dashed", "solid")

## plot data
ga.plot <- ggplot(data = ga2, aes(x = year, y=rsl.m, group = scenario.t)) + 
  geom_smooth(data = filter(ga2, level == "med"), se = FALSE, size = 0.5,
              aes(color = scenario.t)) +
  scale_colour_manual(name='Scenario', values=clr, labels = lbl, breaks = lbl) +
  xlab("Year") + 
  ylab("Relative sea level (m)") +
  scale_y_continuous(limits = c(0,4), expand = c(0,0), sec.axis = sec_axis(~., labels = NULL)) +
  scale_x_continuous(limits = c(2000,2100), expand = c(0,0), sec.axis = sec_axis(~., labels = NULL)) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(margin = margin(5,0.5,5,5, "cm")),
        axis.text.y.right = element_blank(),
        axis.text.x.top = element_blank(),
        legend.position = c(0.32, 0.65)
        ##legend.title = element_text(name='SLR Scenario')
  )
ga.plot

## export tiff
tiff(file.path(datadir, "figures/ga-slr.tif"), width = 3.25, bg="white",
     height = 3, units = 'in', res = 300)
ga.plot
dev.off()



## plot data
ga.plot2 <- ggplot(data = ga2, aes(x = year, y=rsl.m, group = scenario.t)) + 
  geom_smooth(data = filter(ga2, level == "med"), se = FALSE, size = 0.5,
              aes(color = scenario.t, linetype = scenario.t)) +
  scale_linetype_manual(values = ln) +
  scale_colour_manual(name='SLR Scenario', values=clr, labels = lbl, breaks = lbl) +
  xlab("Year") + 
  ylab("Georgia relative sea level (m)") +
  scale_y_continuous(limits = c(0,4), expand = c(0,0), sec.axis = sec_axis(~., labels = NULL)) +
  scale_x_continuous(limits = c(2000,2100), expand = c(0,0), sec.axis = sec_axis(~., labels = NULL)) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(-0.2, "cm"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(margin = margin(5,0.5,5,5, "cm")),
        axis.text.y.right = element_blank(),
        axis.text.x.top = element_blank(),
        legend.position = c(0.32, 0.65)
        ##legend.title = element_text(name='SLR Scenario')
  )
ga.plot2

## export tiff
tiff("figures/ga.slr2.tif", width = 3.25, bg="white",
     height = 3, units = 'in', res = 300)
dev.off()