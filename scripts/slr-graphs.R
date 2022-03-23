rm(list=ls())

library(tidyverse) ## load tidyverse package

## set data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

## define SLR site(s) of interest
loc <- c('grid_31.5_278.5') ## Sapelo Island, Georgia's closest grid point

## import data from Sweet et al 2017, grid pt 31.5, 278.5 just inland in McIntosh
slr <- read.csv(file.path(datadir, "data/slr-noaa17.csv"), stringsAsFactors = F, header=T)

## filter to site(s) of interest
slr_f <- slr %>%
  filter(site == loc) %>%
  select(site, scenario, rsl00:rsl2100) %>%
  separate(scenario, c('scenario', 'level'), sep = " - ") %>%
  gather('year', 'rsl.cm', 4:14) %>%
  mutate(level = tolower(level),
         year = ifelse(year == 'rsl00', 2000, 
                       ifelse(year == 'rsl10', 2010,
                              ifelse(year == 'rsl20', 2020,
                                     ifelse(year == 'rsl30', 2030, 
                                            ifelse(year == 'rsl40', 2040,
                                                   ifelse(year == 'rsl50', 2050,
                                                          ifelse(year == 'rsl60', 2060,
                                                                 ifelse(year == 'rsl70', 2070,
                                                                        ifelse(year == 'rsl80', 2080,
                                                                               ifelse(year == 'rsl90', 2090,
                                                                                      ifelse(year == 'rsl2100', 2100, year)))))))))))) %>%
  arrange(., scenario) %>%
  mutate(year = as.integer(year))

## add text description column for scenarios
slr2 <- mutate(slr_f, scenario.t = rep(c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High", "Extreme"), 
                                  each = 33),
               rsl.m = rsl.cm / 100) %>%
               mutate(rsl.ft = rsl.m * 3.28084)

slr3 <- filter(slr2, scenario.t != "Low") 
  
## define plot/legend visual aids
clr<- (c("red", "orange", "green", "yellow", "blue"))
lbl <- (c("Extreme", "High", "Intermediate-High", "Intermediate", "Intermediate-Low"))
ln <- c("twodash", "longdash", "dotdash", "dashed", "solid")

## plot data
slr.mid <- ggplot(data = slr3, aes(x = year, y=rsl.ft, group = scenario.t)) + 
  geom_smooth(data = filter(slr3, level == 'med'), se = FALSE, size = 0.5,
              aes(color = scenario.t)) +
  scale_colour_manual(name='NOAA SLR Scenario', values=clr, labels = lbl, breaks = lbl) +
  xlab("Year") + 
  ylab("Relative sea level (ft)") +
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, 1), expand = c(0,0), 
                     sec.axis = dup_axis(name ='', labels = NULL)) +
  scale_x_continuous(limits = c(2000,2100), expand = c(0,0), 
                     sec.axis = sec_axis(~., labels = NULL)) +
  ggtitle('Sea Level Rise Forecast for Sapelo Island Area') +
  labs(caption = 'Data from Sweet et al. 2017') + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_line(colour = "black"),
         axis.ticks.length = unit(-0.1, "cm"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(margin = margin(5,0.5,5,5, "cm")),
        axis.text.y.right = element_blank(),
        axis.text.x.top = element_blank(),
        legend.position = c(0.32, 0.65)
        ##legend.title = element_text(name='SLR Scenario')
  )
slr.mid

## export
jpeg(file.path(datadir, "figures/sea-level-rise-forecast-sapelo-year2100.jpg"), width = 7, bg="white",
     height = 5, units = 'in', res = 300)
slr.mid
dev.off()

## plot data using shading for upper and lower ranges
slr.ranges <- ggplot(data = slr3, aes(x = year, y=rsl.m, group = scenario.t)) + 
  geom_smooth(data = filter(slr3, level == 'med'), se = FALSE, size = 0.5,
              aes(color = scenario.t)) +
  # geom_line() + 
  geom_ribbon(data = filter(slr3, level == 'low'), aes(ymin = rsl.m, ymax = rsl.m, color = scenario.t), 
              linetype = 2, alpha = 0, lwd = 0.1) +
  geom_ribbon(data = filter(slr3, level == 'high'), aes(ymin = rsl.m, ymax = rsl.m, color = scenario.t), 
              linetype = 2, alpha = 0, lwd = 0.1) +
  scale_colour_manual(name='Scenario', values=clr, labels = lbl, breaks = lbl) +
  xlab("Year") + 
  ylab("Relative sea level (m)") +
  scale_y_continuous(limits = c(0,3.5), expand = c(0,0), sec.axis = sec_axis(~., labels = NULL)) +
  scale_x_continuous(limits = c(2000,2100), expand = c(0,0), sec.axis = sec_axis(~., labels = NULL)) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(margin = margin(5,0.5,5,5, "cm")),
        axis.text.y.right = element_blank(),
        axis.text.x.top = element_blank(),
        legend.position = c(0.25, 0.65)
        ##legend.title = element_text(name='SLR Scenario')
  ) + 
  ggtitle("Sapelo Island, Georgia") + 
  labs(caption = "Data from Kopp et al. (2014)")
slr.ranges

## export tiff
tiff(file.path(datadir, "figures/sapelo-slr-ranges.tif"), width = 7, bg="white",
     height = 7, units = 'in', res = 300)
slr.ranges
dev.off()
