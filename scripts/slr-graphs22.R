## script to plot Sweet et al. 2022 SLR data from NOAA and other agencies.

rm(list=ls())

library(tidyverse) ## load tidyverse package
library(data.table)

## set data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

## define SLR site(s) of interest
loc <- 'GMSL'

## import data
slr22 <- read.csv(file.path(datadir, "data/slr-noaa22.csv"), stringsAsFactors = F, header=T, skip = 17) %>%
  select(!c(X:X.93)) 

## rename columnns
old <- colnames(slr22)
new <- c('site', 'psmsl_id', 'noaa_id', 'name', 'grid_num', 'lat', 'long', 'region_class', 'coastline_intersect', 'scenario', 'vlm_contribution',
         'epoch_offset_92_00', 'offset_00_05', 'rsl2005', 'rsl2020', 'rsl2030', 'rsl2040', 'rsl2050', 'rsl2060', 'rsl2070', 'rsl2080', 'rsl2090', 
         'rsl2100', 'rsl2110', 'rsl2120', 'rsl2130', 'rsl2140', 'rsl2150')
  
setnames(slr22, old = old, 
         new = new)

## filter to site(s) of interest
slr_f <- slr22 %>%
  filter(site == loc) %>%
  select(site, scenario, rsl2005:rsl2100) %>%
  mutate(rsl2000 = as.integer(0)) %>%
  separate(scenario, c('scenario', 'level'), sep = " - ") %>%
  gather('year', 'rsl.cm', 4:14) %>%
  mutate(level = tolower(level),
         year = ifelse(year == 'rsl2000', 2000, 
                       ifelse(year == 'rsl2010', 2010,
                              ifelse(year == 'rsl2020', 2020,
                                     ifelse(year == 'rsl2030', 2030, 
                                            ifelse(year == 'rsl2040', 2040,
                                                   ifelse(year == 'rsl2050', 2050,
                                                          ifelse(year == 'rsl2060', 2060,
                                                                 ifelse(year == 'rsl2070', 2070,
                                                                        ifelse(year == 'rsl2080', 2080,
                                                                               ifelse(year == 'rsl2090', 2090,
                                                                                      ifelse(year == 'rsl2100', 2100, 
                                                                                             if_else(year == 'rsl2005', 2005, year))))))))))))) %>%
  arrange(., scenario) %>%
  mutate(year = as.integer(year))

## add text description column for scenarios
slr2 <- mutate(slr_f, scenario.t = rep(c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"), 
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
