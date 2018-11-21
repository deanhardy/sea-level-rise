rm(list=ls())

library(tidyverse) ## load tidyverse package

## set data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

## define SLR site(s) of interest
loc <- 'GMSL'
# loc <- 'grid_31.5_278.5' ## Sapelo Island, Georgia's closest grid point

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
               rsl.m = rsl.cm / 100)

slr3 <- filter(slr2, scenario.t != "Low")
  
## define plot/legend visual aids
clr<- (c("red", "orange", "green", "yellow", "blue"))
lbl <- (c("Extreme", "High", "Intermediate-High", "Intermediate", "Intermediate-Low"))
ln <- c("twodash", "longdash", "dotdash", "dashed", "solid")

## plot data
slr.plot <- ggplot(data = slr3, aes(x = year, y=rsl.m, group = scenario.t)) + 
  geom_smooth(data = filter(slr3, level == "med"), se = FALSE, size = 0.5,
              aes(color = scenario.t)) +
  scale_colour_manual(name='Scenario', values=clr, labels = lbl, breaks = lbl) +
  xlab("Year") + 
  ylab("Relative sea level (m)") +
  scale_y_continuous(limits = c(0,3), expand = c(0,0), sec.axis = sec_axis(~., labels = NULL)) +
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
slr.plot

## export tiff
tiff(file.path(datadir, "figures/gmsl-slr.tif"), width = 3.25, bg="white",
     height = 3, units = 'in', res = 300)
slr.plot
dev.off()

