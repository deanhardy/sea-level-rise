################################################################################
## By: Dean Hardy
## script to download and graph mean monthly sea level from NOAA tide gage stations
## depends on rnoaa & tidyverse packages
################################################################################
rm(list=ls())

library(rnoaa) ## package info https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
library(tidyverse)
library(tidyquant)
library(hydrostats)
library(zoo)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

## set text size in figures
txt <- 32  

## import average seasonal cycle data
# test <- read.csv("https://tidesandcurrents.noaa.gov/sltrends/data/USAverageSeasonalCycleData.csv",
#                  stringsAsFactors = FALSE)

## define variables

STATION <- c(8720030, 8670870) ## define stations 8720030 (Fernandina), 8670870 (Fort Pulaski), 8661070 (Springmaid Pier, SC), 8729108 (Panama City, FL)
DATUM <- 'MSL' ## define datum
P <- seq(0,8,1) ## define number of decades of data to grab where 0 = 1 decade, 1 = 2 decades, etc
df <- NULL ## empty dataframe

# high_low <-
#   coops_search(
#     begin_date = as.character(as.Date("2000-03-09"), format = '%Y%m%d') %>% 
#       gsub('-', '', .) %>%
#       as.numeric(),
#     end_date = as.character(as.Date("2001-03-09"), format = '%Y%m%d') %>%
#       gsub('-', '', .) %>%
#       as.numeric(),
#     station_name = '8670870',
#     product = 'high_low', 
#     datum = 'MSL', 
#     units = 'metric', 
#     time_zone = 'GMT')$data

# hl <- high_low %>% filter(ty == 'HH')
# 
# ggplot(aes(t, v), data = hl) +
#   geom_point()

## for loop to grab data in decadal increments for multiple stations
for (z in 1:length(STATION)) {
  
for (i in 1:length(P)) {
DATE <- Sys.Date()-(3653*P)

OUT <-
  coops_search(
    begin_date = as.character(as.Date(DATE[[i]]-3653), format = '%Y%m%d') %>% 
      gsub('-', '', .) %>%
      as.numeric(),
    end_date = as.character(as.Date(DATE[[i]]), format = '%Y%m%d') %>%
      gsub('-', '', .) %>%
      as.numeric(),
    station_name = STATION[[z]],
    product = 'monthly_mean', 
    datum = DATUM, 
    units = 'metric', 
    time_zone = 'GMT')$data 
  
  OUT2 <- OUT %>%
    mutate(station = as.character(STATION[[z]])) %>%
    mutate(yrmo = paste(year, month, sep = '-')) %>%
    mutate(date = as.Date(paste(yrmo, '-15', sep = '')))

  df <- rbind(df, OUT2)
}
}

## plot annual mean sea levels for all stations and draw regression line for each station
df.annual <- df %>%
  group_by(year) %>%
  summarise(MSL = mean(MSL*39.3701)) 

# class <- df.annual %>%
#   mutate(class = )

range <- (max(df.annual$MSL, na.rm = T) - min(df.annual$MSL, na.rm = T)) / 12

fig <- ggplot(df.annual, aes(x = year, y = MSL)) +
  geom_line(lwd = 0.5) + 
  geom_smooth(method = 'loess') + 
  scale_y_continuous(name = paste('Datum', DATUM, '(in)'),
                     breaks = round(seq(-9, 8, by = 1), 2),
                     minor_breaks = seq(-9, 8, by = 1)) + 
  scale_color_gradient(low = 'yellow', high = 'red') + 
  # scale_x_date(name = 'Date', 
  #              date_breaks = '12 months', 
  #              date_labels = '%y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = 'right') + 
  ggtitle(paste("Annual Mean Sea Level")) 
fig

## plot mean monthly sea levels for all stations and draw regression line for each station
fig <- ggplot(df, aes(x = date, y = MSL, color = station)) +
  geom_line(lwd = 0.5) + 
  geom_smooth(method = 'lm') + 
  scale_y_continuous(name = paste('Datum', DATUM, '(m)'),
                     breaks = round(seq(-0.2, 0.5, by = 0.1), 2),
                     minor_breaks = seq(-0.2, 0.5, by = 0.05)) + 
  scale_x_date(name = 'Date', 
               date_breaks = '12 months', 
               date_labels = '%y') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = 'right') + 
  ggtitle(paste("Monthly Mean Sea Level")) 
fig

# save plots as .png
ggsave(fig, file=paste(datadir,
                        '/figures/sea-level', ".png", sep=''), width = 6, height = 4, units = 'in', scale=2)

## working on adding equation with slope
library(ggpmisc)

## set parameters
my.formula <- y ~ x # generic formula for use in equation
D <- P+1

for (z in 1:length(STATION)) {

for (i in D) {

T <- 3653*i ## set time period
T_name <- ifelse(T == 3653, "Decade", 
                 ifelse(T == 3653*2, 'Two Decades', 
                        ifelse(T == 3653*3, 'Three Decades', 
                               ifelse(T == 3653*4, 'Four Decades',
                                      ifelse(T == 3653*5, 'Five Decades',
                                             ifelse(T == 3653*6, 'Six Decades',
                                                    ifelse(T == 3653*7, 'Seven Decades', 
                                                           ifelse(T == 3653*8, 'Eight Decades', 'Nine Decades'))))))))

## filter all data to one station and selected time period
# dat2 <- filter(df, station == STATION[1]) %>%
#   mutate(MSL = MSL*100) %>%
#   arrange(date) %>%
#   ifelse(first(date) > Sys.Date()-T, filter(dat2$date >= first(date)), filter(dat2$date >= Sys.Date()-T))

dat2 <- filter(df, station == STATION[[z]]) %>%
  mutate(MSL = MSL*100) %>%
  arrange(date) %>%
filter(
  if (first(date) > Sys.Date()-T) {
    date >= first(date)
  } else {
    date >= Sys.Date()-T
  }
)
  
m <- lm(MSL ~ date, dat2) ## create regression line

## 
## mimic style in NOAA graph here: https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8670870
## add moving averages https://www.r-bloggers.com/2021/04/ggplot-with-moving-averages/
fig2 <- ggplot(dat2, aes(x = date, y = MSL)) +
  geom_hline(yintercept = 0, linetype = 1.5, lwd = 0.5) +
  geom_line(color = 'blue', lwd = 1) + 
  geom_smooth(method = 'lm', color = 'black', formula = my.formula) +
  geom_smooth(method = 'loess', color = 'grey30', linetype = 2, se = FALSE) +
  geom_ma(ma_fun = SMA, n = 60, color = "red") +  # Plot 5-year SMA
  scale_y_continuous(name = paste(DATUM, '(cm)'),
                     breaks = seq(-50, 50, by = 10),
                     minor_breaks = seq(-50, 50, by = 5),
                     limits = c(-50, 50),
                     expand = c(0,0),
                     sec.axis = sec_axis(~. *0.393701, 
                                         name = "MSL (inches)",
                                         breaks = seq(-20,20,2))) + 
  scale_x_date(name = 'Year', 
               date_breaks = '2 years', 
               date_minor_breaks = '1 year',
               date_labels = '%Y',
               expand = c(0,0)) + 
              # limits = c(first(date), last(date))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = txt),
        axis.text.y = element_text(size = txt),
        text = element_text(size = txt),
        legend.position = 'bottom',
        panel.background = element_rect(fill = "white", color = 'black', linetype = 1),
        #panel.border = element_rect(fill = 'white', color = 'black'),
        panel.grid.major.y = element_line(colour = 'black', linetype = 2),
        panel.grid.minor.y = element_line(colour = 'black', linetype = 2),
        axis.ticks.x = element_line(color = 'black')) + 
  ggtitle(paste("Sea Level Trend Over Past ", T_name, sep = ''))  +
  annotate(geom = 'text', label = paste("Total Observed SLR for Period =",
                                        round(coef(m)[2]*T, 2), 'cm'),
           x = Sys.Date()-(T), y = Inf, hjust = -0.1, vjust = 5, size = 12) +
  annotate(geom = 'text', label = paste("Linear Relative Sea Level Trend (black line) =",
                                        round((coef(m)[2]*T*10/(T/365.3)), 2), 'mm/yr'),
           x = Sys.Date()-(T), y = Inf, hjust =-0.08, vjust = 7, size = 12) +
  labs(caption = paste("Data: Monthly ", DATUM, " for NOAA Station ID: ", dat2$station, 
                       ', ', first(as.yearmon(dat2$date)), ' to ', last(as.yearmon(dat2$date)), sep = ''))
fig2

# save plots as .png
ggsave(fig2, file=paste(datadir,
                       '/sea-level-trends/', dat2$station[[z]], '-sea-level-trends_', i, "-decades", ".png", sep=''), width = 13, height = 6, units = 'in', scale=2)
}
}
