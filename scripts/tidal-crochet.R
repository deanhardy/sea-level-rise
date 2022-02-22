################################################################################
## By: Dean Hardy
## script to download and graph mean monthly sea level from NOAA tide gage stations
## depends on rnoaa & tidyverse packages
################################################################################
rm(list=ls())

library(rnoaa) ## package info https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
library(tidyverse)
library(hydrostats)
library(lubridate)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

## import average seasonal cycle data
# test <- read.csv("https://tidesandcurrents.noaa.gov/sltrends/data/USAverageSeasonalCycleData.csv",
#                  stringsAsFactors = FALSE)

## define variables
STATION <- c(8670870) ## define stations 8720030 (Fernandina), 8670870 (Fort Pulaski), 8661070 (Springmaid Pier, SC)
DATUM <- 'MSL' ## define datum
P <- seq(1970,1970,1) ## define number of decades of data to grab where 0 = 1 decade, 1 = 2 decades, etc
df <- NULL ## empty dataframe

hh <-
  coops_search(
    begin_date = as.character(as.Date(paste('2000', "-01-01", sep = '')), format = '%Y%m%d') %>% 
      gsub('-', '', .) %>%
      as.numeric(),
    end_date = as.character(as.Date(paste('2000', '-12-31', sep = '')), format = '%Y%m%d') %>%
      gsub('-', '', .) %>%
      as.numeric(),
    station_name = STATION,
    product = 'high_low', 
    datum = DATUM, 
    units = 'english', 
    time_zone = 'GMT')$data %>%
  filter(v == max(v)) %>%
  mutate(date = as.Date(t, format = '%Y%m%d'),
         year = year(date),
         month = month(date))


ggplot(aes(t, v), data = hh) +
  geom_point()

## for loop to grab data in decadal increments for multiple stations
for (z in 1:length(STATION)) {
  
for (i in 1:length(P)) {
# DATE <- Sys.Date()-(365*P)

OUT <-
  coops_search(
    begin_date = as.character(as.Date(paste(P[[i]], "-01-01", sep = '')), format = '%Y%m%d') %>% 
      gsub('-', '', .) %>%
      as.numeric(),
    end_date = as.character(as.Date(paste(P[[i]], '-12-31', sep = '')), format = '%Y%m%d') %>%
      gsub('-', '', .) %>%
      as.numeric(),
    station_name = STATION[[z]],
    product = 'high_low', 
    datum = DATUM, 
    units = 'metric', 
    time_zone = 'GMT')$data

  
  OUT2 <- OUT %>%
    filter(v == max(v)) %>%
    mutate(date = as.Date(t, format = '%Y%m%d'),
           year = year(date),
           month = month(date)) %>%
    mutate(station = as.character(STATION[[z]]))
    # mutate(yrmo = paste(year, month, sep = '-'))
    # mutate(date = as.Date(paste(yrmo, sep = '')))

  df <- rbind(df, OUT2)
}
}

ggplot(aes(t, v), data = df) +
  geom_point()




# ## plot annual mean sea levels for all stations and draw regression line for each station
# df.annual <- df %>%
#   group_by(year) %>%
#   summarise(MSL = mean(MSL*39.3701)) 
# 
# class <- df.annual %>%
#   mutate(class = )
# 
# range <- (max(df.annual$MSL, na.rm = T) - min(df.annual$MSL, na.rm = T)) / 12
# 
# fig <- ggplot(df.annual, aes(x = year, y = MSL)) +
#   geom_line(lwd = 0.5) + 
#   geom_smooth(method = 'loess') + 
#   scale_y_continuous(name = paste('Datum', DATUM, '(in)'),
#                      breaks = round(seq(-9, 8, by = 1), 2),
#                      minor_breaks = seq(-9, 8, by = 1)) + 
#   scale_color_gradient(low = 'yellow', high = 'red') + 
#   # scale_x_date(name = 'Date', 
#   #              date_breaks = '12 months', 
#   #              date_labels = '%y') + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         legend.position = 'right') + 
#   ggtitle(paste("Annual Mean Sea Level")) 
# fig
# 
# ## plot mean monthly sea levels for all stations and draw regression line for each station
# fig <- ggplot(df, aes(x = date, y = MSL, color = station)) +
#   geom_line(lwd = 0.5) + 
#   geom_smooth(method = 'lm') + 
#   scale_y_continuous(name = paste('Datum', DATUM, '(m)'),
#                      breaks = round(seq(-0.2, 0.4, by = 0.1), 2),
#                      minor_breaks = seq(-0.2, 0.4, by = 0.05)) + 
#   scale_x_date(name = 'Date', 
#                date_breaks = '12 months', 
#                date_labels = '%y') + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         legend.position = 'right') + 
#   ggtitle(paste("Monthly Mean Sea Level")) 
# fig


