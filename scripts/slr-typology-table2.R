rm(list=ls())

library(tidyverse)

datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

applevels <- c('MHHW', 'EAE', 'RL100', 'LECZ')

df <- read.csv(file.path(datadir, 'data/slr-typology-table2.csv'), stringsAsFactors = FALSE) %>%
  select(-FIPS.) %>%
  gather('Approach', 'percent',3:6) %>%
  mutate(Group2 = Group, 
         Group = as.character(Group),
         GroupApproach = ifelse(Approach == 'MHHW', str_c(Group, '.1'), 
                                ifelse(Approach == 'RL100', str_c(Group, '.2'),
                                       ifelse(Approach == 'LECZ', str_c(Group, '.3'),
                                              ifelse(Approach == 'EAE', str_c(Group, '.4'), Group))))) %>%
  mutate(Approach = factor(Approach, levels = applevels))

fig <- ggplot(filter(df, Group2 %in% c(1:6, 8:9))) +
  geom_col(aes(x = reorder(County, Group2), y=0.25, group = (GroupApproach), fill = Group)) + 
  geom_point(aes(x=reorder(County, Group2), y=percent, color = Approach)) +
  geom_path(aes(x=reorder(County, Group2), y=percent, group = (GroupApproach), color = Approach)) +
  scale_y_continuous(breaks = seq(0,1,0.1), expand = c(0,0)) + 
  # scale_x_discrete(expand=c(0,0)) +
  labs(y = "Percent", x = "County") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = 'black', fill = NA),
        panel.grid.major.y = element_line(color = 'grey', linetype = 'dashed'),
        # panel.grid.major.x = element_line(color = 'grey', linetype = 'dashed'),
        panel.grid.major.x = element_blank())
fig

tiff(file.path(datadir, 'figures/table2_plotted.tif'), height = 4, width = 6, units = 'in', compression = 'lzw', res = 300)
fig
dev.off()
