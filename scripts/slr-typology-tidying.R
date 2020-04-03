rm(list=ls())

library(tidyverse)
library(readxl)

datadir <- '/Users/dhardy/Dropbox/r_data/sea-level-rise'

dat_old <- read_xls(file.path(datadir, 'data/OLD-web-of-science-searches/savedrecs-slr+pop+risk+vuln+exp.xls'), sheet = 'filtered')
dat_new <- read_xls(file.path(datadir, 'data/web-of-science-searches/SCI-E+SSCI_slr+pop+risk+vuln+exp.xls'), sheet = 'filtered')
dat_new_full <- read_xls(file.path(datadir, 'data/web-of-science-searches/SCI-E+SSCI_slr+pop+risk+vuln+exp.xls'), sheet = 'savedrecs', skip = 27)


dat_addition <- anti_join(by = 'Title', dat_new, dat_old)
## 54 new articles to comb through, see dat_addition

dat_remove <- anti_join(by = 'Title', dat_old, dat_new)

## Was 101, now 155, so 54 new articles to comb through

## seems there are 3 articles in the old dataset that are not in the new dataset. see dat_remove
## Only 2 of the 3 were previously "relevant" meaning made the cut. One (Hallegatte et al 2011) has fewer citations (somehow), so now falls below the cut of >=4 cites/year
## The other, Dawson et al (2009 has) enough cites/yr, but doesn't focus on population, so maybe was cut due to that filter working better this time.

## modify to confirm to Google Doc columns, but manually reorder columns after export
dat4upload <- dat_addition %>%
  mutate(Assignment = NA,
    'DOI BASE' = 'http://dx.doi.org/', 'DOI LINK' = paste('http://dx.doi.org/', DOI, sep = ''),
    AUTHORS_ETAL = paste(Authors, " et al."), PaperID = seq(102, 155, 1)) %>%
  select('Publication Year',	'PaperID',	'DOI BASE',	'DOI', 'Assignment',	'DOI LINK',	'Title','Authors',	'AUTHORS_ETAL',	'Total Citations',	'Average per Year')

write.csv(dat4upload, file.path(datadir, 'data/web-of-science-searches/SCI-E+SSCI_slr+pop+risk+vuln+exp.csv'))


