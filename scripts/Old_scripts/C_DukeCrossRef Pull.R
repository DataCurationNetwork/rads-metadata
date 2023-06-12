################################
## Duke CrossRef metadata
##
## 2022-07-22
################################

#clear workspace
rm(list=ls())

#required packages
pacman::p_load(dplyr, 
               tidyr, 
               ggplot2)

pacman::p_load_current_gh("ropensci/rcrossref")



setwd("~/Documents/NSF_RADS/Metadata_Analysis/")

# first try the members search
dukemems <- cr_members(query = "Duke", cursor = "*", cursor_max = 1000)
dukememsdat <- dukemems$data

#dukeDOI prefix
dukedoi <- unlist(strsplit(dukememsdat$prefixes[which(dukememsdat$primary_name == "Duke University Libraries")], ","))

# then search works based on those DOI prefixes (doing it separately instead looped because there are only three)
duke1 <- cr_prefixes(prefixes=dukedoi[1], .progress = "text", works = T, cursor = "*", cursor_max = 13200)
duke2 <- cr_prefixes(prefixes=dukedoi[2], .progress = "text", works = T, cursor = "*", cursor_max = 13200)
duke3 <- cr_prefixes(prefixes=dukedoi[3], .progress = "text", works = T, cursor = "*", cursor_max = 13200)

duke2dat <- duke2$data
duke3dat <- duke3$data

dukedat <- bind_rows(duke2dat, duke3dat)

#write out data, removing author column (nested list)
dukedat %>% 
  select(-author) %>% 
write.csv(file="Duke_CrossrefDOIs_20220722.csv", row.names = F)

dukedat %>% 
  group_by(type, container.title, publisher) %>% 
  summarize(count=n()) %>% 
  knitr::kable()

summary(duplicated(dukedat$doi))
