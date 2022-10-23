################################
## Pull CrossRef metadata
##
## 2022-10-12
################################

#clear workspace
rm(list=ls())

#required packages
pacman::p_load(dplyr, 
               tidyr, 
               ggplot2)

pacman::p_load_current_gh("ropensci/rcrossref")

# BY AFFILIATION ############
#only pull data dois, as cross ref doesn't have a software type: https://api.crossref.org/v1/types

## STEP 1: Pull data from crossref
# query_strings <- data.frame(search = c("*University*of*Minnesota*",
#                                        "*Cornell*University*",
#                                        "*Duke*University*",
#                                        "*University*of*Michigan*",
#                                        "*Washington*University*in*St.*Louis",
#                                        "*Virginia*Tech*"),
#                             institution = c("UMN", "Cornell", "Duke", "Michigan", "WashU", "VT"),
#                             totaln = NA)
# 
# 
# for (item in 1:nrow(query_strings)){
#   currentsearch = query_strings$search[item]
#   currentinstitution = query_strings$institution[item]
#   
#   cat("Currently pulling", currentinstitution, "- initial pull \n")
#   
#   initialpull <- cr_works(flq = c(`query.affiliation` = currentsearch), filter = c(type='dataset', from_pub_date='2012-01-01'), limit=1, .progress = TRUE)
#   
#   #write out how many results
#   cat("Currently pulling", currentinstitution, "- N results = ", initialpull$meta$total_results, "\n")
#   
#   #write total items found to query strings
#   query_strings$totaln[item] <- initialpull$meta$total_results
#   
#   #pull with all the results
#   currentpull <- cr_works(flq = c(`query.affiliation` = 'University of Minnesota'),  cursor = "*", .progress = TRUE, filter = c(type='dataset', from_pub_date='2012-01-01'))
#   
#   assign(paste(currentinstitution, 1,  sep = "_"), currentpull)
#   
#   save(list=c(ls()[grep(currentinstitution, ls())]), file = paste0("data_rdata_files/CrossRefPull_", currentinstitution,"_", gsub("-", "", Sys.Date()), ".Rdata"))
#   
#   
# }
# 
# write.csv(query_strings, file="Query_totals.csv", row.names=F)

## STEP 2: Flatten files
#Load in the files if not pulling new data
filestoload <- list.files("data_rdata_files/")[grep("CrossRef", list.files("data_rdata_files/"))]
for (i in filestoload) {
  load(paste0("data_rdata_files/", i))
}




# BY PUBLISHER #######
## Because Duke was the only IR that was using CrossRef to issue DOIs, only Duke publisher data was pulled. The rest used Datacite
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
