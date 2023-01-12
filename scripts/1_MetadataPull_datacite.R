#################################################
## RADS Metadata Pull - Datacite
##
## Updated 2022-10-12
#################################################

#clear workspace
rm(list=ls())

#packages
pacman::p_load(dplyr, 
               tidyr, 
               ggplot2, 
               rjson,
               rdatacite,
               cowplot, 
               stringr)

# BY AFFILIATION ########
## Pull data using the R DataCite package  #####

#using the same query as Ted: https://docs.google.com/document/d/1pjquNgfh9_r6V9kAtHUhi288meXGR78o2j1FgBPY8W4/edit

#UNCOMMENT TO REPULL RESULTS
# #STEP 1: Pull data from datacite 
## Try 2: Pull complete data for all institutions using the cursor pagination
# query_strings <- data.frame(search = c("creators.affiliation.name:*University*of*Minnesota*",
#                                            "creators.affiliation.name:*Cornell*University*",
#                                            "creators.affiliation.name:*Duke*University*",
#                                            "creators.affiliation.name:*University*of*Michigan*",
#                                            "creators.affiliation.name:*Washington*University*in*St.*Louis",
#                                            "creators.affiliation.name:*Virginia*Tech*"),
#                             institution = c("UMN", "Cornell", "Duke", "Michigan", "WashU", "VT"),
#                             totaln = NA)
# 
# 
# #https://api.test.datacite.org/providers/caltech/dois?page[cursor]=1&page[size]=1000
# 
# for (item in 1:nrow(query_strings)){
#   currentsearch = query_strings$search[item]
#   currentinstitution = query_strings$institution[item]
#   initialpull <- dc_dois(query = currentsearch, cursor = 1, limit=1000)
# 
#   #write total items found to query strings
#   query_strings$totaln[item] <- initialpull$meta$total
# 
#   #cursor has no limitations
#   nextcursor <- gsub("^.+?=|&.+$", "", initialpull$links$`next`)
# 
#   #keep run for the rest of the pages
#   totalpages <- initialpull$meta$totalPages
# 
#   for (i in 2:totalpages) {
#     cat(currentinstitution,  "Pull = ", i, "\n")
#     temp <- dc_dois(query = currentsearch, cursor = nextcursor, limit=1000)
#     assign(x=paste(currentinstitution, i, sep = "_"), temp)
#     nextcursor <- gsub("^.+?=|&.+$", "", temp$links$`next`)
#   }
# 
#   assign(paste(currentinstitution, 1,  sep = "_"), initialpull)
# 
#   save(list=c(ls()[grep(currentinstitution, ls())]), file = paste0("data_rdata_files/DataCitePull_", currentinstitution,"_", gsub("-", "", Sys.Date()), ".Rdata"))
# 
# 
# }
# 
# write.csv(query_strings, file="Query_totals.csv", row.names=F)

## STEP 2: Reformat pulled data into a flat file
## Reformat data into flat data files ##################
#Load in saved files if not repulling
# filestoload <- list.files("data_rdata_files/")[grep("20221012", list.files("data_rdata_files/"))]
# for (i in filestoload) {
#   load(paste0("data_rdata_files/", i))
# }
# 
# ## Pull out relevant things in a dataset - unlist the returned data
# #https://tidyr.tidyverse.org/reference/hoist.html
# 
# #for (i in ls()[grep("_Dataset|_Software", ls())]) {
# for (i in ls()[grep("[[:digit:]]+", ls())]) {
# 
#   temp <- get(i)
# 
#   tempdat <- temp$data %>%
#     unnest_wider(attributes, simplify = T) %>%
#     unnest_wider(relationships) %>%
#     unnest_wider(types) %>%
#     unnest_wider(client) %>%
#     unnest_wider(data, names_sep = "_") %>%
#     unnest_wider(container, names_sep = "_")
# 
#   assign(i, tempdat)
# 
# }
# 
# #Put them all together - all types
# umn <- bind_rows(lapply(grep("UMN_[[:digit:]]+", ls(), value=T), get))
# cornell <- bind_rows(lapply(grep("Cornell_[[:digit:]]+", ls(), value=T), get))
# duke <- bind_rows(lapply(grep("Duke_[[:digit:]]+", ls(), value=T), get))
# michigan <- bind_rows(lapply(grep("Michigan_[[:digit:]]+", ls(), value=T), get))
# vt <- bind_rows(lapply(grep("VT_[[:digit:]]+", ls(), value=T), get))
# washu <- bind_rows(lapply(grep("WashU_[[:digit:]]+", ls(), value=T), get))
# 
# 
# #Bind them all together...
# all_dois <- bind_rows(list("Minnesota" = umn, "Cornell" = cornell, "Duke" = duke, "Michigan" = michigan, "Virginia Tech" = vt, "Washington U" = washu), .id = "institution")
# 
# #write out combined files
# save(all_dois, umn, cornell, duke, michigan, vt, washu, file = "data_rdata_files/Combined_datacite_metadata.Rdata")

## COUNT OF DATACITE AFFILIATION
load(file = "data_rdata_files/Combined_datacite_metadata.Rdata")
dim(all_dois)

#check affiliation
affiliation <- lapply(all_dois$creators, function(x) x[[1]]["affiliation"])
names(affiliation) <- all_dois$institution

affiliation1 <- unlist(lapply(affiliation, unlist), use.names = T)

# reduce by >= 2012 and dataset or software types
all_dois_12 <- all_dois %>% 
  filter(publicationYear >= 2012) 
dim(all_dois_12)

all_dois_ds12 <- all_dois_12 %>% 
  filter(resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software")

dim(all_dois_ds12)
save(all_dois_ds12, file="data_rdata_files/Combined_datacite_metadata_yeartypefilter.Rdata")


# BY PUBLISHER - Get Institutional IR Data ########
#using a query on publisher

#remove any objects in workspace
rm(list=ls())

#UNCOMMENT TO REPULL RESULTS
# #STEP 1: Pull data from datacite
## Try 2: Pull complete data for all institutions using the cursor pagination
query_strings <- data.frame(search = c("publisher:Data Repository for the University of Minnesota",
                                       "publisher:Cornell University Library",
                                       "publisher:Duke",
                                       "publisher:University of Michigan",
                                       "publisher:Washington University in St Louis", "publisher:Virginia Tech"),
                            institution = c("UMN", "Cornell", "Duke", "Michigan", "WashU", "VT"),
                            totaln = NA)


#https://api.test.datacite.org/providers/caltech/dois?page[cursor]=1&page[size]=1000

for (item in 1:nrow(query_strings)){
  currentsearch = query_strings$search[item]
  currentinstitution = query_strings$institution[item]
  cat(currentinstitution,  "Pull = ", 1, "\n")
  
  initialpull <- dc_dois(query = currentsearch, cursor = 1, limit=1000)
  
  
  
  #write total items found to query strings
  query_strings$totaln[item] <- initialpull$meta$total
  
  #cursor has no limitations
  nextcursor <- gsub("^.+?=|&.+$", "", initialpull$links$`next`)
  
  #keep run for the rest of the pages
  totalpages <- initialpull$meta$totalPages
  
  if (totalpages > 1) {
    for (i in 2:totalpages) {
      cat(currentinstitution,  "Pull = ", i, "\n")
      temp <- dc_dois(query = currentsearch, cursor = nextcursor, limit=1000)
      assign(x=paste(currentinstitution, i, sep = "_"), temp)
      nextcursor <- gsub("^.+?=|&.+$", "", temp$links$`next`)
    }
  }
  
  assign(paste(currentinstitution, 1,  sep = "_"), initialpull)
  
  save(list=c(ls()[grep(currentinstitution, ls())]), file = paste0("data_rdata_files/DataCitePull_byPublisher_", currentinstitution,"_", gsub("-", "", Sys.Date()), ".Rdata"))
  
  
}

write.csv(query_strings, file="Query_totals_publisher.csv", row.names=F)

## STEP 2: Reformat pulled data into a flat file
## Reformat data into flat data files ##################
#Load in saved files if not repulling
filestoload <- list.files("data_rdata_files/")[grep("byPublisher", list.files("data_rdata_files/"))][grep("20221012", list.files("data_rdata_files/")[grep("byPublisher", list.files("data_rdata_files/"))])]
for (i in filestoload) {
  load(paste0("data_rdata_files/", i))
}

## Pull out relevant things in a dataset - unlist the returned data
#https://tidyr.tidyverse.org/reference/hoist.html

#for (i in ls()[grep("_Dataset|_Software", ls())]) {
for (i in ls()[grep("[[:digit:]]+", ls())]) {
  
  temp <- get(i)
  
  tempdat <- temp$data %>%
    unnest_wider(attributes, simplify = T) %>%
    unnest_wider(relationships) %>%
    unnest_wider(types) %>%
    unnest_wider(client) %>%
    unnest_wider(data, names_sep = "_") %>%
    unnest_wider(container, names_sep = "_")
  
  assign(i, tempdat)
  
}

#Put them all together - all types
umn <- bind_rows(lapply(grep("UMN_[[:digit:]]+", ls(), value=T), get))
cornell <- bind_rows(lapply(grep("Cornell_[[:digit:]]+", ls(), value=T), get))
duke <- bind_rows(lapply(grep("Duke_[[:digit:]]+", ls(), value=T), get))
michigan <- bind_rows(lapply(grep("Michigan_[[:digit:]]+", ls(), value=T), get))
vt <- bind_rows(lapply(grep("VT_[[:digit:]]+", ls(), value=T), get))
washu <- bind_rows(lapply(grep("WashU_[[:digit:]]+", ls(), value=T), get))


#Bind them all together...
all_dois <- bind_rows(list("Minnesota" = umn, "Cornell" = cornell, "Duke" = duke, "Michigan" = michigan, "Virginia Tech" = vt, "Washington U" = washu), .id = "institution")

#write out combined files
save(all_dois, umn, cornell, duke, michigan, vt, washu, file = "data_rdata_files/Combined_datacite_publisher_metadata.Rdata")

## COUNT OF PUBLISHER - DATACITE
load("data_rdata_files/Combined_datacite_publisher_metadata.Rdata")

#all dois
dim(all_dois)

#remove duke
all_dois %>% 
  filter(institution != "Duke") %>% 
  dim
#18985


# Explore data ###################
#If new data is not needed, just run this to load old pull
load(file="data_rdata_files/Combined_datacite_publisher_metadata.Rdata")

publisher_by_institution <- all_dois %>% 
  group_by(institution, publisher) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count))

write.csv(publisher_by_institution, file="data_summary_data/DataCite_PublisherSearch_byInstitution.csv", row.names = T)


publisher_by_institution_year <- all_dois %>% 
  group_by(institution, publisher, publicationYear) %>% 
  summarize(count=n()) %>% 
  arrange(institution, publicationYear, desc(count))

write.csv(publisher_by_institution_year, file="data_summary_data/DataCite_PublisherSearch_byInstitution_Year.csv", row.names = T)

