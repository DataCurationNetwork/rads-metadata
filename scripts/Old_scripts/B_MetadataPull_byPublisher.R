#################################################
## RADS Metadata Pull by Publisher
##
## 2022-07-14
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


#set working directory
setwd("~/Documents/NSF_RADS/Metadata_Analysis/")


## Pull data using the R DataCite package  #####

#using a query on publisher

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

  save(list=c(ls()[grep(currentinstitution, ls())]), file = paste0("DataCitePull_", currentinstitution,"_", gsub("-", "", Sys.Date()), ".Rdata"))


}

write.csv(query_strings, file="Query_totals_publisher.csv", row.names=F)

## STEP 2: Reformat pulled data into a flat file
## Reformat data into flat data files ##################
#Load in saved files if not repulling
filestoload <- list.files()[grep("20220715", list.files())]
for (i in filestoload) {
  load(i)
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
save(all_dois, umn, cornell, duke, michigan, vt, washu, file = "Combined_datacite_publisher_metadata.Rdata")


# Explore data ###################
#If new data is not needed, just run this to load old pull
load(file="Combined_datacite_publisher_metadata.Rdata")

publisher_by_institution <- all_dois %>% 
  group_by(institution, publisher) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count))

write.csv(publisher_by_institution, file="DataCite_PublisherSearch_byInstitution.csv", row.names = T)


publisher_by_institution_year <- all_dois %>% 
  group_by(institution, publisher, publicationYear) %>% 
  summarize(count=n()) %>% 
  arrange(institution, publicationYear, desc(count))

write.csv(publisher_by_institution_year, file="DataCite_PublisherSearch_byInstitution_Year.csv", row.names = T)

#Repository name cleaning
#START HERE - will need a lot of this...

#DRUM is inconsistently specified (with and without DRUM)
all_dois$publisher[grep("Data Repository for the University of Minnesota", all_dois$publisher)] <- "Data Repository for the University of Minnesota (DRUM)"




