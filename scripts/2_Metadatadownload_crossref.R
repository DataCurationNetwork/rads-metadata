#########################
## Examine Crossref
##
#########################
## Data before April 2022 Crossref was downloaded via Academic Torrent from 
## https://academictorrents.com/details/4dcfdf804775f2d92b7a030305fa0350ebef6f3e on a server and then searched data files from > 2012, that are datasets, and have affiliation with one of the colleges. 
#########################

#clear workspace
rm(list=ls())

#required packages
pacman::p_load(ggplot2, 
               dplyr, 
               tidyr, 
               jsonlite, 
               readr)


# setwd("/Volumes/hofelich/Documents/NSF_RADS/Metadata_Pull/")
# setwd("Documents/NSF_RADS/Metadata_Pull/")

## STEP 1: Parse and search file 
# datwewant <- list()
# 
# files <- list.files()
# 
# ## Left off on i = 18270 - 20221018
# ## Left off on i = 22208 - 20221018  10am
# 
# #This is an embarrassingly parallel problem, but I will do this in a serial loop with only a moderate amount of shame. 
# for (i in 24940:length(files)){
#   cat("On", i, "of", length(files), "files \n")
#   
#   temp <-read_json(files[i])
#   
#   #first take ones that are over 2012
#   whichover2012 <- lapply(temp$items, function(x) x$created$`date-parts`[[1]][[1]][1]) >= 2012
#   
#   over2012 <- temp$items[whichover2012]
#   
#   if (length(over2012) > 0) {
#     #then subset by type is dataset
#     whichdata <- lapply(over2012, function(x) x$type) == "dataset"
#     
#     datasetsover2012 <- over2012[whichdata]
#     
#     if (length(datasetsover2012) > 0) {
#       
#       whichaffiliated <- grepl("University of Minnesota|University of Michigan|Cornell University|Washington University|Duke University", lapply(datasetsover2012, function(x) x$author))
#       
#       affiliated <- datasetsover2012[whichaffiliated]
#       
#       datwewant <- c(datwewant, affiliated)
#     
#   } 
#   
#     
#   }
#   
# }
# 
# save(datwewant, file="Filtered_CrossRef_Data_round3.Rdata")

## STEP 2, combine files
# #load in the data
# load(file = "Filtered_CrossRef_Data_round3.Rdata")
# datwewantr3 <- datwewant
# load("Filtered_CrossRef_Data_round2.Rdata")
# datwewantr2 <- datwewant
# load("Filtered_CrossRef_Data.Rdata")
# 
# alldat <- c(datwewant, datwewantr2, datwewantr3)
# 
# save(alldat, file="Combined_Filtered_Crossref_Data.Rdata")


## STEP 3: Flatten the file
load("data_rdata_files/Combined_Filtered_Crossref_Data.Rdata")

#for each entry, add in which Institution the affiliation is from
affiliations <- lapply(alldat, function(x) grep("University of Minnesota|University of Michigan|Cornell University|Washington University|Duke University", unlist(lapply(x$author, function(y) y$affiliation)), value = T))

#split on commas
affiliations1 <- lapply(affiliations, function(x) strsplit(x, ","))

#get just the university
affiliations2 <- lapply(affiliations1, function(a) unlist(lapply(a, function(x) grep("University", x, value=T))))

#select relevant columns and unlist
alldat1 <- lapply(alldat, function(x) unlist(x[c("URL", "DOI", "member", "reference-count", "publisher", "type", "title", "created")]))

names(alldat1) <- 1:length(alldat1)
names(affiliations2) <- 1:length(affiliations2)

#merge the lists
alldat2 <- mapply(c, alldat1, affiliations2)

#put into dataframe
alldat_dat <- bind_rows(alldat2)

save(alldat_dat, file = "data_rdata_files/Combined_Filtered_Crossref_Data_Dataset.Rdata")

# #Code for testing
# #year
# temp$items[[1]]$created$`date-parts`[[1]][[1]][1]
# #type
# temp$items[[1]]$type
# #author affiliation
# unlist(lapply(datasetsover2012$items[[1]]$author, function(x) x$affiliation))
