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

pacman::p_load_current_gh("ropensci/rcrossref")



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
load("data_rdata_files/Filtered_CrossRef_Data_VT.Rdata")

alldat <- c(alldat, datwewant)

#for each entry, add in which Institution the affiliation is from
affiliations <- lapply(alldat, function(x) grep("University of Minnesota|University of Michigan|Cornell University|Washington University|Duke University|Virginia Tech|Virginia Poly", unlist(lapply(x$author, function(y) y$affiliation)), value = T))

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

## COUNT OF CROSS REF AFFILIATION
load("data_rdata_files/Combined_Filtered_Crossref_Data_Dataset.Rdata")
dim(alldat_dat)

## STEP 3: Filter out other affiliations

load("data_rdata_files/Combined_Filtered_Crossref_Data_Dataset.Rdata")

#trim white space on all variables
alldat_dat <- alldat_dat %>% 
  mutate(across(.cols=starts_with("name"), .fns = trimws))

#Condense the name variables
alldat_dat$name[which(is.na(alldat_dat$name))] 
for (i in which(is.na(alldat_dat$name))) {
  #if all non-missing affiliations are the same, put that in name
  nonmissingnames <- alldat_dat[i, intersect(grep("name", names(alldat_dat)), which(!is.na(alldat_dat[i,])))]
  if (length(unique(unlist(nonmissingnames))) == 1) {
    alldat_dat$name[i] <- unlist(nonmissingnames)[1]
  }
  else if (any(nonmissingnames == "University of Minnesota")) {
      alldat_dat$name[i] <- "University of Minnesota"
  }
  else if (any(nonmissingnames == "University of Michigan" |
               nonmissingnames == "University of Michigan Medical School")) {
    alldat_dat$name[i] <- "University of Michigan"
  }
  else if (any(nonmissingnames == "Cornell University")) {
    alldat_dat$name[i] <- "Cornell University"
  }
  else if (any(nonmissingnames == "Duke University" |
               nonmissingnames == "Duke University School of Medicine")) {
    alldat_dat$name[i] <- "Duke University"
  }
  else if (any(grepl("St Louis|Saint Louis", nonmissingnames))) {
    alldat_dat$name[i] <- "Washington University in St Louis"
  }
 
  }
    

#condense UMinn
alldat_dat$name <- trimws(alldat_dat$name)
table(grep("University of Minnesota", alldat_dat$name, value=T))

alldat_dat$name[which(alldat_dat$name == "University of Minnesota - Twin Cities")] <- "University of Minnesota"
alldat_dat$name[which(alldat_dat$name == "University of Minnesota Twin Cities")] <- "University of Minnesota"
alldat_dat$name[which(alldat_dat$name == "University of Minnesota Center for Forest Ecology")] <- "University of Minnesota"
alldat_dat$name[which(alldat_dat$name == "University of Minnesota College of Biological Sciences")] <- "University of Minnesota"
alldat_dat$name[which(alldat_dat$name == "University of Minnesota Medical Center")] <- "University of Minnesota"
alldat_dat$name[which(alldat_dat$name == "University of Minnesota Medical School")] <- "University of Minnesota"

#condense UMich
table(grep("University of Michigan", alldat_dat$name, value=T))
alldat_dat$name[which(alldat_dat$name == "University of Michigan Ann Arbor")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "The University of Michigan Medical School")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan Medical Center")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan Medical School")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan School of Public Health")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan School of Dentistry")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan School of Medicine")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan WK Kellogg Eye Center")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan Comprehensive Cancer Center")] <- "University of Michigan"
alldat_dat$name[which(alldat_dat$name == "University of Michigan Health System")] <- "University of Michigan"

#condense Cornell
table(grep("Cornell University", alldat_dat$name, value=T))
alldat_dat$name[which(alldat_dat$name == "Cornell University College of Agriculture & Life Sciences")] <- "Cornell University"
alldat_dat$name[which(alldat_dat$name == "Cornell University College of Agriculture and Life Sciences")] <- "Cornell University"

#Duke University 
table(grep("Duke University", alldat_dat$name, value=T))
alldat_dat$name[which(alldat_dat$name == "Duke University Medical Center")] <- "Duke University"
alldat_dat$name[which(alldat_dat$name == "Duke University School of Medicine")] <- "Duke University"

#Virgina Tech
table(grep("Virginia", alldat_dat$name, value=T))

#Washington University
table(grep("Washington University", alldat_dat$name, value=T))
alldat_dat$name[which(alldat_dat$name == "Washington University in St. Louis")] <- "Washington University in St Louis"
alldat_dat$name[which(alldat_dat$name == "Washington University at St Louis")] <- "Washington University in St Louis"
alldat_dat$name[which(alldat_dat$name == "Washington University at St. Louis")] <- "Washington University in St Louis"
alldat_dat$name[which(alldat_dat$name == "Washington University in Saint Louis")] <- "Washington University in St Louis"
alldat_dat$name[grep("Louis", alldat_dat$name)] <- "Washington University in St Louis"


table(alldat_dat$name)

#which are still missing?
filter(alldat_dat, is.na(name)) %>% View()
  #seems to not be relevant U's

#start here
alldat_filtered <- alldat_dat %>% 
  filter(name %in% c("Cornell University", "Duke University", "University of Michigan", "University of Minnesota", "Washington University in St Louis", "Virginia Polytechnic Institute and State University")) %>% 
  select(-starts_with("name.."))

#save as R data and CSV
save(alldat_filtered, file="data_rdata_files/Combined_Filtered_Crossref_Data_ForAnalysis.Rdata")

write.csv(alldat_filtered, file="data_data_dois/Data_dois_Crossref_20221022.csv", row.names=F)

## COUNT OF FILTERED CROSS REF AFFILIATION
#filting name
load("data_rdata_files/Combined_Filtered_Crossref_Data_ForAnalysis.Rdata")
dim(alldat_filtered)

## Data Exploration
instpub <- alldat_filtered %>% 
  group_by(publisher) %>% 
  summarize(count=n())

alldat_filtered %>% 
  group_by(publisher) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 

alldat_filtered %>% 
  group_by(publisher, name, `created.date-parts1`) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  write.csv("data_summary_data/Counts of CrossRef Publishers by Institution and Year.csv", row.names = F)

Umich<- alldat_filtered %>% 
  filter(name == "University of Michigan")

# #Code for testing
# #year
# temp$items[[1]]$created$`date-parts`[[1]][[1]][1]
# #type
# temp$items[[1]]$type
# #author affiliation
# unlist(lapply(datasetsover2012$items[[1]]$author, function(x) x$affiliation))



# BY PUBLISHER #######
#Use the API to pull publisher data
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

save(dukedat, file="data_rdata_files/CrossRefPull_Duke_Publisher.Rdata")
# load("data_rdata_files/CrossRefPull_Duke_Publisher.Rdata")
# dim(dukedat) #this must have gotten saved in the middle of pulling - need to read in the CSV



#write out data, removing author column (nested list)
dukedat %>% 
  select(-author) %>% 
  write.csv(file="Duke_CrossrefDOIs_20221113.csv", row.names = F)

dukedat <- read.csv("data_all_dois/Duke_CrossrefDOIs_20221113.csv")
dim(dukedat)

dukedat %>% 
  group_by(type, container.title, publisher) %>% 
  summarize(count=n()) %>% 
  knitr::kable()

summary(duplicated(dukedat$doi))
