#################################################
## RADS Metadata analysis
##
## 2022-06-17
#################################################

#clear workspace
rm(list=ls())

#packages
pacman::p_load(dplyr, 
               tidyr, 
               ggplot2, 
               rdatacite,
               cowplot)


#set working directory
setwd("~/Documents/NSF_RADS/Metadata_Analysis/")


## Using the R DataCite package #####

#using the same query as Ted: https://docs.google.com/document/d/1pjquNgfh9_r6V9kAtHUhi288meXGR78o2j1FgBPY8W4/edit

#UNCOMMENT TO REPULL RESULTS
#Set up the queries for each university
# query_strings <- data.frame(search = rep(c("creators.affiliation.name:*University*of*Minnesota*",
#                    "creators.affiliation.name:*Cornell*University*",
#                    "creators.affiliation.name:*Duke*University*",
#                    "creators.affiliation.name:*University*of*Michigan*",
#                    "creators.affiliation.name:*Washington*University*in*St.*Louis",
#                    "creators.affiliation.name:*Virginia*Tech*"),3),
#                    institution = rep(c("UMN", "Cornell", "Duke", "Michigan", "WashU", "VT"),3),
#                    resourcetype = rep(c("Dataset", "Software", ""),each=6),
#                    totaln = NA)
# 
# for (item in 1:nrow(query_strings)) {
#   currentsearch <- query_strings$search[item]
#   currentinst <- query_strings$institution[item]
#   resource = query_strings$resourcetype[item]
# 
#   if (resource != "") {
#     initresults <- dc_dois(query = currentsearch, resource_type_id = resource, limit=1000)
# 
#     #get total number of results from the metadata
#     query_strings$totaln[item] <- initresults$meta$total
# 
#     if (initresults$meta$totalPages > 1) {
#       #This uses the pages, limit of 10,000
#       for (i in 2:initresults$meta$totalPages) {
#         cat("Page = ", i, currentinst, resource, "\n")
#         temp <- dc_dois(query = currentsearch,resource_type_id = resource, limit=1000, page = i)
#         assign(x=paste(currentinst, i, resource, sep = "_"), temp)
#       }
#     }
# 
#     assign(paste(currentinst, 1, resource, sep = "_"), initresults)
# 
#     save(list=c(ls()[grep(currentinst, ls())]), file = paste0("DataCitePull_", currentinst,"_",resource, "_", gsub("-", "", Sys.Date()), ".Rdata"))
#   } else {
# 
#     initresults <- dc_dois(query = currentsearch, limit=1000)
# 
#     #get total number of results from the metadata
#     query_strings$totaln[item] <- initresults$meta$total
# 
#     if (initresults$meta$totalPages > 1) {
#       #This uses the pages, limit of 10,000
#       for (i in 2:initresults$meta$totalPages) {
#         cat("Page = ", i, currentinst, resource, "\n")
#         temp <- dc_dois(query = currentsearch, limit=1000, page = i)
#         assign(x=paste(currentinst, i, resource, sep = "_"), temp)
#       }
#     }
# 
#     assign(paste(currentinst, 1,  sep = "_"), initresults)
# 
#     save(list=c(ls()[grep(currentinst, ls())]), file = paste0("DataCitePull_", currentinst,"_", gsub("-", "", Sys.Date()), ".Rdata"))
# 
#   }
# }



## Try 2: Pull complete data for all institutions using the cursor pagination
query_strings <- data.frame(search = c("creators.affiliation.name:*University*of*Minnesota*",
                                           "creators.affiliation.name:*Cornell*University*",
                                           "creators.affiliation.name:*Duke*University*",
                                           "creators.affiliation.name:*University*of*Michigan*",
                                           "creators.affiliation.name:*Washington*University*in*St.*Louis",
                                           "creators.affiliation.name:*Virginia*Tech*"),
                            institution = c("UMN", "Cornell", "Duke", "Michigan", "WashU", "VT"),
                            totaln = NA)


#https://api.test.datacite.org/providers/caltech/dois?page[cursor]=1&page[size]=1000

for (item in 1:nrow(query_strings)){
  currentsearch = query_strings$search[item]
  currentinstitution = query_strings$institution[item]
  initialpull <- dc_dois(query = currentsearch, cursor = 1, limit=1000)
  
  #write total items found to query strings
  query_strings$totaln[item] <- initialpull$meta$total
  
  #cursor has no limitations 
  nextcursor <- gsub("^.+?=|&.+$", "", initialpull$links$`next`)
  
  #keep run for the rest of the pages
  totalpages <- initialpull$meta$totalPages
  
  for (i in 2:totalpages) {
    cat(currentinstitution,  "Pull = ", i, "\n")
    temp <- dc_dois(query = currentsearch, cursor = nextcursor, limit=1000)
    assign(x=paste(currentinstitution, pull, sep = "_"), temp)
    nextcursor <- gsub("^.+?=|&.+$", "", temp$links$`next`)
  }
  
  assign(paste(currentinstitution, 1,  sep = "_"), initialpull)
  
  save(list=c(ls()[grep(currentinstitution, ls())]), file = paste0("DataCitePull_", currentinstitution,"_", gsub("-", "", Sys.Date()), ".Rdata"))
  
  
}



## Pull U of Mich #########################
## Need to repull Michigan's as they have > 10,000
currentsearch = "creators.affiliation.name:*University*of*Michigan*"

umich_initial <- dc_dois(query = currentsearch, cursor = 1)

umich_initial$meta$totalPages

#cursor has no limitations (but this wasn't working for me)
nextcursor <- gsub("^.+?=|&.+$", "", umich_initial$links$`next`)

pull = 2

while (!is.null(nextcursor)) {
  cat("Pull = ", pull, "\n")
  temp <- dc_dois(query = currentsearch, cursor = nextcursor)
  assign(x=paste("Michigan_cursor_", pull, sep = "_"), temp)
  nextcursor <- gsub("^.+?=|&.+$", "", temp$links$`next`)
  pull = pull + 1
}

save(list=c(ls()[grep("Michigan_cursor", ls())]), file = paste0("DataCitePull_", "Michigan_cursor","_", gsub("-", "", Sys.Date()), ".Rdata"))



# # load("DataCitePull_UMN_20220617.Rdata")
filestoload <- list.files()[grep("20220623", list.files())]

for (i in filestoload) {
  load(i)
}


## Pull out relevant things in a dataset
#https://tidyr.tidyverse.org/reference/hoist.html

#for (i in ls()[grep("_Dataset|_Software", ls())]) {
for (i in ls()[grep("[[:digit:]]+", ls())]) {

  temp <- get(i)

  tempdat <- temp$data %>%
    unnest_wider(attributes, simplify = T) %>%
    unnest_wider(relationships) %>%
    unnest_wider(types) %>%
    unnest_wider(client) %>%
    unnest_wider(data, names_sep = "_")

  assign(i, tempdat)

}


#Put them all together - all types
umn <- bind_rows(UMN_1, UMN_2_, UMN_3_, UMN_4_, UMN_5_)
cornell <- bind_rows(Cornell_1, Cornell_2_, Cornell_3_, Cornell_4_, Cornell_5_, Cornell_6_, Cornell_7_, Cornell_8_, Cornell_9_, Cornell_10_)
duke <- bind_rows(Duke_1, Duke_2_, Duke_3_, Duke_5_, Duke_6_)
michigan <- bind_rows(Michigan_1, Michigan_2_, Michigan_3_, Michigan_4_, Michigan_5_, Michigan_6_, Michigan_7_, Michigan_8_, Michigan_9_, Michigan_10_)
michigan_cursor <- bind_rows(lapply(grep("Michigan_cursor__[[:digit:]]+$", ls(), value=T), get))
vt <- bind_rows(lapply(grep("VT_[[:digit:]]$|VT_[[:digit:]]_$", ls(), value=T), get))
washu <- bind_rows(lapply(grep("WashU_[[:digit:]]_$|WashU_[[:digit:]]$", ls(), value=T), get))

umn_data <- bind_rows(lapply(grep("UMN_[[:digit:]]+_Dataset", ls(), value=T), get))
cornell_data <- bind_rows(lapply(grep("Cornell_[[:digit:]]+_Dataset", ls(), value=T), get))
duke_data <- bind_rows(lapply(grep("Duke_[[:digit:]]+_Dataset", ls(), value=T), get))
michigan_data <- bind_rows(lapply(grep("Michigan_[[:digit:]]+_Dataset", ls(), value=T), get))
vt_data <- bind_rows(lapply(grep("VT_[[:digit:]]+_Dataset", ls(), value=T), get))
washu_data <- bind_rows(lapply(grep("WashU_[[:digit:]]+_Dataset", ls(), value=T), get))

umn_software <- bind_rows(lapply(grep("UMN_[[:digit:]]+_Software", ls(), value=T), get))
cornell_software <- bind_rows(lapply(grep("Cornell_[[:digit:]]+_Software", ls(), value=T), get))
duke_software <- bind_rows(lapply(grep("Duke_[[:digit:]]+_Software", ls(), value=T), get))
michigan_software <- bind_rows(lapply(grep("Michigan_[[:digit:]]+_Software", ls(), value=T), get))
vt_software <- bind_rows(lapply(grep("VT_[[:digit:]]+_Software", ls(), value=T), get))
washu_software <- bind_rows(lapply(grep("WashU_[[:digit:]]+_Software", ls(), value=T), get))

#Bind them all together...

dc_data <- bind_rows(list("Minnesota" = umn_data, "Cornell" = cornell_data, "Duke" = duke_data, "Michigan" = michigan_data, "Virginia Tech" = vt_data, "Washington U" = washu_data), .id = "institution")

dc_software <- bind_rows(list("Minnesota" = umn_software, "Cornell" = cornell_software, "Duke" = duke_software, "Michigan" = michigan_software, "Virginia Tech" = vt_software, "Washington U" = washu_software), .id = "institution")
dc_all <- bind_rows(list("Minnesota" = umn, "Cornell" = cornell, "Duke" = duke, "Michigan" = michigan, "Virginia Tech" = vt, "Washington U" = washu), .id = "institution")
dc_all_wcursor <- bind_rows(list("Minnesota" = umn, "Cornell" = cornell, "Duke" = duke, "Michigan" = michigan_cursor, "Virginia Tech" = vt, "Washington U" = washu), .id = "institution")

#write out combined files
save(dc_data, dc_software, dc_all,dc_all_wcursor, file = "Combined_datacite_metadata.Rdata")


#If new data is not needed, just run this to load old pull
load(file="Combined_datacite_metadata.Rdata")

dc_all_wcursor %>% 
  mutate(dupid = duplicated(id)) %>% 
  group_by(institution, dupid) %>% 
  summarize(count=n())

#DRUM is inconsistently specified (with and without DRUM)
dc_all_wcursor$publisher[grep("Data Repository for the University of Minnesota", dc_all_wcursor$publisher)] <- "Data Repository for the University of Minnesota (DRUM)"

### Explore this data

#original data?
by_publisher_all <- dc_all_wcursor %>% 
  group_by(institution, publisher) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 

dc_all_wcursor %>% 
  group_by(institution, resourceTypeGeneral) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count)) %>% 
  View()


#only datasets
dc_all_data <- dc_all_wcursor %>% 
  filter(resourceTypeGeneral == "Dataset") %>% 
  unnest_wider(container, names_sep = "_") 

by_publisher_data <- dc_all_data %>% 
  group_by(publisher, institution) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

#only software
umn_software <- umn %>% 
  filter(resourceTypeGeneral == "Software") 
  

umn_by_publisher_software <- umn_software %>% 
  group_by(publisher) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))



#unnest titles
titles <- lapply(umn_data$titles, function(x) x[[1]])
names(titles) <- umn_data$id

titles_df <- bind_rows(titles, .id="id")

umn_data_1 <- umn_data %>% 
  full_join(titles_df, by="id") %>% 
  relocate(title, .after = "id")

umn_data_1 %>% 
  group_by(publisher) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 

#Look at ones within the same container (e.g., files for same project)
umn_data_1 %>% 
  group_by(container_identifier) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  View()

#explore these a bit deeper: 
umn_data_1 %>% 
  filter(container_identifier== "doi:10.7910/DVN/3GOXDP") %>% 
  View()


# seems to be individual identifiers are applied to each file within a larger project. Dataverse and QDR both have this - larger projects with individual DOIs assigned to each file. Examples:
#Dataverse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3GOXDP - each file as DOI, multiple versions
#QDR: https://data.qdr.syr.edu/dataset.xhtml?persistentId=doi:10.5064/F6JW8BSD&version=1.0

#WAYS to DE DUP

# 0. make sure we don't have duplicate ids introduced by titles
umn_data_d0 <- umn_data_1 %>% 
  filter(!duplicated(id))

#starting from 
nrow(umn_data_d0)

# 1. Remove entires with duplicate non-NA container identifiers
#What if we de-duplicate the data to include cases with unique container identifiers? Keep NAs
umn_data_d1 <- umn_data_d0 %>% 
  filter(is.na(container_identifier) | !duplicated(container_identifier))

#rows affected
nrow(umn_data_d0) - nrow(umn_data_d1)

# 2. Remove entries with same DOI but different versions (keep most recent)
umn_data_d2 <- umn_data_d1 %>% 
  arrange(desc(id)) %>%
  filter(!duplicated(gsub("\\.v[[:digit:]]+$|\\.[[:digit:]]+$","", id))) 

#rows affected 
nrow(umn_data_d1) - nrow(umn_data_d2)

#new summary of data places
umn_data_d2 %>% 
  group_by(publisher) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  View()

#DRUM is inconsistently specified (with and without DRUM) - Added this above
#umn_data_d2$publisher[grep("Data Repository for the University of Minnesota", umn_data_d2$publisher)] <- "Data Repository for the University of Minnesota (DRUM)"

#re-do summary of data places
umn_by_publisher_dedup_data <- umn_data_d2 %>% 
  group_by(publisher) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 


#Plot the top 5 places  #####

umn_by_publisher_combinedtop5 <- umn_by_publisher_all %>% 
  bind_rows(umn_by_publisher_data, umn_by_publisher_dedup_data, .id="subset") %>% 
  mutate(subset = case_when(subset == 1 ~ "all dois", 
                            subset == 2 ~ "data dois", 
                            subset == 3 ~ "de-duplciated data dois"), 
         publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR", 
                               publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM", 
                               TRUE ~ publisher)) %>% 
  group_by(subset) %>% 
  slice_head(n=5) 

colors <- c("Zenodo" = "darkorange1",
            "Harvard Dataverse" = "dodgerblue2",
            "Dryad" = "lightgray", 
            "figshare" = "purple",
            "arXiv" = "red",
            "Neotoma Paleoecological Database" = "forestgreen",
            "Qualitative Data Repository" = "gold1",
            "ICPSR" = "darkcyan",
            "DRUM" = "pink")

(plot <- umn_by_publisher_combinedtop5 %>% 
    ggplot(aes(x=subset, y=count, fill=publisher)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_manual(values = colors) +
    guides(fill = guide_legend(title.position = "top")) +
    labs(x = "Data Subset Used", y="Count of DOIs") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title.align = .5))


ggsave(plot, file="University of Minnesota DataCite DOIs by Type and Publisher.png", device = "png")



#recreate the graphs by time with the de-duplicated datasets
(plot_byyear_dedup <- umn_data_d2 %>% 
  mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR", 
                               publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM", 
                               TRUE ~ publisher)) %>% 
  filter(publicationYear >= 2012, publisher %in% umn_by_publisher_combinedtop5$publisher) %>% 
  group_by(publicationYear, publisher) %>% 
  summarize(count=n()) %>% 
  mutate(publicationYear = factor(publicationYear)) %>% 
  ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values=colors) +
    labs(x="Year", y="Count of DOIs") +
    guides(fill = guide_legend(title.position = "top")) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title.align = .5))
  

ggsave(plot_byyear_dedup, filename = "University of Minnesota DataCite DOIs by Year and Publisher - Dedup.png", device="png")


#with DOIs as is
(plot_byyear <- umn_data_d0 %>% 
    mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR", 
                                 publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM", 
                                 TRUE ~ publisher)) %>% 
    filter(publicationYear >= 2012, publisher %in% umn_by_publisher_combinedtop5$publisher) %>% 
    group_by(publicationYear, publisher) %>% 
    summarize(count=n()) %>% 
    mutate(publicationYear = factor(publicationYear)) %>% 
    ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values=colors) +
    labs(x="Year", y="Count of DOIs", title="University of Minnesota", subtitle = "Shared Datasets by Year and Repository") +
    guides(fill = guide_legend(title.position = "top", nrow = 3)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title.align = .5))


ggsave(plot_byyear, filename = "University of Minnesota DataCite DOIs by Year and Publisher.png", device="png", width = 5.5, height = 4, units="in")


#Graph all the data and software locations

(pubdat <- umn_by_publisher_data %>% 
  slice_head(n=5) %>% 
  ggplot(aes(x=publisher, y=count)) +
  geom_bar(stat="identity", aes(fill=publisher)) +
  scale_fill_manual(values=colors[which(names(colors) %in% umn_by_publisher_data$publisher[1:5])]) +
  labs(x="Repository", y="Count of DOIs", title = "University of Minnesota Datasets by Repository - Top 5") +
  guides(fill = guide_legend(title.position = "top")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title.align = .5))

ggsave(pubdat, file="UMN Published Data by Repository.png", dev = "png")

(pubsoft <- umn_by_publisher_software %>% 
  slice_head(n=5) %>% 
  mutate(publisher = gsub("Data Repository for the University of Minnesota \\(DRUM\\)", "DRUM", publisher)) %>% 
  ggplot(aes(x=publisher, y=count)) +
  geom_bar(stat="identity", aes(fill=publisher)) +
  scale_fill_manual(values=c("Zenodo" = "darkorange1",
                             "DRUM" = "pink", 
                             "Code Ocean" = "darkblue", 
                             "CoMSES Net" = "purple", 
                             "University of Minnesota" = "darkgreen")) +
  labs(x="Repository", y="Count of DOIs", title = "University of Minnesota Software by Repository - Top 5") +
  guides(fill = guide_legend(title.position = "top")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title.align = .5))

ggsave(pubsoft, file="UMN Published Software by Repository.png", dev="png")

pub_both <- plot_grid(pubdat, pubsoft, nrow = 2)

ggsave(pub_both, filename = "UMN Published Data and Software.png", dev="png",  width = 10, height = 8, units="in")

#So some of these are individual files:https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/3GOXDP/MGFNG7 

#try to unlist the related identifier column so that entires related to each other can be not doubly counted

#try this the for loop way..
umn_relatedidentifiers <- umn_data %>% 
  select(id, relatedIdentifiers)

umn_relatedidentifiers$relatedIdentifiers <-  lapply(umn_relatedidentifiers$relatedIdentifiers, function(x) x[[1]])

identifierstopull <- list()

for (i in 1:length(umn_relatedidentifiers$relatedIdentifiers)) {
  if (ncol(umn_relatedidentifiers$relatedIdentifiers[[i]]) != 0) {
    identifierstopull[[i]] <- pivot_wider(umn_relatedidentifiers$relatedIdentifiers[[i]], 
                names_from = "relationType", 
                values_from = "relatedIdentifier") 
    #try to unlist any lists
    if (any(unlist(lapply(identifierstopull[[i]], is.list)))) {
      coltounlist = names(which(unlist(lapply(identifierstopull[[i]], is.list))))
      #unlist one variable at a time for multiple lists
      for (j in coltounlist) {
        identifierstopull[[i]] <- unnest_longer(identifierstopull[[i]], col= all_of(j))
      }
     
    }
  } else { identifierstopull[[i]] <- data.frame(relatedIdentiferType="")}
}

names(identifierstopull) <- umn_relatedidentifiers$id

identifierspulled <- bind_rows(lapply(identifierstopull, bind_rows), .id="id")

#merge back into larger dataset
umn_data_2 <- full_join(umn_data, identifierspulled, by="id")

#Now try to collapse the cases that are related to the same dois
partofsame <- umn_data_2 %>% 
  group_by(IsPartOf) %>% 
  summarize(count = n(), 
            uniqueIds = n_distinct(id)) 






# while (!is.null(nextcursor)) {
#   cat("Pull = ", pull, "\n")
#   temp <- dc_dois(query = "creators.affiliation.name:*University*of*Minnesota*", limit=1000, cursor = nextcursor)
#   assign(x=paste("umnresults", pull, sep = "_"), temp)
#   nextcursor <- temp$links$`next`
#   pull = pull + 1
# }





## Ted's data #############

umnjsonfiles <- list.files("University_of_Minnesota__20211119_13/json/") 

for (i in umnjsonfiles){
  temp <- fromJSON(file=paste0("University_of_Minnesota__20211119_13/json/", i))
  assign(x=gsub("\\.json", "", i), temp)
}