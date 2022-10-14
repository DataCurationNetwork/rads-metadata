#################################################
## UMN RADS Metadata analysis
##
## 2022-06-17
#################################################

#packages
pacman::p_load(jsonlite, 
               rjson,
               dplyr, 
               tidyr, 
               ggplot2, 
               rdatacite, 
               cowplot)


#set working directory
setwd("~/Documents/NSF_RADS/Metadata_Analysis/")


## Using the R DataCite package #####

#using the same query as Ted: https://docs.google.com/document/d/1pjquNgfh9_r6V9kAtHUhi288meXGR78o2j1FgBPY8W4/edit

#UNCOMMENT TO REPULL RESULTS
# umnresults_initial <- dc_dois(query = "creators.affiliation.name:*University*of*Minnesota*", limit=1000)
# 
# umnresults_initial$meta$totalPages
# 
# #cursor has no limitations (but this wasn't working for me)
# nextcursor <- umnresults_initial$links$`next`
# pull = 2
# 
# #This uses the pages, limit of 10,000
# for (i in 2:umnresults_initial$meta$totalPages) {
#   cat("Page = ", i, "\n")
#   temp <- dc_dois(query = "creators.affiliation.name:*University*of*Minnesota*", limit=1000, page = i)
#   assign(x=paste("umnresults", i, sep = "_"), temp)
# }
# 
# save(umnresults_initial, umnresults_2, umnresults_3, umnresults_4, umnresults_5, file = "DataCitePull_UMN_20220617.Rdata")
# 
# # load("DataCitePull_UMN_20220617.Rdata")
# 
# ## Pull out relevant things in a dataset
# #https://tidyr.tidyverse.org/reference/hoist.html
# 
# umn_1 <- umnresults_initial$data %>%
#   unnest_wider(attributes, simplify = T) %>%
#   unnest_wider(relationships) %>%
#   unnest_wider(types) %>%
#   unnest_wider(client) %>%
#   unnest_wider(data, names_sep = "_")
# 
# umn_2 <- umnresults_2$data %>%
#   unnest_wider(attributes, simplify = T) %>%
#   unnest_wider(relationships) %>%
#   unnest_wider(types) %>%
#   unnest_wider(client) %>%
#   unnest_wider(data, names_sep = "_")
# 
# 
# umn_3 <- umnresults_3$data %>%
#   unnest_wider(attributes, simplify = T) %>%
#   unnest_wider(relationships) %>%
#   unnest_wider(types) %>%
#   unnest_wider(client) %>%
#   unnest_wider(data, names_sep = "_")
# 
# umn_4 <- umnresults_4$data %>%
#   unnest_wider(attributes, simplify = T) %>%
#   unnest_wider(relationships) %>%
#   unnest_wider(types) %>%
#   unnest_wider(client) %>%
#   unnest_wider(data, names_sep = "_")
# 
# umn_5 <- umnresults_5$data %>%
#   unnest_wider(attributes, simplify = T) %>%
#   unnest_wider(relationships) %>%
#   unnest_wider(types) %>%
#   unnest_wider(client) %>%
#   unnest_wider(data, names_sep = "_")
# 
# 
# #Put them all together
# umn <- bind_rows(umn_1, umn_2, umn_3, umn_4, umn_5)
# 
# #write this out
# save(umn, file= "UMN_datacite_metadata_20220617.Rdata")

#If new data is not needed, just run this to load old pull
load(file="UMN_datacite_metadata_20220617.Rdata")

#DRUM is inconsistently specified (with and without DRUM)
umn$publisher[grep("Data Repository for the University of Minnesota", umn$publisher)] <- "Data Repository for the University of Minnesota (DRUM)"

### Explore this data

#original data?
umn_by_publisher_all <- umn %>% 
  group_by(publisher) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 

umn %>% 
  group_by(resourceTypeGeneral) %>% 
  summarize(count=n())

umn %>% 
  group_by(resourceTypeGeneral, publisher) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  slice_head(n=15) 


#only datasets
umn_data <- umn %>% 
  filter(resourceTypeGeneral == "Dataset") %>% 
  unnest_wider(container, names_sep = "_") 

umn_by_publisher_data <- umn_data %>% 
  group_by(publisher) %>% 
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