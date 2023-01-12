#################################################
## RADS Metadata - Combine IR/CrossRef/DataCite Data
##
## 2022-11-13
#################################################

#clear workspace
rm(list=ls())

#packages
pacman::p_load(dplyr, 
               tidyr, 
               ggplot2, 
               googlesheets4,
               rdatacite,
               cowplot, 
               stringr)

## Data by PUBLISHER ##############

#read in datacite data by publisher
load("data_rdata_files/Combined_datacite_publisher_metadata.Rdata")

#read in Duke data
duke_cr <- read.csv(file="data_all_dois/Duke_CrossrefDOIs_20221113.csv")
  
duke_cr %>% 
  group_by(type, container.title, publisher) %>% 
  summarize(count=n())

#read in google sheet with specifications
whichpub <-  read_sheet("https://docs.google.com/spreadsheets/d/113bhDnX5qERsohkZhHNJJguyYAnuZvULpm6_yQnLfRs/edit#gid=0", sheet = "By Publisher")
umichpub <- read_sheet("https://docs.google.com/spreadsheets/d/113bhDnX5qERsohkZhHNJJguyYAnuZvULpm6_yQnLfRs/edit#gid=0", sheet = "Michigan List")

## STOP here to authenticate

## Subset to correct publisher names #####

whichpubtake <- whichpub %>% 
  filter(`Correct Publisher 
(1= yes/keep; 0=no/remove)` == 1)


#remove Duke
all_dois_noduke <- all_dois %>% 
  filter(institution != "Duke")

#first subset to year and type
# filter to >=2012
all_dois_noduke2012 <- all_dois_noduke %>% 
  filter(publicationYear >= 2012) 
dim(all_dois_noduke2012)

#filter to software and data only
all_doisfiltered <- all_dois_noduke2012 %>% 
  filter(resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software") 
dim(all_doisfiltered)



all_dois_include <- all_doisfiltered %>% 
  filter(publisher %in% whichpubtake$publisher)

all_dois_include1 <- all_dois_include %>% 
  select(institution, id, doi, publisher, publicationYear, resourceType, resourceTypeGeneral, url, metadataVersion, citationCount, versionCount, registered, subjects, dates, relatedIdentifiers, sizes, rightsList, descriptions, geoLocations, fundingReferences, formats, titles, creators, language)

## Replace Dukes's data with the CR data
#for the two NAs for published print, go by deposit year (2018 for both)
duke_cr1 <- duke_cr %>% 
  mutate(institution = "Duke", 
         id = as.character(row_number()), 
         publicationYear = case_when(!is.na(published.print) ~ gsub("[[:digit:]]{2}-[[:digit:]]{2}$", "", published.print), 
                                     is.na(published.print) ~ gsub("[[:digit:]]{2}-[[:digit:]]{2}$", "", deposited))) %>% 
  select(institution, id,  doi, publisher,  publicationYear, type, container.title, url, is.referenced.by.count, issued) %>% 
  rename(resourceTypeGeneral = type, 
         citationCount= is.referenced.by.count, 
         registered = issued)



#Remove Duke datacite info, then merge the datasets (DID THIS ABOVE)
# all_dois_include2_noduke <- all_dois_include1 %>% 
#   filter(institution != "Duke") 
# 
# all_dois_include2_noduke %>% 
#   group_by(institution) %>% 
#   summarize(count=n())
# 
# dim(all_dois_include2_noduke)

#duke without morphorsource
duke_cr2 <- duke_cr1 %>% 
  filter(container.title != "MorphoSource Media") %>% 
  mutate(publicationYear = gsub("-", "", publicationYear))

dim(duke_cr2)
  
all_dois_include2 <- all_dois_include1 %>% 
  mutate(publicationYear = as.character(publicationYear)) %>% 
  bind_rows(duke_cr2)

dim(all_dois_include2)

## Add subsets for Michigan, using container.title?
all_dois_include2$container.title[which(all_dois_include2$publisher %in% umichpub$publisher)] <-  umichpub$`Aggregated publisher?`[match(all_dois_include2$publisher[which(all_dois_include2$publisher %in% umichpub$publisher)],  umichpub$publisher)]


## Harmonize publisher names ######

#DRUM is inconsistently specified (with and without DRUM)
all_dois_include2$publisher[grep(" University of Minnesota", all_dois_include2$publisher)] <- "Data Repository for the University of Minnesota (DRUM)"

all_dois_include2$publisher[grep("Virginia", all_dois_include2$publisher)] <- "University Libraries, Virginia Tech"


#add container.title as more detailed publisher
all_dois_include3 <- all_dois_include2 %>% 
  mutate(publisher_plus = case_when(is.na(container.title) ~ institution, 
                                     !is.na(container.title) ~ paste(institution, container.title, sep="-")))


all_dois_by_IRpublisher <-  all_dois_include3 %>% 
  mutate(group = "IR_publisher")


  

#write out for later join plotting
write.csv(all_dois_by_IRpublisher, file="data_all_dois/All_IR_dois_20221216.csv", row.names = F)
save(all_dois_by_IRpublisher, file="data_all_dois/All_IR_dois_20221216.Rdata")

load("data_all_dois/All_IR_dois_20221216.Rdata")
dim(all_dois_by_IRpublisher)

#make sure dataset is capitalized in all metadata resource types
all_dois_by_IRpublisher[which(all_dois_by_IRpublisher$resourceTypeGeneral == "dataset"),]$resourceTypeGeneral <- "Dataset"

#make sure year is numberic
all_dois_by_IRpublisher$publicationYear <- as.numeric(gsub("-", "", all_dois_by_IRpublisher$publicationYear))

## filter to >= 2012 and data/software
all_dois_by_IRpublisher1 <- all_dois_by_IRpublisher %>% 
  filter(publicationYear >= 2012, 
         resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software") %>% 
  mutate(publicationYear = as.character(publicationYear))

### Datacite data by affiliation #########

#remove all from workspace except IRdois
rm(list = ls()[-which(ls() == "all_dois_by_IRpublisher1")])

#read in datacite data by affiliation
load(file="data_rdata_files/Combined_datacite_metadata_yeartypefilter.Rdata")

all_dois_ds12 %>% 
  group_by(resourceTypeGeneral) %>% 
  summarize(count=n())


#load in crossref data just in case generated by 2_Metadatadownload_crossref.R
load(file="data_rdata_files/Combined_Filtered_Crossref_Data_ForAnalysis.Rdata")

#update crossref institution variable
alldat_filtered <- alldat_filtered %>% 
  mutate(institution = case_when(grepl("Cornell", name) ~ "Cornell", 
                                 grepl("Duke", name) ~ "Duke", 
                                 grepl("Michigan", name) ~ "Michigan", 
                                 grepl("Minnesota", name) ~ "Minnesota", 
                                 grepl("Virginia", name) ~ "Virginia Tech", 
                                 grepl("Washington", name) ~ "Washington U"))




all_dois_by_AffiliationDC <- all_dois_ds12 %>% 
  mutate(group = "Affiliation - Datacite", 
         publicationYear = as.character(publicationYear)) %>% 
  rename(DOI = doi, 
         URL = url)


all_dois_by_AffiliationCR <- alldat_filtered %>% 
  mutate(group = "Affiliation - CrossRef") %>% 
  rename(publicationYear = `created.date-parts1`, 
         resourceTypeGeneral = type)

all_dois_by_AffiliationCR[which(all_dois_by_AffiliationCR$resourceTypeGeneral == "dataset"),]$resourceTypeGeneral <- "Dataset"


combined_dois <- bind_rows(all_dois_by_AffiliationDC, all_dois_by_AffiliationCR, all_dois_by_IRpublisher1)
dim(combined_dois)

by(combined_dois, combined_dois$group, function(y) apply(y, 2, function(x) sum(is.na(x))))

tapply(combined_dois$publicationYear, combined_dois$group, function(x) sum(is.na(x)))

combined_dois %>% 
  group_by(publisher) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

combined_dois %>% 
  group_by(group) %>% 
  summarize(count=n())


#write out combined data 
save(combined_dois, file = "data_rdata_files/Combined_ALL_data.Rdata")



# 
# ### Overall counts ######
# 
# #counts by publisher
# combined_dois %>%
#   group_by(publisher_plus) %>%
#   summarize(count=n()) %>%
#   arrange(desc(count))
# 
# 
# #counts by resource type & publisher
# by_resource <- all_dois_include3 %>%
#   group_by(publisher_plus, resourceTypeGeneral) %>%
#   summarize(count=n()) %>%
#   arrange(publisher_plus, desc(count))
# 
# write.csv(by_resource, file = "Counts of Resource Types by IR Publisher.csv", row.names = F)
# 
# ### Counts of data ########
# #subset to only datasets
# data_dois <- all_dois_include3 %>%
#   mutate(resourceTypeGeneral = gsub("dataset", "Dataset", resourceTypeGeneral)) %>%
#   filter(resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software")
# 
# data_dois %>%
#   group_by(publisher_plus, resourceTypeGeneral) %>%
#   summarize(count=n()) %>%
#   arrange(publisher_plus) %>%
#   pivot_wider(names_from = resourceTypeGeneral,
#               values_from = count,
#               values_fill = 0)
# 
# 
# ## Bar graph of data + software by institution
# ds.barplot <- data_dois %>%
#   group_by(institution, publisher_plus) %>%
#   summarize(count=n()) %>%
#   ggplot(aes(x=publisher_plus, y=count)) +
#   geom_bar(stat = "identity", aes(fill=institution)) +
#   coord_flip(ylim = c(0,1000)) +
#   scale_fill_discrete(name="Institution") +
#   labs(y="Number of DOIs", x="Publisher", title="Number of Data & Software DOIs by Institutional Repository", caption = "Note: MorphoSource (n=12899) is cut off at 1000 for scaling" ) +
#   theme_bw()
# 
# ggsave(ds.barplot,
#        filename = "Data and Software DOIs by Publisher.png",
#        device = "png",
#        width = 12,
#        height = 6)
# 
# 
# 
# ##START HERE - by year, type, citations??
# 
# #table of  publishers - data
# by_publisher_data_table <- by_publisher_data %>%
#   pivot_wider(names_from = institution,
#               values_from = count,
#               values_fill = 0) %>%
#   rowwise %>%
#   mutate(Total = sum(c_across(Cornell:`Washington U`))) %>%
#   arrange(desc(Total))
# 
# 
# #write out the table of data publishers
# write.csv(by_publisher_data_table, file="Counts of Data Publishers By Insitituion.csv", row.names = F)
# 
# 
# ### Counts of software ######
# #subset to only software
# software_dois <- all_dois %>%
#   filter(resourceTypeGeneral == "Software")
# 
# by_publisher_software <- software_dois %>%
#   group_by(publisher, institution) %>%
#   summarize(count=n()) %>%
#   arrange(institution, desc(count))
# 
# #table of  publishers - software
# by_publisher_software_table <- by_publisher_software %>%
#   pivot_wider(names_from = institution,
#               values_from = count,
#               values_fill = 0) %>%
#   rowwise %>%
#   mutate(Total = sum(c_across(Cornell:`Washington U`))) %>%
#   arrange(desc(Total))
# 
# 
# #write out the table of software publishers
# write.csv(by_publisher_software_table, file="Counts of Software Publishers By Insitituion.csv", row.names = F)
# 
# 
# ## Create Graphs ######
# 
# ### Recreating the Main Graph from Ted - Data only ####
# # top 6 publishers of data dois
# 
# top6pubs <- by_publisher_data_table$publisher[1:6]
# 
# 
# top6colors <- c("Harvard Dataverse" = "dodgerblue2",
#             "Zenodo" = "darkorange1",
#             "ICPSR - Interuniversity Consortium for Political and Social Research" = "darkcyan",
#             "Dryad" = "lightgray",
#             "Qualitative Data Repository" = "gold1",
#             "figshare" = "purple")
# 
# 
# (by_publisher_data_plot <-  by_publisher_data %>%
#   filter(publisher %in% top6pubs) %>%
#   ggplot(aes(x=institution, y=count, fill=publisher)) +
#     geom_bar(stat="identity", position=position_dodge(preserve = "single")) +
#     scale_fill_manual(values = top6colors, name="Publisher")+
#     guides(fill = guide_legend(title.position = "top")) +
#     scale_y_continuous(breaks = seq(from = 0, to=5000, by=500)) +
#     coord_cartesian(ylim = c(0,5000)) +
#     labs(x = "Institution", y="Count of Data DOIs", caption = "Note: Michigan Dataverse bar cutoff for scaling") +
#     theme_bw() +
#     theme(legend.position = "bottom", legend.title.align = .5))
# 
# 
# ggsave(by_publisher_data_plot, filename = "Counts of Data DOIs by Institution.png", device = "png",  width = 8, height = 6, units="in")
# 
# 
# ### Recreating the Main Graph from Ted - Software only ####
# # top 4 publishers of software dois
# 
# top6pubs_soft <- by_publisher_software_table$publisher[1:4]
# 
# 
# top6colors_soft <- c("Zenodo" = "darkorange1",
#                      "Code Ocean" = "darkblue",
#                      "Optica Publishing Group" = "purple",
#                      "Cornell University Library" = "pink")
# 
# 
# (by_publisher_software_plot <-  by_publisher_software %>%
#     filter(publisher %in% top6pubs_soft) %>%
#     ggplot(aes(x=institution, y=count, fill=publisher)) +
#     geom_bar(stat="identity", position=position_dodge(preserve = "single")) +
#     scale_fill_manual(values = top6colors_soft, name="Publisher")+
#     guides(fill = guide_legend(title.position = "top")) +
#     labs(x = "Institution", y="Count of Software DOIs") +
#     theme_bw() +
#     theme(legend.position = "bottom", legend.title.align = .5))
# 
# 
# ggsave(by_publisher_software_plot, filename = "Counts of Software DOIs by Institution.png", device = "png",  width = 8, height = 6, units="in")
# 
# 
# ## Write out institutional data######
# 
# #Need to subset list variables
# all_dois_forcsv <- all_dois[,which(unlist(lapply(lapply(all_dois, class), length)) == 1)]
# data_dois_forcsv <- data_dois[,which(unlist(lapply(lapply(data_dois, class), length)) == 1)]
# software_dois_forcsv <- software_dois[,which(unlist(lapply(lapply(software_dois, class), length)) == 1)]
# 
# for (i in unique(all_dois$institution)) {
# 
#   all_dois_forcsv %>%
#     filter(institution == i) %>%
#     write.csv(file=paste0("All_dois_", i, gsub("-", "", Sys.Date()), ".csv"), row.names = F)
# 
#   data_dois_forcsv %>%
#     filter(institution == i) %>%
#     write.csv(file=paste0("Data_dois_", i, gsub("-", "", Sys.Date()), ".csv"), row.names = F)
# 
#   software_dois_forcsv %>%
#     filter(institution == i) %>%
#     write.csv(file=paste0("Software_dois_", i, gsub("-", "", Sys.Date()), ".csv"), row.names = F)
# 
# }
# 
# #write out combined datasets
# write.csv(all_dois_forcsv, file=paste0("All_dois_",  gsub("-", "", Sys.Date()), ".csv"), row.names = F)
# write.csv(data_dois_forcsv, file=paste0("Data_dois_", gsub("-", "", Sys.Date()), "csv"), row.names = F)
# write.csv(software_dois_forcsv, file=paste0("Software_dois_", gsub("-", "", Sys.Date()), ".csv"), row.names = F)
# 
# ### Individual Institution Plots ##############
# 
# #loop through each institution and save out graphs
# # think about how to set up colors to stay consistent
# 
# #replace ICPSR and DRUM with their acronyms for graphing
# all_dois <- all_dois %>%
#   mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR",
#                                publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM",
#                                publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab",
#                                publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
#                                TRUE ~ publisher))
# 
# data_dois <- data_dois %>%
#   mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR",
#                                publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM",
#                                publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab",
#                                publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
#                                TRUE ~ publisher))
# 
# by_publisher_data <- by_publisher_data %>%
#   mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR",
#                                publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM",
#                                publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab",
#                                publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
#                                TRUE ~ publisher))
# 
# by_publisher_software <- by_publisher_software %>%
#   mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR",
#                                publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM",
#                                publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab",
#                                publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
#                                TRUE ~ publisher))
# 
# colors <- data.frame(publisher = c("Harvard Dataverse", "Zenodo", "ICPSR",  "Dryad", "Qualitative Data Repository", "figshare"),
#                      color = c("dodgerblue2", "darkorange1", "darkcyan", "lightgray", "gold1", "purple"))
# 
# extracolors <- c( "darkgreen", "pink", "aquamarine", "deeppink3", "bisque4", "coral3", "darkolivegreen3", "darkslategray1", "darksalmon", "burlywood3", "darkseagreen1", "cornsilk4", "chocolate4", "darkmagenta")
# 
# for (i in unique(all_dois$institution)) {
#   ## Top 5 publishers - data
#   temp_pub_data <- by_publisher_data %>%
#     filter(institution == i) %>%
#     arrange(desc(count)) %>%
#     ungroup() %>%
#     slice_head(n=5)
# 
#   temp_pub_data_colors <- c(colors$color[match(temp_pub_data$publisher[order(temp_pub_data$publisher)], colors$publisher)])
#   temp_pub_data_colors[is.na(temp_pub_data_colors)] <- extracolors[1:length(temp_pub_data_colors[is.na(temp_pub_data_colors)])]
# 
#  temp_pub_graph_data <-  temp_pub_data %>%
#     ggplot(aes(x=publisher, y=count)) +
#     geom_bar(stat="identity", aes(fill=publisher)) +
#     scale_fill_manual(values=temp_pub_data_colors, labels = function(x) str_wrap(x, width = 10)) +
#     labs(x="Repository", y="Count of DOIs", title = paste(i, "Datasets by Repository - Top 5")) +
#     guides(fill = guide_legend(title.position = "top")) +
#     theme_bw() +
#     theme(legend.position = "bottom", legend.title.align = .5)
# 
#  #ggsave(temp_pub_graph_data, file=paste(i, "Datasets by Repository - Top 5.png"), dev="png", width = 8, height = 5, units="in")
# 
# 
#   ## Top 5 publishers - software
#  temp_pub_soft <- by_publisher_software %>%
#    filter(institution == i) %>%
#    arrange(desc(count)) %>%
#    ungroup() %>%
#    slice_head(n=5)
# 
#  temp_pub_soft_colors <- c(colors$color[match(temp_pub_soft$publisher[order(temp_pub_soft$publisher)], colors$publisher)])
#  temp_pub_soft_colors[is.na(temp_pub_soft_colors)] <- extracolors[1:length(temp_pub_soft_colors[is.na(temp_pub_soft_colors)])]
# 
#  temp_pub_graph_software <-  temp_pub_soft %>%
#    ggplot(aes(x=publisher, y=count)) +
#    geom_bar(stat="identity", aes(fill=publisher)) +
#    scale_fill_manual(values=temp_pub_soft_colors) +
#    labs(x="Repository", y="Count of DOIs", title = paste(i, "Software by Repository - Top 5")) +
#    guides(fill = guide_legend(title.position = "top")) +
#    theme_bw() +
#    theme(legend.position = "bottom", legend.title.align = .5)
# 
#  #ggsave(temp_pub_graph_software, file=paste(i, "Software by Repository - Top 5.png"), dev="png", width = 7, height = 5, units="in")
# 
# 
#  #Combine the data and software into a single graph?
#  temp_pub_comb <- temp_pub_data %>%
#    bind_rows(temp_pub_soft, .id = "Resource") %>%
#    mutate(Resource = case_when(Resource == 1 ~ "Dataset",
#                                Resource == 2 ~ "Software"))
# 
#  #adjust colors
#  temp_pub_comb_colors <- data.frame(publisher = temp_pub_comb$publisher,
#                                     color = colors$color[match(temp_pub_comb$publisher, colors$publisher)])
# 
#  temp_pub_comb_colors$color[is.na(temp_pub_comb_colors$color)] <- extracolors[1:length(temp_pub_comb_colors$color[is.na(temp_pub_comb_colors$color)])]
# 
#  temp_pub_comb_colors1 <- temp_pub_comb_colors$color
#  names(temp_pub_comb_colors1) <- temp_pub_comb_colors$publisher
# 
#  temp_pub_comb_graph <- temp_pub_comb %>%
#    ggplot(aes(x=publisher, y=count)) +
#    geom_bar(stat="identity", aes(fill=publisher)) +
#    scale_fill_manual(values=temp_pub_comb_colors1, name="") +
#    facet_grid(Resource~., scales = "free_y", switch="y") +
#    labs(x="Repository", y="Count of DOIs", title = paste(i, "DOIs by Repository - Top 5")) +
#    guides(fill = guide_legend(title.position = "top")) +
#    theme_bw() +
#    theme(legend.position = "bottom", legend.title.align = .5,
#          axis.text.x = element_text(angle=45, hjust = 1))
# 
# 
#  ggsave(temp_pub_comb_graph, file=paste(i, "Combined - Top 5.png"), dev="png", width=9, height = 7, units = "in")
# 
#   ## Number of data DOIs by year - line graph
#  # dataline <-  data_dois %>%
#  #    filter(institution == i) %>%
#  #    group_by(publicationYear) %>%
#  #    summarize(count=n()) %>%
#  #    mutate(publicationYear = factor(publicationYear)) %>%
#  #    ggplot(aes(x=publicationYear, y=count, group=1)) +
#  #    geom_line(stat = "identity") +
#  #    labs(x="Year", y="Number of Data DOIs", title=paste(i, "Number of Shared Datasets (2012-2022"))+
#  #    theme_bw()
# 
#  #ggsave(dataline, file=paste(i, "Number of Shared Datasets.png"), dev="png", width = 5.5, height = 4, units="in")
# 
#   ## Number of software DOIs by year - line graph
# # softline <-  software_dois %>%
# #    filter(institution == i) %>%
# #    group_by(publicationYear) %>%
# #    summarize(count=n()) %>%
# #    mutate(publicationYear = factor(publicationYear)) %>%
# #    ggplot(aes(x=publicationYear, y=count, group=1)) +
# #    geom_line(stat = "identity") +
# #    labs(x="Year", y="Number of Software DOIs", title=paste(i, "Number of Shared Software DOIs (2012-2022"))+
# #    theme_bw()
# 
#  #ggsave(softline, file=paste(i, "Number of Shared Software DOIs.png"), dev="png", width = 5.5, height = 4, units="in")
# 
#  ## combined line graph with data and software
#  datasoftline <- all_dois %>%
#    filter(resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software") %>%
#    filter(institution == i) %>%
#    group_by(publicationYear, resourceTypeGeneral) %>%
#    summarize(count=n()) %>%
#    mutate(publicationYear = factor(publicationYear)) %>%
#    ggplot(aes(x=publicationYear, y=count, group=resourceTypeGeneral)) +
#    geom_line(stat = "identity", aes(color=resourceTypeGeneral)) +
#    scale_color_discrete( name = "Resource Type") +
#    labs(x="Year", y="Number of DOIs", title=paste(i, "Number of Shared DOIs (2012-2022)"))+
#    theme_bw()
# 
#  ggsave(datasoftline, file=paste(i, "Number of Shared DOIs by Year.png"), dev="png", width = 5.5, height = 4, units="in")
# 
# 
# ## Top Publishers by year (top 10) - data
#  toppubs_data <- by_publisher_data %>%
#    filter(institution == i) %>%
#    arrange(desc(count)) %>%
#    ungroup() %>%
#    slice_head(n=10)
# 
#  temp_pub_daty_colors <- c(colors$color[match(toppubs_data$publisher[order(toppubs_data$publisher)], colors$publisher)])
#  temp_pub_daty_colors[is.na(temp_pub_daty_colors)] <- extracolors[1:length(temp_pub_daty_colors[is.na(temp_pub_daty_colors)])]
# 
# 
#  plot_byyear_data <- data_dois %>%
#    filter(institution == i, publisher %in% toppubs_data$publisher) %>%
#    group_by(publicationYear, publisher) %>%
#    summarize(count=n()) %>%
#    mutate(publicationYear = factor(publicationYear)) %>%
#    ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
#    geom_bar(stat="identity", position="stack") +
#    scale_fill_manual(values=temp_pub_daty_colors, labels = function(x) str_wrap(x, width = 10)) +
#    labs(x="Year", y="Count of DOIs", title=paste(i), subtitle = "Shared Datasets by Year and Repository") +
#    guides(fill = guide_legend(title.position = "top", nrow = 2)) +
#    theme_bw() +
#    theme(legend.position = "bottom", legend.title.align = .5)
# 
# # ggsave(plot_byyear_data, filename = paste(i, "Data DOIs by Year and Publisher.png"), device="png", width = 5.5, height = 4, units="in")
# 
# 
#   ## Top Publishers by year -- software
#  toppubs_soft <- by_publisher_software %>%
#    filter(institution == i) %>%
#    arrange(desc(count)) %>%
#    ungroup() %>%
#    slice_head(n=10)
# 
#  temp_pub_softy_colors <- c(colors$color[match(toppubs_soft$publisher[order(toppubs_soft$publisher)], colors$publisher)])
#  temp_pub_softy_colors[is.na(temp_pub_softy_colors)] <- extracolors[1:length(temp_pub_softy_colors[is.na(temp_pub_softy_colors)])]
# 
# 
#  plot_byyear_software <- software_dois %>%
#    filter(institution == i, publisher %in% toppubs_soft$publisher) %>%
#    group_by(publicationYear, publisher) %>%
#    summarize(count=n()) %>%
#    mutate(publicationYear = factor(publicationYear)) %>%
#    ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
#    geom_bar(stat="identity", position="stack") +
#    scale_fill_manual(values=temp_pub_daty_colors) +
#    labs(x="Year", y="Count of DOIs", title=paste(i), subtitle = "Shared Software by Year and Repository") +
#    guides(fill = guide_legend(title.position = "top", nrow = 3)) +
#    theme_bw() +
#    theme(legend.position = "bottom", legend.title.align = .5)
# 
#  #ggsave(plot_byyear_software, filename = paste(i, "Sofware DOIs by Year and Publisher.png"), device="png", width = 5.5, height = 4, units="in")
# 
# 
#  #Combine data and software by year
#  toppubs_comb <- toppubs_data %>%
#    bind_rows(toppubs_soft, .id = "Resource") %>%
#    mutate(Resource = case_when(Resource == 1 ~ "Dataset",
#                                Resource == 2 ~ "Software"))
# 
#  #adjust colors
#  temp_pub_comby_colors <- data.frame(publisher = toppubs_comb$publisher,
#                                     color = colors$color[match(toppubs_comb$publisher, colors$publisher)])
# 
#  temp_pub_comby_colors$color[is.na(temp_pub_comby_colors$color)] <- extracolors[1:length(temp_pub_comby_colors$color[is.na(temp_pub_comby_colors$color)])]
# 
#  temp_pub_comby_colors1 <- temp_pub_comby_colors$color
#  names(temp_pub_comby_colors1) <- temp_pub_comby_colors$publisher
# 
#  plot_byyear <- all_dois %>%
#    filter(resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software") %>%
#    filter(institution == i, publisher %in% toppubs_comb$publisher) %>%
#    group_by(publicationYear, publisher, resourceTypeGeneral) %>%
#    summarize(count=n()) %>%
#    mutate(publicationYear = factor(publicationYear)) %>%
#    ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
#    geom_bar(stat="identity", position="stack") +
#    facet_grid(resourceTypeGeneral~., scales = "free_y", switch="y") +
#    scale_fill_manual(values=temp_pub_comby_colors1, name="Repository") +
#    labs(x="Year", y="Count of DOIs", title=paste(i), subtitle = "Shared Software and Data by Year and Repository") +
#    guides(fill = guide_legend(title.position = "top", nrow = 3)) +
#    theme_bw() +
#    theme(legend.position = "bottom", legend.title.align = .5, legend.text = element_text(size = 6))
# 
#  ggsave(plot_byyear, filename = paste(i, "DOIs by Year and Publisher.png"), device="png", width = 8.5, height = 6, units="in")
# 
# 
# }
# 
# 
# 
# ## Look at VT ######
# 
# vt_data <- data_dois %>%
#   filter(institution == "Virginia Tech")
# 
# vt_data %>%
#   group_by(publicationYear, publisher) %>%
#   summarize(count=n()) %>%
#   arrange(publisher, publicationYear) %>%
#   write.csv(file="VT_dataDOIs_byPublisherYear.csv", row.names = F)
# 
# ## Ted's data #############
# 
# #read in the json files
# umnjsonfiles <- list.files("University_of_Minnesota__20211119_13/json/")
# 
# for (i in umnjsonfiles){
#   temp <- fromJSON(file=paste0("University_of_Minnesota__20211119_13/json/", i))
#   assign(x=gsub("\\.json", "", i), temp)
# }
# 
# #un-nest all files
# #This will transform all items with year at the end
# for (i in ls()[grep("[[:digit:]]{4}$", ls())]) {
# 
#   temp <- get(i)
#   temp1 <- as.data.frame(do.call(rbind, temp$data))
# 
#   tempdat <- temp1 %>%
#     unnest_wider(attributes, simplify = T) %>%
#     unnest_wider(relationships) %>%
#     unnest_wider(types) %>%
#     unnest_wider(client) %>%
#     unnest_wider(data, names_sep = "_") %>%
#     unnest_wider(container, names_sep = "_") %>%
#     mutate(sizes = list(sizes),
#            formats = list(formats)) %>%
#     tibble()
# 
#   assign(i, tempdat)
# 
# }
# 
# 
# #Combine them all together
# 
# umn_allyears <- bind_rows(lapply(grep("University_of_Minnesota", ls(), value=T), get))
# 
# save(umn_allyears, file="University_of_Minnesota_json_all.Rdata", row.names = F)
# 
# #remove list columsn for CSV
# umn_allyears_forcsv <- umn_allyears[,which(unlist(lapply(umn_allyears, class)) != "list")]
# 
# write.csv(umn_allyears_forcsv, file="University_of_Minnesota_json_all.csv", row.names = F)
