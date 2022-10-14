#################################################
## RADS Metadata analysis
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


# Explore data ###################
#If new data is not needed, just run this to load old pull
load(file="data_rdata_files/Combined_datacite_metadata.Rdata")

#subset the data to published years >= 2012
all_dois <- all_dois %>% 
  filter(publicationYear >= 2012) 
  

all_dois %>% 
  mutate(dupid = duplicated(id)) %>% 
  group_by(institution, dupid) %>% 
  summarize(count=n())

#only a few duplicates per institution. Curiously, VT seems to have fewer DOIs counted in datacite than when Ted pulled in 2021. 



#Repository name cleaning
#DRUM is inconsistently specified (with and without DRUM)
all_dois$publisher[grep("Data Repository for the University of Minnesota", all_dois$publisher)] <- "Data Repository for the University of Minnesota (DRUM)"


### Overall counts ######

#counts by publisher
by_publisher <- all_dois %>% 
  group_by(institution, publisher) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count)) 


#counts by resource type
by_resource <- all_dois %>% 
  group_by(institution, resourceTypeGeneral) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count)) 

#create table of top resources
by_resource_table <-  by_resource %>% 
  #filter(resourceTypeGeneral %in% c("Dataset", "Software", "Text", "Image")) %>% 
  pivot_wider(names_from = institution, 
              values_from = count, 
              values_fill = 0) %>%  
  rowwise %>% 
  mutate(Total = sum(c_across(Cornell:`Washington U`))) %>% 
  arrange(desc(Total))

write.csv(by_resource_table, file = "data_summary_data/Counts of Resource Types by Insitution.csv", row.names = F)

### Counts of data ########
#subset to only datasets
data_dois <- all_dois %>% 
  filter(resourceTypeGeneral == "Dataset") 

by_publisher_data <- data_dois %>% 
  group_by(publisher, institution) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count))

#table of  publishers - data
by_publisher_data_table <- by_publisher_data %>% 
  pivot_wider(names_from = institution, 
              values_from = count, 
              values_fill = 0) %>% 
  rowwise %>% 
  mutate(Total = sum(c_across(Cornell:`Washington U`))) %>% 
  arrange(desc(Total))


#write out the table of data publishers
write.csv(by_publisher_data_table, file="data_summary_data/Counts of Data Publishers By Insitituion.csv", row.names = F)


### Counts of software ######
#subset to only software
software_dois <- all_dois %>% 
  filter(resourceTypeGeneral == "Software")

by_publisher_software <- software_dois %>% 
  group_by(publisher, institution) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count))

#table of  publishers - software
by_publisher_software_table <- by_publisher_software %>% 
  pivot_wider(names_from = institution, 
              values_from = count, 
              values_fill = 0) %>% 
  rowwise %>% 
  mutate(Total = sum(c_across(Cornell:`Washington U`))) %>% 
  arrange(desc(Total))


#write out the table of software publishers
write.csv(by_publisher_software_table, file="data_summary_data/Counts of Software Publishers By Insitituion.csv", row.names = F)


## Create Graphs ######

### Recreating the Main Graph from Ted - Data only ####
# top 6 publishers of data dois

top6pubs <- by_publisher_data_table$publisher[1:6]


top6colors <- c("Harvard Dataverse" = "dodgerblue2",
            "Zenodo" = "darkorange1",
            "ICPSR - Interuniversity Consortium for Political and Social Research" = "darkcyan",
            "Dryad" = "lightgray", 
            "Qualitative Data Repository" = "gold1",
            "figshare" = "purple")

by_publisher_data %>% 
  group_by(publisher) %>% 
  summarize(count=sum(count)) %>% 
  mutate(intop6pub = publisher %in% top6pubs) %>% 
  group_by(intop6pub) %>% 
  summarize(totalDOIs = sum(count), nrepos = n()) %>% 
  ungroup() %>% 
  mutate(propDOIs = totalDOIs/sum(totalDOIs))


#publisher plots
by_publisher_data %>% 
  group_by(publisher) %>% 
  summarize(count=sum(count)) %>% 
  arrange(desc(count)) %>% 
  mutate(pubrank = order(count, decreasing = T)) %>% 
  ggplot(aes(x=pubrank, y=count)) +
  geom_bar(stat="identity") +
  scale_x_continuous(limits = c(0,10), n.breaks = 10) +
  labs(x = "Publisher Rank", y="Number of DOIs", title="Number of DOIs by top Publishers")+
  theme_bw() 

(by_publisher_data_plot <-  by_publisher_data %>% 
  filter(publisher %in% top6pubs) %>% 
  ggplot(aes(x=institution, y=count, fill=publisher)) +
    geom_bar(stat="identity", position=position_dodge(preserve = "single")) +
    scale_fill_manual(values = top6colors, name="Publisher")+
    guides(fill = guide_legend(title.position = "top")) +
    scale_y_continuous(breaks = seq(from = 0, to=5000, by=500)) +
    coord_cartesian(ylim = c(0,5000)) +
    labs(x = "Institution", y="Count of Data DOIs", caption = "Note: Michigan Dataverse bar cutoff for scaling") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title.align = .5))


ggsave(by_publisher_data_plot, filename = "figures/Counts of Data DOIs by Institution.png", device = "png",  width = 8, height = 6, units="in")


### Recreating the Main Graph from Ted - Software only ####
# top 4 publishers of software dois

top6pubs_soft <- by_publisher_software_table$publisher[1:4]


top6colors_soft <- c("Zenodo" = "darkorange1",
                     "Code Ocean" = "darkblue", 
                     "Optica Publishing Group" = "purple", 
                     "Cornell University Library" = "pink")


(by_publisher_software_plot <-  by_publisher_software %>% 
    filter(publisher %in% top6pubs_soft) %>% 
    ggplot(aes(x=institution, y=count, fill=publisher)) +
    geom_bar(stat="identity", position=position_dodge(preserve = "single")) +
    scale_fill_manual(values = top6colors_soft, name="Publisher")+
    guides(fill = guide_legend(title.position = "top")) +
    labs(x = "Institution", y="Count of Software DOIs") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title.align = .5))


ggsave(by_publisher_software_plot, filename = "figures/Counts of Software DOIs by Institution.png", device = "png",  width = 8, height = 6, units="in")


## Write out institutional data######

#Need to subset list variables
all_dois_forcsv <- all_dois[,which(unlist(lapply(lapply(all_dois, class), length)) == 1)]
data_dois_forcsv <- data_dois[,which(unlist(lapply(lapply(data_dois, class), length)) == 1)]
software_dois_forcsv <- software_dois[,which(unlist(lapply(lapply(software_dois, class), length)) == 1)]

for (i in unique(all_dois$institution)) {
  
  all_dois_forcsv %>% 
    filter(institution == i) %>% 
    write.csv(file=paste0("data_all_dois/All_dois_", i, gsub("-", "", Sys.Date()), ".csv"), row.names = F)
 
  data_dois_forcsv %>% 
    filter(institution == i) %>% 
    write.csv(file=paste0("data_data_dois/Data_dois_", i, gsub("-", "", Sys.Date()), ".csv"), row.names = F)
  
  software_dois_forcsv %>% 
    filter(institution == i) %>% 
    write.csv(file=paste0("data_software_dois/Software_dois_", i, gsub("-", "", Sys.Date()), ".csv"), row.names = F)
   
}

#write out combined datasets
write.csv(all_dois_forcsv, file=paste0("data_all_dois/All_dois_",  gsub("-", "", Sys.Date()), ".csv"), row.names = F)
write.csv(data_dois_forcsv, file=paste0("data_data_dois/Data_dois_", gsub("-", "", Sys.Date()), "csv"), row.names = F)
write.csv(software_dois_forcsv, file=paste0("data_software_dois/Software_dois_", gsub("-", "", Sys.Date()), ".csv"), row.names = F)

### Individual Institution Plots ##############

#loop through each institution and save out graphs
# think about how to set up colors to stay consistent

#replace ICPSR and DRUM with their acronyms for graphing
all_dois <- all_dois %>% 
  mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR", 
                               publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM",
                               publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab", 
                               publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
                               TRUE ~ publisher))
  
data_dois <- data_dois %>% 
  mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR", 
                               publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM",  
                               publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab", 
                               publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
                               TRUE ~ publisher))

by_publisher_data <- by_publisher_data %>% 
  mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR", 
                               publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM", 
                               publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab", 
                               publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
                               TRUE ~ publisher))

by_publisher_software <- by_publisher_software %>% 
  mutate(publisher = case_when(publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "ICPSR", 
                               publisher == "Data Repository for the University of Minnesota (DRUM)" ~ "DRUM", 
                               publisher == "Los Alamos National Laboratory (LANL), Los Alamos, NM (United States)" ~ "Los Alamos National Lab", 
                               publisher == "Sandia National Laboratories (SNL-NM), Albuquerque, NM (United States)" ~ "Sandia National Labs",
                               TRUE ~ publisher))

colors <- data.frame(publisher = c("Harvard Dataverse", "Zenodo", "ICPSR",  "Dryad", "Qualitative Data Repository", "figshare"),
                     color = c("dodgerblue2", "darkorange1", "darkcyan", "lightgray", "gold1", "purple"))

extracolors <- c( "darkgreen", "pink", "aquamarine", "deeppink3", "bisque4", "coral3", "darkolivegreen3", "darkslategray1", "darksalmon", "burlywood3", "darkseagreen1", "cornsilk4", "chocolate4", "darkmagenta")

for (i in unique(all_dois$institution)) {
  ## Top 5 publishers - data
  temp_pub_data <- by_publisher_data %>% 
    filter(institution == i) %>% 
    arrange(desc(count)) %>% 
    ungroup() %>% 
    slice_head(n=5) 
  
  temp_pub_data_colors <- c(colors$color[match(temp_pub_data$publisher[order(temp_pub_data$publisher)], colors$publisher)])
  temp_pub_data_colors[is.na(temp_pub_data_colors)] <- extracolors[1:length(temp_pub_data_colors[is.na(temp_pub_data_colors)])]
  
 temp_pub_graph_data <-  temp_pub_data %>% 
    ggplot(aes(x=publisher, y=count)) +
    geom_bar(stat="identity", aes(fill=publisher)) +
    scale_fill_manual(values=temp_pub_data_colors, labels = function(x) str_wrap(x, width = 10)) +
    labs(x="Repository", y="Count of DOIs", title = paste(i, "Datasets by Repository - Top 5")) +
    guides(fill = guide_legend(title.position = "top")) +
    theme_bw() +
    theme(legend.position = "bottom", legend.title.align = .5)
 
 #ggsave(temp_pub_graph_data, file=paste(i, "Datasets by Repository - Top 5.png"), dev="png", width = 8, height = 5, units="in")
    
  
  ## Top 5 publishers - software
 temp_pub_soft <- by_publisher_software %>% 
   filter(institution == i) %>% 
   arrange(desc(count)) %>% 
   ungroup() %>% 
   slice_head(n=5) 
 
 temp_pub_soft_colors <- c(colors$color[match(temp_pub_soft$publisher[order(temp_pub_soft$publisher)], colors$publisher)])
 temp_pub_soft_colors[is.na(temp_pub_soft_colors)] <- extracolors[1:length(temp_pub_soft_colors[is.na(temp_pub_soft_colors)])]
 
 temp_pub_graph_software <-  temp_pub_soft %>% 
   ggplot(aes(x=publisher, y=count)) +
   geom_bar(stat="identity", aes(fill=publisher)) +
   scale_fill_manual(values=temp_pub_soft_colors) +
   labs(x="Repository", y="Count of DOIs", title = paste(i, "Software by Repository - Top 5")) +
   guides(fill = guide_legend(title.position = "top")) +
   theme_bw() +
   theme(legend.position = "bottom", legend.title.align = .5)
 
 #ggsave(temp_pub_graph_software, file=paste(i, "Software by Repository - Top 5.png"), dev="png", width = 7, height = 5, units="in")
  
 
 #Combine the data and software into a single graph? 
 temp_pub_comb <- temp_pub_data %>% 
   bind_rows(temp_pub_soft, .id = "Resource") %>% 
   mutate(Resource = case_when(Resource == 1 ~ "Dataset", 
                               Resource == 2 ~ "Software"))
 
 #adjust colors
 temp_pub_comb_colors <- data.frame(publisher = temp_pub_comb$publisher, 
                                    color = colors$color[match(temp_pub_comb$publisher, colors$publisher)])
 
 temp_pub_comb_colors$color[is.na(temp_pub_comb_colors$color)] <- extracolors[1:length(temp_pub_comb_colors$color[is.na(temp_pub_comb_colors$color)])]
 
 temp_pub_comb_colors1 <- temp_pub_comb_colors$color
 names(temp_pub_comb_colors1) <- temp_pub_comb_colors$publisher
 
 temp_pub_comb_graph <- temp_pub_comb %>% 
   ggplot(aes(x=publisher, y=count)) +
   geom_bar(stat="identity", aes(fill=publisher)) +
   scale_fill_manual(values=temp_pub_comb_colors1, name="") +
   facet_grid(Resource~., scales = "free_y", switch="y") +
   labs(x="Repository", y="Count of DOIs", title = paste(i, "DOIs by Repository - Top 5")) +
   guides(fill = guide_legend(title.position = "top")) +
   theme_bw() +
   theme(legend.position = "bottom", legend.title.align = .5, 
         axis.text.x = element_text(angle=45, hjust = 1))
   
 
 ggsave(temp_pub_comb_graph, file=paste0("figures/", i, " Combined - Top 5.png"), dev="png", width=9, height = 7, units = "in")
 
  ## Number of data DOIs by year - line graph
 # dataline <-  data_dois %>% 
 #    filter(institution == i) %>% 
 #    group_by(publicationYear) %>% 
 #    summarize(count=n()) %>% 
 #    mutate(publicationYear = factor(publicationYear)) %>% 
 #    ggplot(aes(x=publicationYear, y=count, group=1)) +
 #    geom_line(stat = "identity") +
 #    labs(x="Year", y="Number of Data DOIs", title=paste(i, "Number of Shared Datasets (2012-2022"))+
 #    theme_bw()
 
 #ggsave(dataline, file=paste(i, "Number of Shared Datasets.png"), dev="png", width = 5.5, height = 4, units="in")
  
  ## Number of software DOIs by year - line graph
# softline <-  software_dois %>% 
#    filter(institution == i) %>% 
#    group_by(publicationYear) %>% 
#    summarize(count=n()) %>% 
#    mutate(publicationYear = factor(publicationYear)) %>% 
#    ggplot(aes(x=publicationYear, y=count, group=1)) +
#    geom_line(stat = "identity") +
#    labs(x="Year", y="Number of Software DOIs", title=paste(i, "Number of Shared Software DOIs (2012-2022"))+
#    theme_bw()
 
 #ggsave(softline, file=paste(i, "Number of Shared Software DOIs.png"), dev="png", width = 5.5, height = 4, units="in")
 
 ## combined line graph with data and software
 datasoftline <- all_dois %>% 
   filter(resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software") %>% 
   filter(institution == i) %>% 
   group_by(publicationYear, resourceTypeGeneral) %>% 
   summarize(count=n()) %>% 
   mutate(publicationYear = factor(publicationYear)) %>% 
   ggplot(aes(x=publicationYear, y=count, group=resourceTypeGeneral)) +
   geom_line(stat = "identity", aes(color=resourceTypeGeneral)) +
   scale_color_discrete( name = "Resource Type") +
   labs(x="Year", y="Number of DOIs", title=paste(i, "Number of Shared DOIs (2012-2022)"))+
   theme_bw()
 
 ggsave(datasoftline, file=paste0("figures/", i, " Number of Shared DOIs by Year.png"), dev="png", width = 5.5, height = 4, units="in")
 
 
## Top Publishers by year (top 10) - data 
 toppubs_data <- by_publisher_data %>% 
   filter(institution == i) %>% 
   arrange(desc(count)) %>% 
   ungroup() %>% 
   slice_head(n=10)
 
 temp_pub_daty_colors <- c(colors$color[match(toppubs_data$publisher[order(toppubs_data$publisher)], colors$publisher)])
 temp_pub_daty_colors[is.na(temp_pub_daty_colors)] <- extracolors[1:length(temp_pub_daty_colors[is.na(temp_pub_daty_colors)])]
 
 
 plot_byyear_data <- data_dois %>% 
   filter(institution == i, publisher %in% toppubs_data$publisher) %>%  
   group_by(publicationYear, publisher) %>% 
   summarize(count=n()) %>% 
   mutate(publicationYear = factor(publicationYear)) %>% 
   ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
   geom_bar(stat="identity", position="stack") +
   scale_fill_manual(values=temp_pub_daty_colors, labels = function(x) str_wrap(x, width = 10)) +
   labs(x="Year", y="Count of DOIs", title=paste(i), subtitle = "Shared Datasets by Year and Repository") +
   guides(fill = guide_legend(title.position = "top", nrow = 2)) +
   theme_bw() +
   theme(legend.position = "bottom", legend.title.align = .5)
  
# ggsave(plot_byyear_data, filename = paste(i, "Data DOIs by Year and Publisher.png"), device="png", width = 5.5, height = 4, units="in")
 
 
  ## Top Publishers by year -- software
 toppubs_soft <- by_publisher_software %>% 
   filter(institution == i) %>% 
   arrange(desc(count)) %>% 
   ungroup() %>% 
   slice_head(n=10)
 
 temp_pub_softy_colors <- c(colors$color[match(toppubs_soft$publisher[order(toppubs_soft$publisher)], colors$publisher)])
 temp_pub_softy_colors[is.na(temp_pub_softy_colors)] <- extracolors[1:length(temp_pub_softy_colors[is.na(temp_pub_softy_colors)])]
 
 
 plot_byyear_software <- software_dois %>% 
   filter(institution == i, publisher %in% toppubs_soft$publisher) %>%  
   group_by(publicationYear, publisher) %>% 
   summarize(count=n()) %>% 
   mutate(publicationYear = factor(publicationYear)) %>% 
   ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
   geom_bar(stat="identity", position="stack") +
   scale_fill_manual(values=temp_pub_daty_colors) +
   labs(x="Year", y="Count of DOIs", title=paste(i), subtitle = "Shared Software by Year and Repository") +
   guides(fill = guide_legend(title.position = "top", nrow = 3)) +
   theme_bw() +
   theme(legend.position = "bottom", legend.title.align = .5)
 
 #ggsave(plot_byyear_software, filename = paste(i, "Sofware DOIs by Year and Publisher.png"), device="png", width = 5.5, height = 4, units="in")
  
 
 #Combine data and software by year
 toppubs_comb <- toppubs_data %>% 
   bind_rows(toppubs_soft, .id = "Resource") %>% 
   mutate(Resource = case_when(Resource == 1 ~ "Dataset", 
                               Resource == 2 ~ "Software"))
 
 #adjust colors
 temp_pub_comby_colors <- data.frame(publisher = toppubs_comb$publisher, 
                                    color = colors$color[match(toppubs_comb$publisher, colors$publisher)])
 
 temp_pub_comby_colors$color[is.na(temp_pub_comby_colors$color)] <- extracolors[1:length(temp_pub_comby_colors$color[is.na(temp_pub_comby_colors$color)])]
 
 temp_pub_comby_colors1 <- temp_pub_comby_colors$color
 names(temp_pub_comby_colors1) <- temp_pub_comby_colors$publisher
 
 plot_byyear <- all_dois %>% 
   filter(resourceTypeGeneral == "Dataset" | resourceTypeGeneral == "Software") %>% 
   filter(institution == i, publisher %in% toppubs_comb$publisher) %>%  
   group_by(publicationYear, publisher, resourceTypeGeneral) %>% 
   summarize(count=n()) %>% 
   mutate(publicationYear = factor(publicationYear)) %>% 
   ggplot(aes(x=publicationYear, y=count, fill = publisher)) +
   geom_bar(stat="identity", position="stack") +
   facet_grid(resourceTypeGeneral~., scales = "free_y", switch="y") +
   scale_fill_manual(values=temp_pub_comby_colors1, name="Repository") +
   labs(x="Year", y="Count of DOIs", title=paste(i), subtitle = "Shared Software and Data by Year and Repository") +
   guides(fill = guide_legend(title.position = "top", nrow = 3)) +
   theme_bw() +
   theme(legend.position = "bottom", legend.title.align = .5, legend.text = element_text(size = 6))
 
 ggsave(plot_byyear, filename = paste0("figures/", i, " DOIs by Year and Publisher.png"), device="png", width = 8.5, height = 6, units="in")
 
 
}



## Look at VT ######

vt_data <- data_dois %>% 
  filter(institution == "Virginia Tech")

vt_data %>% 
  group_by(publicationYear, publisher) %>% 
  summarize(count=n()) %>% 
  arrange(publisher, publicationYear) %>% 
  write.csv(file="VT_dataDOIs_byPublisherYear.csv", row.names = F)

## Ted's data #############

#read in the json files 
umnjsonfiles <- list.files("json_files/University_of_Minnesota__20211119_13/json/") 

for (i in umnjsonfiles){
  temp <- fromJSON(file=paste0("University_of_Minnesota__20211119_13/json/", i))
  assign(x=gsub("\\.json", "", i), temp)
}

#un-nest all files 
#This will transform all items with year at the end
for (i in ls()[grep("[[:digit:]]{4}$", ls())]) {
  
  temp <- get(i)
  temp1 <- as.data.frame(do.call(rbind, temp$data))
  
  tempdat <- temp1 %>% 
    unnest_wider(attributes, simplify = T) %>%
    unnest_wider(relationships) %>%
    unnest_wider(types) %>%
    unnest_wider(client) %>%
    unnest_wider(data, names_sep = "_") %>%
    unnest_wider(container, names_sep = "_") %>% 
    mutate(sizes = list(sizes), 
           formats = list(formats)) %>% 
    tibble()
  
  assign(i, tempdat)
  
}


#Combine them all together

umn_allyears <- bind_rows(lapply(grep("University_of_Minnesota", ls(), value=T), get))

save(umn_allyears, file="data_rdata_files/University_of_Minnesota_json_all.Rdata", row.names = F)

#remove list columsn for CSV
umn_allyears_forcsv <- umn_allyears[,which(unlist(lapply(umn_allyears, class)) != "list")]

write.csv(umn_allyears_forcsv, file="json_files/University_of_Minnesota_json_all.csv", row.names = F)
