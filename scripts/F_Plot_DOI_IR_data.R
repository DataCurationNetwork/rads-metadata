#################################################
## RADS Metadata IR & Repo Graphing
##
## 2022-07-27
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


#set working directory
setwd("~/Documents/NSF_RADS/Metadata_Analysis/")


#read in IR data
irdat <- read.csv("All_IR_dois_20220727.csv")
doidat <- read.csv("All_dois_20220624.csv")


# Examine overlap and merge ######

summary(irdat$doi %in% doidat$doi)
  #there is some overlap

#add in the IR data, but do not include the overlapping DOIs
doidat$publicationYear <- as.character(doidat$publicationYear)

alldat <- irdat %>% 
  filter(doi %in% doidat$doi == FALSE) %>% 
  bind_rows(doidat)


#subset to only datasets
data_dois <- alldat %>% 
  mutate(resourceTypeGeneral = gsub("dataset", "Dataset", resourceTypeGeneral), 
         publisher_plus = case_when(is.na(container.title) ~ publisher, 
                                    !is.na(container.title) ~ paste(institution, container.title, sep="-"))) %>% 
  filter(resourceTypeGeneral == "Dataset") 

data_dois %>% 
  group_by(publisher_plus, resourceTypeGeneral) %>% 
  summarize(count=n()) %>% 
  arrange(publisher_plus) %>% 
  pivot_wider(names_from = resourceTypeGeneral, 
              values_from = count, 
              values_fill = 0)

#clean up some of the publisher names (ICPSR)
data_dois$publisher[grep("Consortium for Political and Social Research", data_dois$publisher)] <- "ICPSR - Interuniversity Consortium for Political and Social Research"
data_dois$publisher_plus[grep("Consortium for Political and Social Research", data_dois$publisher)] <- "ICPSR - Interuniversity Consortium for Political and Social Research"

#change publisher_plus name to ISR as ICPSR is captured by the publisher name
data_dois$publisher_plus[which(data_dois$publisher_plus == "Michigan-ICPSR/ISR")] <- "Michigan-ISR"

## Bar graph- data dois ONLY. Top 6 places + IRs
top6pubs <- c("Harvard Dataverse", "Zenodo", "Dryad", "Qualitative Data Repository", "ICPSR - Interuniversity Consortium for Political and Social Research", "figshare")
irpubs <- unique(data_dois$publisher_plus[which(data_dois$publisher %in% irdat$publisher)])
  
# top6colors <- c("Harvard Dataverse" = "dodgerblue2",
#                 "Zenodo" = "darkorange1",
#                 "ICPSR - Interuniversity Consortium for Political and Social Research" = "darkcyan",
#                 "Dryad" = "lightgray", 
#                 "Qualitative Data Repository" = "gold1",
#                 "figshare" = "purple")

## HOW MUCH DATA IS IN HERE VS ELSEWHERE?

#wrap version - free scales by institution
freescale_bar <- data_dois %>% 
  filter(publisher_plus %in% c(top6pubs, irpubs)) %>% 
  group_by(institution, publisher_plus) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=institution, y=count)) +
  geom_bar(stat = "identity", aes(fill=publisher_plus), position="dodge") +
  facet_wrap(~institution, scales = "free") +
  scale_fill_discrete(name="Institution") +
  labs(y="Number of DOIs", x="Publisher", title="Number of Data DOIs in Top 6 Publishers and IRs" ) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 3))


ggsave(freescale_bar, 
       filename = "Counts of Data DOIs by Insitution and IR.png", 
       device = "png", 
       height = 7, 
       width = 10)


## Collapse IRs into one column 
## Bar graph- data dois ONLY. Top 6 places + IRs
top6pubs <- c("Harvard Dataverse", "Zenodo", "Dryad", "Qualitative Data Repository", "ICPSR - Interuniversity Consortium for Political and Social Research", "figshare")
irpubs <- unique(data_dois$publisher_plus[which(data_dois$publisher %in% irdat$publisher)])


#second try 
collapseir <- data_dois %>% 
  filter(publisher_plus %in% c(top6pubs, irpubs)) %>% 
  mutate(publisher_plus = case_when(grepl("Michigan", publisher_plus) ~ "Michigan-Deep Blue & Others", 
                                    grepl("Duke", publisher_plus) ~ "Duke-RDR & Others", 
                                  TRUE ~ publisher_plus)) %>% 
  group_by(institution, publisher_plus) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=institution, y=count)) +
  geom_bar(stat = "identity", aes(fill=publisher_plus), position="dodge") +
  facet_wrap(~institution, scales = "free") +
  scale_fill_discrete(name="Institution") +
  labs(y="Number of DOIs", x="Publisher", title="Number of Data DOIs in Top 6 Publishers and IRs", caption = "Multiple Duke and Michigan IRs have been collapsed into a single bar") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 3))


ggsave(collapseir, 
       filename = "Counts of Data DOIs by Insitution and Combined IR.png", 
       device = "png", 
       height = 7, 
       width = 12)



## 3. Collapse IRs into one column - SAME COLUMN FOR ALL INSTITUTIONS
## Bar graph- data dois ONLY. Top 6 places + IRs
top6pubs <- c("Harvard Dataverse", "Zenodo", "Dryad", "Qualitative Data Repository", "ICPSR - Interuniversity Consortium for Political and Social Research", "figshare")
irpubs <- unique(data_dois$publisher_plus[which(data_dois$publisher %in% irdat$publisher)])


#graph
collapseallir <- data_dois %>% 
  filter(publisher_plus %in% c(top6pubs, irpubs)) %>% 
  mutate(publisher_plus = case_when(publisher_plus %in% irpubs ~ "Institutional Repository", 
                                    TRUE ~ publisher_plus)) %>% 
  group_by(institution, publisher_plus) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=institution, y=count)) +
  geom_bar(stat = "identity", aes(fill=publisher_plus), position="dodge") +
  facet_wrap(~institution, scales = "free") +
  scale_fill_discrete(name="Institution") +
  labs(y="Number of DOIs", x="Publisher", title="Number of Data DOIs in Top 6 Publishers and IRs", caption = "Note: Multiple IRs from Duke and Michigan have been combined") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 4))


ggsave(collapseallir, 
       filename = "Counts of Data DOIs by Insitution and IRs_singlebar.png", 
       device = "png", 
       height = 7, 
       width = 12)


#graph
collapseallir_oldbar <- data_dois %>% 
  filter(publisher_plus %in% c(top6pubs, irpubs)) %>% 
  mutate(publisher_plus = case_when(publisher_plus %in% irpubs ~ "Institutional Repository", 
                                    TRUE ~ publisher_plus)) %>% 
  group_by(institution, publisher_plus) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=publisher_plus, y=count)) +
  geom_bar(stat = "identity", aes(fill=institution), position="dodge") +
  #facet_wrap(~institution, scales = "free") +
  scale_fill_discrete(name="Publisher") +
  # coord_cartesian(ylim = c(0,5000)) +
  coord_flip()+
  labs(y="Number of DOIs", x="Institution", title="Number of Data DOIs in Top 6 Publishers and IRs", caption = "Scale has been cut off at 5000") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 6))


ggsave(collapseallir_oldbar, 
       filename = "Counts of Data DOIs by Publisher with IRs.png", 
       device = "png", 
       height = 6, 
       width = 12)



## OLD #########3

## Bar graph of data + software by institution
ds.barplot <- data_dois %>% 
  group_by(institution, publisher_plus) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=publisher_plus, y=count)) +
  geom_bar(stat = "identity", aes(fill=institution)) +
  coord_flip(ylim = c(0,1000)) +
  scale_fill_discrete(name="Institution") +
  labs(y="Number of DOIs", x="Publisher", title="Number of Data & Software DOIs by Institutional Repository", caption = "Note: MorphoSource (n=12899) is cut off at 1000 for scaling" ) +
  theme_bw()

ggsave(ds.barplot, 
       filename = "Data and Software DOIs by Publisher.png", 
       device = "png", 
       width = 12, 
       height = 6)



### Recreating the Main Graph from Ted - Data only ####
# top 6 publishers of data dois
#subset to only datasets
data_dois <- all_dois %>% 
  filter(resourceTypeGeneral == "Dataset") 

by_publisher_data <- data_dois %>% 
  group_by(publisher, institution) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count))

top6pubs <- by_publisher_data_table$publisher[1:6]


top6colors <- c("Harvard Dataverse" = "dodgerblue2",
                "Zenodo" = "darkorange1",
                "ICPSR - Interuniversity Consortium for Political and Social Research" = "darkcyan",
                "Dryad" = "lightgray", 
                "Qualitative Data Repository" = "gold1",
                "figshare" = "purple")


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


ggsave(by_publisher_data_plot, filename = "Counts of Data DOIs by Institution.png", device = "png",  width = 8, height = 6, units="in")

