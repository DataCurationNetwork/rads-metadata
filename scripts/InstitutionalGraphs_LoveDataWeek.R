## Metadata graphs for WASH U Love Data Presentation
## 2023-02-07

#packages
pacman::p_load(dplyr, 
               tidyr, 
               ggplot2, 
               rjson,
               rdatacite,
               cowplot, 
               stringr, 
               knitr, 
               DT)


#Load the combined data from 3_Combined_data.R
load(file="data_rdata_files/Combined_ALL_data.Rdata")

#set colors
washucolors <- c("#e1c4ac", "#172752", "#005f85", "#ffcc00", "#d15f27")

#rename object
all_dois <- combined_dois 

#re-factor group so that datacite appears before cross ref
all_dois$group <- factor(all_dois$group, levels = c("Affiliation - Datacite", "Affiliation - CrossRef", "IR_publisher"))

#Collapse by container
containerdups <- which(!is.na(all_dois$container_identifier) & duplicated(all_dois$container_identifier))

all_dois_collapsed <- all_dois[-containerdups,]

by_publisher_dc_collapse <- all_dois_collapsed %>% 
  group_by(publisher, institution) %>% 
  summarize(count=n()) %>% 
  arrange(institution, desc(count)) %>% 
  filter(institution == "Washington U") %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  slice_head(n=8) %>% 
  mutate(RepoType = case_when(publisher %in% c("Zenodo", "figshare", "Dryad") ~ "General Repository", 
                              publisher == "ICPSR - Interuniversity Consortium for Political and Social Research" ~ "Domain Repository", 
                              publisher == "Washington University in St. Louis" ~ "Institutional Repository - WashU", 
                              TRUE ~ "Journal"), 
         publisher = gsub("- Interuniversity Consortium for Political and Social Research", "", publisher))



by_publisher_dc_collapse %>% 
  ggplot(aes(x=publisher, y=count, fill=RepoType)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single")) +
  scale_fill_manual(values=washucolors[1:4], name="Repository Type") +
  guides(fill = guide_legend(title.position = "top")) +
  labs(y="Count of Data DOIs - Collapsed", title = "Wash U Top 8 Data Publishers", x="Repository") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom", legend.title.align = .5, text=element_text(size=15))

ggsave(filename = "figures/Washinton U - Top 8 For Love Data Week.png", device = "png", width = 11, height = 5)
