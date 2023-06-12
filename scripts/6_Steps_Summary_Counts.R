##### 
## Walking through the steps of Metadata
####
# This script summarizes the counts at each step of the process 

## AFFILIATION PULLS
# 1 Datacite metadata by researcher affiliation  (all 6 universities)
#55634 Done in 1_metadataPull_datacite.r; line 112

#filter >= 2012
#51053

#filter resource type = dataset and software
#31946

#2 Crossref metadata from April public use dataset (filter by year and affiliation and dataset)
#152376; 2_Metadatadownload_crossref.R, line 108

#a Cross ref public data search - after filtering out irrelevant Universities
#147702; 2_Metadatadownload_crossref.R, line 212


#3 INSTITUTIONAL REPOSITORIES
# a Datacite metadata by publisher 
#18985; 1_MetadataPull_datacite.R line 227

#filter >= 2012
#13279 - 1_MetadataPull (will need to move)

#filter to data or software only
#2532 - 1_metadata (will need to move)





# 5 Cross ref API based on Duke's IR prefixes
#13425; 2_Metadatadownload_crossref.R, line 272


# 6 remove Morphosource from duke
#225; 3_Combine_data.R; line 74 (Need to double check when there is an internet connection) -
 

#7 After review datacite counts by publisher to refine to rrelevant IR repositories (except for Duke)
# 2536; 3_Combine_data.R; line 68

#8 Combine steps 6 and 7 
#all repositories plue Duke 
#2164; 3_combine_data.R; line 110

#9 Combine 1 + 4 + 8
#181812; 3_combine_data.R, line 153


