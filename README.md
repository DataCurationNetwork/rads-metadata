# rads
Public repository for the Realities of Academic Data Sharing (RADS) Initiative 


This repository contains code and data for R metadata pulls. 

**Datacite Metadata**

Data Cite metadata was pulled using the rdatacite package. Searchers were done by author affiliation affiliation and by publisher for each institutional repository. Individual DOIs dataset and software DOIs were compared across instiutions and by publisher. 

**Crossref Metadata**

Crossref metadata was downloaded from the April 2022 Public Release file (https://academictorrents.com/details/4dcfdf804775f2d92b7a030305fa0350ebef6f3e) and filtered down to the relevant insitutional affiliations and year (>2012). The crossref API was used to pull Duke institutional repository information using the rcrossref package. 
