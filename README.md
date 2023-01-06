### Examining the geographical and taxonomic variation in the environmental impacts of invasive alien insects. 

All data and necessary R scripts to re-create the analyses and figures associated with this project are located in this repository. If so inclined, one could simply run the `1.run_analysis.R` script which will carry out all analyses and create all figures. At the beginning of each script, it is noted what data are required to carry out the analyses for that particular script. The lvl_0.RData and lvl_1.RData data come from the Database of Global Administrative Areas (GADM) downloadable from https://gadm.org/data.html. Invasive alien species checklist information was obtained from the Global Register of Introduced and Invasive Species  (https://doi.org/10.5281/zenodo.63481164). Information on the number and location of shipping ports was obtained from Natural Earth (www.naturalearthdata.com/downloads/10m-cultural-vectors/ports/). 

### Required R packages
The following R packages will need to be installed prior to running these analyses:
* tidyverse
* sf
* janitor
* readxl
* tmap
* ggpol
* ggpubr
* MASS
* rms

If used, please cite the data and code: [![DOI](https://zenodo.org/badge/367756845.svg)](https://zenodo.org/badge/latestdoi/367756845)

Please also cite the associated scientific article (currently in preprint and under review) Associated with the preprint Clarke, D. A. & McGeoch, M. A. (2022). Invasive alien insects represent a clear but variable threat to biodiversity. _bioRxiv_. https://doi.org/10.1101/2022.06.16.496186
