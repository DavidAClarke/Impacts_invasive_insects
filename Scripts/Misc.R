#Stuff I am not sure if I need for this chapter

#######################################################################################################
#~# Getting GBIF records of Non-DD species for calculating EOO
SppNames <- read_csv("C:/Users/dcla0008/Dropbox/PhD/Milestones/Mid-Candidature/SppNames.csv") #work computer
SppNames <- read_csv("C:/Users/David Clarke.DESKTOP-NNNNVLL/Dropbox/PhD/Milestones/Mid-Candidature/SppNames.csv") #my computer
SppList <- as.character(SppNames$Name)
sppOcc <- as.data.frame(GetOcc(SppList))
save(sppOcc, file = "Outcome/sppOcc.RData", compress = "xz")
load("Outcome/sppOcc.Rdata")

#~# Subset based on basisofRecord
sppOcc.min <- sppOcc[sppOcc$basisOfRecord == "HUMAN_OBSERVATION" | sppOcc$basisOfRecord == "LITERATURE"
                     | sppOcc$basisOfRecord == "LIVING_SPECIMEN" | sppOcc$basisOfRecord == "OBSERVATION"
                     | sppOcc$basisOfRecord == "PRESERVED_SPECIMEN",]


#Need to separate those with trinomial names (A.m. scutellata has no GBIF records)
Species <- sppOcc.min %>% 
  filter(!grepl("carnica", scientificName) & 
           !grepl("nigrithorax", scientificName))

Subspecies <- sppOcc.min %>% 
  filter(grepl("carnica", scientificName) | 
           grepl("nigrithorax", scientificName))


#Removing authors from species names
Genspec <- as.data.frame(str_split_fixed(Species$scientificName, " ", 4))
Gensubspec <- as.data.frame(str_split_fixed(Subspecies$scientificName, " ", 5))
Genspec <- Genspec[,c(1,2)]
Gensubspec <- Gensubspec[,c(1:3)]
Binom <- paste(Genspec$V1, Genspec$V2, sep = " ")
Trinom <- paste(Gensubspec$V1, Gensubspec$V2, Gensubspec$V3, sep = " ")
Species <- Species %>% mutate(scientificName = Binom)
Subspecies <- Subspecies %>% mutate(scientificName = Trinom)
sppOcc.min <- bind_rows(Species, Subspecies)

# Taxonomic harmonisation
source("Scripts/TaxonHarm.R")

# Calculate EOO per species per country
source("Scripts/EOO.R")

#Can do a little plot if export_shp = TRUE when computing EOO
plot(object$spatial.polygon_1, col = "red")
plot(land, add = T)
#######################################################################################################

#######################################################################################################
#~# Geographic harmonisation----
Country <- ISO_3166_1[,1:4]
GADM <- merge(lvl_1_sf, Country, by.x = "GID_0", by.y = "Alpha_3") #this removes the Alpha_3 column. 
#need to figure out how to do this for level 1

##Helpful little code below will find all values in "Code" that begins(^) with "US"
subsdiv <- ISO_3166_2
subsdiv[grep("^US", subsdiv$Code),] 
#in this case, it only returns subdivisions of USA
#will be useful for fuzzy matching
#######################################################################################################

#######################################################################################################
#Phylogenetic stuff----
instree <- read.tree("Data/InsectPhylogeny/syen12414-sup-0001-appendixs1.nwk")
instree.names <- instree$tip.label #species names from phylogeny

#my species names. This includes those from chapter 1.
sppnames <- read_csv("Data/SpeciesNames.csv") 
sppnames <- str_replace(sppnames$`Species name`, " ", "_")

#the following returns the row number from the phylogeny dataset that matches my names
myNames <- match(sppnames, instree.names) 
#the following is even easier i.e. species that are in both sets
myNames <- intersect(sppnames, instree.names)

#there may be taxonomic issues e.g. synonyms being used so I should also try to do some fuzzy matching
#I think the following is all species in large set thats not in myNames i.e. species to drop
to_drop <- setdiff(instree.names, myNames)

#dropping those species from large set
instree.reduced <- drop.tip(instree, to_drop)
ggtree(instree.reduced) + geom_tiplab()
#######################################################################################################

#Map of the world for number of impact studies per country
#~# Example of making a map
Country_Imp <- read.csv("C:/Users/David Clarke.DESKTOP-NNNNVLL/Dropbox/PhD/Data/Chapter 2/Data/GADM_Country_Imp.csv") #my computer
Country_Imp <- read.csv("C:/Users/dcla0008/Dropbox/PhD/Data/Data general/GADM_Country_Imp.csv") #work computer
# add the impacts column to existing spatial dataframe
lvl_0_sf$No_Imp <- Country_Imp$No_Impacts
lvl_0_sf_NA <- lvl_0_sf[-12,] # removing Antarctica for graphical purpose
# create subset containing only countries with impacts
Imps <- lvl_0_sf_NA[lvl_0_sf_NA$No_Imp > 0,]
# Plotting world map of impacts by country
# first plot entire world coloured in white
tmap_mode("plot")
tm_shape(lvl_0_sf_NA) +
  tm_fill("white") +
  tm_borders("grey") +
  # then plot the countries with impacts
  tm_shape(Imps) +
  tm_polygons("No_Imp", breaks = c(1, 5, 10, 20, 60), palette = "Blues",
              title = "No. of impact studies", legend.is.portrait = T, legend.hist = F) +
  tm_layout(frame = T, legend.outside = T, legend.outside.position = "bottom",
            legend.stack = "horizontal", legend.hist.height = 0.5)
# tmap_save(Numimps_map, "Outcome/Numimps_map.jpg")
# # think I will need to add the legend separately, if I also want a histogram and maybe the
# # legend centred more

#For some reason, tmap is not working on my computer. Therefore, on my computer I need to use ggplot
ggplot(lvl_0_sf_NA) +
  geom_sf() +
  geom_sf(data = Imps, mapping = aes(fill = No_Imp)) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  theme_void()

# # If you want just a single country, can do the following:
# Aus <- lvl_0_sf[lvl_0_sf$NAME_0 == "Australia",]
# The following sets a boundary around Australia
# bbox <- st_bbox(c(xmin = 112.921112, xmax = 156.109222, 
#                   ymax = -9.142176, ymin = -45.116943))
# Cropping initial map
# Aus <- st_crop(Aus, y = bbox)
#
# m_Aus <- tm_shape(Aus, bbox = bbox) +
#   tm_polygons() +
#   tm_shape(vicadminveg_sf) +
#   tm_polygons("red")