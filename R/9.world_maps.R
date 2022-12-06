                                  #All insect species in GRIIS
#Requires: Insect_Distribution, Pool, lvl_0_sf

#Number of alien countries (from GRIIS)
Country_alien <- Insect_Distribution_min %>%
  group_by(countryCode_alpha3, country) %>%
  distinct(species = species, .keep_all = T) %>%
  summarise(length(species)) %>%
  rename(GID_0 = "countryCode_alpha3") %>%
  rename(Richness = "length(species)")

#Merging GRIIS and GADM
Alien_dist <- lvl_0_sf %>%
  left_join(Country_alien, by = "GID_0") %>%
  dplyr::mutate(Richness = replace_na(Richness,0))

#############################################################################################
#Map of the world for number of alien insects per country
Alien_dist_NA <- Alien_dist[-12,] #removing Antarctica
Num_species <- Alien_dist_NA %>%
  filter(Richness > 0)
tmap_mode("plot")
GRIIS_map <- tm_shape(Alien_dist_NA) +
  tm_fill("grey") +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_shape(Num_species) +
  tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
              breaks = c(1,2,5,10,20,50,150,500),
              legend.is.portrait = FALSE) +
  tm_layout(frame = T,
            legend.width = 1,
            legend.text.size = 0.8) +
  tm_format("World")
tmap_save(GRIIS_map, "Outcome/PDF/GRIIS_map2.pdf", width = 1920, height = 1080, asp = 0)
################################################################################################
                          #All species where isInvasive == Invasive
#Keep only species x country isInvasive records
isInvasive <- Insect_Distribution_min %>%
  dplyr::filter(isInvasive == "Invasive")

#Number of isInvasive countries (from GRIIS)
Country_isInvasive <- isInvasive %>%
  group_by(ISO3, ISO3.Country) %>%
  distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
  summarise(length(dwc.Taxon)) %>%
  rename(GID_0 = "ISO3") %>%
  rename(Richness = "length(dwc.Taxon)")

#Merging GRIIS and GADM
isInvasive_dist <- left_join(lvl_0_sf, Country_isInvasive, by = "GID_0")
isInvasive_dist <- isInvasive_dist %>%
  dplyr::mutate(Richness = replace_na(Richness,0))


#Map of the world for number of alien insects per country
isInvasive_dist_NA <- isInvasive_dist[-12,] #removing Antarctica
Num_species <- isInvasive_dist_NA %>%
  dplyr::filter(Richness > 0)
tmap_mode("plot")
isInvasive_map <- tm_shape(isInvasive_dist_NA) +
  tm_fill("grey") +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_shape(Num_species) +
  tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
              breaks = c(1,2,5,10,20,50,150,500),
              legend.is.portrait = FALSE) +
  tm_layout(frame = T,
            legend.width = 1,
            legend.text.size = 0.8) +
  tm_format("World")
tmap_save(isInvasive_map, "Outcome/PDF/isInvasive_map.pdf", width = 1920, height = 1080, asp = 0)

                            #All species where isInvasive == Null
#Keep only species x country isInvasive records
notisInvasive <- Insect_Distribution_min %>%
  dplyr::filter(isInvasive == "Null")

#Number of not isInvasive countries (from GRIIS)
Country_notisInvasive <- notisInvasive %>%
  group_by(ISO3, ISO3.Country) %>%
  distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
  summarise(length(dwc.Taxon)) %>%
  rename(GID_0 = "ISO3") %>%
  rename(Richness = "length(dwc.Taxon)")

#Merging GRIIS and GADM
notisInvasive_dist <- left_join(lvl_0_sf, Country_notisInvasive, by = "GID_0")
notisInvasive_dist <- notisInvasive_dist %>%
  dplyr::mutate(Richness = replace_na(Richness,0))


#Map of the world for number of notisInvasive insects per country
notisInvasive_dist_NA <- notisInvasive_dist[-12,] #removing Antarctica
Num_species <- notisInvasive_dist_NA %>%
  dplyr::filter(Richness > 0)
tmap_mode("plot")
notisInvasive_map <- tm_shape(notisInvasive_dist_NA) +
  tm_fill("grey") +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_shape(Num_species) +
  tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
              breaks = c(1,2,5,10,20,50,150,500),
              legend.is.portrait = FALSE) +
  tm_layout(frame = T,
            legend.width = 1,
            legend.text.size = 0.8) +
  tm_format("World")
tmap_save(notisInvasive_map, "Outcome/PDF/notisInvasive_map.pdf", width = 1920, height = 1080, asp = 0)

################################################################################################
################################################################################################
                            #All insect species in GRIIS (from my pool)
#Number of alien countries (from species pool)
Pool_GRIIS <- Pool %>% 
  left_join(Insect_Distribution_min, by = "species") %>%
  dplyr::select(species, countryCode_alpha3, country, isInvasive) %>%
  na.omit(Pool_GRIIS)

Country_alien_pool <- Pool_GRIIS %>%
  group_by(countryCode_alpha3, country) %>%
  distinct(species = species, .keep_all = T) %>%
  summarise(length(species)) %>%
  rename(GID_0 = "countryCode_alpha3") %>%
  rename(Richness = "length(species)")

#Merging species pool and GADM
Pool_dist <- lvl_0_sf %>% 
  left_join(Country_alien_pool, by = "GID_0") %>%
  dplyr::mutate(Richness = replace_na(Richness,0))

#Map of the world for number of alien insects per country from species pool
Pool_dist_NA <- Pool_dist[-12,] #removing Antarctica
Num_species <- Pool_dist_NA %>%
  dplyr::filter(Richness > 0)
tmap_mode("plot")
Pool_map <- tm_shape(Pool_dist_NA) +
  tm_fill("grey") +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_shape(Num_species) +
  tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
              breaks = c(1,2,5,10,20,50,150,500),
              legend.is.portrait = FALSE) +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_layout(frame = T,
            legend.width = 1,
            legend.text.size = 0.8) +
  tm_format("World")
tmap_save(Pool_map, "Outcome/PDF/Pool_map.pdf", width = 1920, height = 1080, asp = 0)
################################################################################################
                          #All species where isInvasive == Invasive
#Keep only species x country isInvasive records
Pool_isInvasive <- Pool_GRIIS %>%
  dplyr::filter(isInvasive == "Invasive")

                                      
#Number of isInvasive countries (from Pool)
Country_Pool_isInvasive <- Pool_isInvasive %>%
  group_by(ISO3, ISO3.Country) %>%
  distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
  summarise(length(dwc.Taxon)) %>%
  rename(GID_0 = "ISO3") %>%
  rename(Richness = "length(dwc.Taxon)")

#Merging GRIIS and GADM
Pool_isInvasive_dist <- lvl_0_sf %>%
  left_join(Country_Pool_isInvasive, by = "GID_0") %>%
  dplyr::mutate(Richness = replace_na(Richness,0))


#Map of the world for number of alien insects per country
Pool_isInvasive_dist_NA <- Pool_isInvasive_dist[-12,] #removing Antarctica
Num_species <- Pool_isInvasive_dist_NA %>%
  dplyr::filter(Richness > 0)
tmap_mode("plot")
Pool_isInvasive_map <- tm_shape(Pool_isInvasive_dist_NA) +
  tm_fill("grey") +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_shape(Num_species) +
  tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
              breaks = c(1,2,5,10,20,50,150,500),
              legend.is.portrait = FALSE) +
  tm_layout(frame = T,
            legend.width = 1,
            legend.text.size = 0.8) +
  tm_format("World")
tmap_save(Pool_isInvasive_map, "Outcome/PDF/Pool_isInvasive_map.pdf", width = 1920, height = 1080, asp = 0)

                            #All species where isInvasive == Null
#Keep only species x country isInvasive records
Pool_notisInvasive <- Pool_GRIIS %>%
  dplyr::filter(isInvasive == "Null")

#Number of not isInvasive countries (from GRIIS)
Country_Pool_notisInvasive <- Pool_notisInvasive %>%
  group_by(ISO3, ISO3.Country) %>%
  distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
  summarise(length(dwc.Taxon)) %>%
  rename(GID_0 = "ISO3") %>%
  rename(Richness = "length(dwc.Taxon)")

#Merging GRIIS and GADM
Pool_notisInvasive_dist <- lvl_0_sf  %>% 
  left_join(Country_Pool_notisInvasive, by = "GID_0") %>%
  dplyr::mutate(Richness = replace_na(Richness,0))


#Map of the world for number of notisInvasive insects per country
Pool_notisInvasive_dist_NA <- Pool_notisInvasive_dist[-12,] #removing Antarctica
Num_species <- Pool_notisInvasive_dist_NA %>%
  dplyr::filter(Richness > 0)
tmap_mode("plot")
Pool_notisInvasive_map <- tm_shape(Pool_notisInvasive_dist_NA) +
  tm_fill("grey") +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_shape(Num_species) +
  tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
              breaks = c(1,2,5,10,20,50,150,500),
              legend.is.portrait = FALSE) +
  tm_layout(frame = T,
            legend.width = 1,
            legend.text.size = 0.8) +
  tm_format("World")
tmap_save(Pool_notisInvasive_map, "Outcome/PDF/Pool_notisInvasive_map.pdf", width = 1920, height = 1080, asp = 0)


###############################################################################################
                              #My impact evidence distribution
#Number of invasive countries (countries with evidence of impact)
Country_invasive <- Impact_Distribution %>%
  dplyr::select(scientificName, ISO3, Country, MaxSeverity) %>%
  group_by(ISO3) %>%
  dplyr::filter(MaxSeverity != "NA" & MaxSeverity != "DD") %>%
  distinct(scientificName = scientificName, .keep_all = T) %>%
  summarise(length(scientificName)) %>%
  rename(Richness = "length(scientificName)") %>%
  rename(GID_0 = "ISO3")

#Merging Invasive countries and GADM
Inv_dist <- left_join(lvl_0_sf, Country_invasive, by = "GID_0") %>%
  mutate(Richness = replace_na(Richness,0))

#Map of the world for number of invasive insects per country
Inv_dist_NA <- Inv_dist[-12,] #removing Antarctica
Num_Inv_species <- Inv_dist_NA %>%
  dplyr::filter(Richness > 0)
tmap_mode("plot")
Invasive_map <- tm_shape(Inv_dist_NA) +
  tm_fill("grey") +
  tm_borders("lightgrey", lwd = 0.5) +
  tm_shape(Num_Inv_species) +
  tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
              breaks = c(1,2,5,10,20,50,150,500),
              legend.is.portrait = FALSE) +
  tm_layout(frame = T, 
            legend.width = 1, 
            legend.text.size = 0.8) +
  tm_format("World")
tmap_save(Invasive_map, "Outcome/PDF/Invasive.pdf", width = 1920, height = 1080, asp = 0)
