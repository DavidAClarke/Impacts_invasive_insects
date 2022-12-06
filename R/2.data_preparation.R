                                      # Data preparation

##What data does 3.information_availability.R need?
#species_assessed_min

##What data does 4.literature.R need?
#Literature, Literature.min, Impacts.Max

##What data does 5.mechanism_severity.R need?
#Impacts.nonDD

##What data does 6.socioeconomic_pests.R need?
#Literature, Impacts.noNA

##What data does 7.islands_mainland.R need?
#Impacts.nonDD

##What data does 8.severity_range.R need?
#MaxSev_range

##What data does 9.world_maps.R need?
#Insect_Distribution, Pool, lvl_0_sf

##What data does 10.impact_confidence.R need?
#Impacts.nonDD

#Species pool
Pool <- Pool %>% rename(species = `Species name`)

#Literature data
Literature <- Literature %>%
  dplyr::select(`Order`, `Species name`, `Number of search returns`, 
                `Final number used in assessments`, 
                `CPC pest datasheet (Yes/No)`) %>%
  mutate(Order = factor(Order, levels = c("Blattodea","Coleoptera","Dermaptera",
                                          "Diptera","Embioptera","Ephemeroptera",
                                          "Hemiptera","Hymenoptera","Lepidoptera",
                                          "Mantodea","Neuroptera","Odonata",
                                          "Orthoptera","Phasmatodea","Psocodea",
                                          "Siphonaptera","Thysanoptera","Trichoptera",
                                          "Zygentoma"))) %>%
  rename(SearchReturns = "Number of search returns") %>%
  rename(Evidence = "Final number used in assessments") %>%
  rename(scientificName = "Species name") %>%
  rename(SEPest = "CPC pest datasheet (Yes/No)") %>%
  mutate(SEPest = as.factor(SEPest))

Literature_min <- Literature %>%
  dplyr::filter(Evidence != 0)

Literature_SE <- Literature %>%
  dplyr::select(`SearchReturns`, `SEPest`) %>%
  arrange(SearchReturns) %>%
  mutate(Rank = seq(1:length(SearchReturns)))

RankSums <- Literature_SE %>%
  dplyr::group_by(SEPest) %>%
  summarise(sum(Rank))

#Impact data preparation
Impacts <- Impacts %>% 
  mutate(PubYear = as.numeric(PubYear)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(SocioEconomicPest = factor(SocioEconomicPest, levels = c("Yes", "No"))) %>%
  mutate(Order = factor(Order, levels = c("Blattodea","Coleoptera","Dermaptera",
                                          "Diptera","Embioptera","Ephemeroptera",
                                          "Hemiptera","Hymenoptera","Lepidoptera",
                                          "Mantodea","Neuroptera","Odonata",
                                          "Orthoptera","Phasmatodea","Psocodea",
                                          "Siphonaptera","Thysanoptera","Trichoptera",
                                          "Zygentoma"))) %>%
  mutate(Severity = factor(Severity, levels = c("NA","DD","MC","MN","MO","MR","MV"))) %>%
  mutate(Confidence = factor(Confidence, levels = c("NA", "Low", "Medium", "High"))) %>%
  mutate(scientificName = as.character(scientificName)) %>%
  mutate(scientificNameID = as.character(scientificNameID)) %>%
  mutate(PubID = as.character(PubID)) 
  

#Removing species considered to not have an alien populations  
Impacts.noNA <- Impacts %>% 
  dplyr::filter(Severity != "NA")


#Selecting only those species who have evidence of impact i.e. removing data deficient species
Impacts.nonDD <- Impacts.noNA %>% 
  dplyr::filter(Severity == "MC" | 
                  Severity == "MN" |
                  Severity == "MO" | 
                  Severity == "MR") %>%
  mutate(Order = factor(Order, levels = c("Blattodea","Coleoptera","Dermaptera",
                                          "Diptera","Hemiptera","Hymenoptera",
                                          "Lepidoptera","Mantodea","Psocodea",
                                          "Siphonaptera","Thysanoptera"))) %>%
  mutate(LandmassType = factor(LandmassType, levels = c("Continent", "Island"))) %>%
  mutate(Confidence = factor(Confidence, levels = c("Low", "Medium", "High")))

#Getting centroid for study locations
source("R/centroids.R")

#Selecting only data deficient species
Impacts.DD <- Impacts.noNA %>% dplyr::filter(Severity == "DD")


#Selecting only maximum impact information
Impacts.Max <- Impacts.nonDD %>% 
  dplyr::filter(GlobalMaxSeverity == "Yes") %>%
  distinct(scientificName = scientificName, .keep_all = T) %>%
  dplyr::select(Order, scientificName, Severity)

Impacts.Max.min <- Impacts.Max %>%
  distinct(scientificName = scientificName, .keep_all = T)

#Number of seearch returns compared to maximum impact severity
Literature_min_max <- Literature_min %>% 
  left_join(Impacts.Max.min, by = "scientificName") %>%
  dplyr::select(Order.x, scientificName, SearchReturns, Severity) %>%
  dplyr::mutate(Severity = factor(Severity, levels = c("MC","MN","MO","MR")))

#Calculate total area
Impact_Distribution <- Impact_Distribution %>%
  left_join(ports_country, by = c("ISO3" = "ISO3166A3")) %>%
  rename(Ports = `length(LOCODE)`) %>%
  mutate(Ports = replace_na(Ports, 0)) %>%
  group_by(scientificName) %>%
  mutate(Total_area = sum(area_km2)) %>% 
  mutate(Total_ports = sum(Ports))
  
  
#Number of alien countries per species (for assessed species)
Ins_Dist <- Impact_Distribution %>%
  dplyr::select(scientificName, ISO3, Country, MaxSeverity, area_km2, Total_area) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::filter(MaxSeverity != "NA") %>%
  group_by(scientificName) %>%
  summarise(length(ISO3))

#Maximum impact severity and geographical range
MaxSev_range <- Impacts.Max %>% 
  left_join(Ins_Dist, by = "scientificName") %>%
  rename(AlienCountries = "length(ISO3)") %>%
  mutate(AlienCountries = replace_na(AlienCountries,30)) %>% #for some reason the number did not come across with join
  mutate(Severity = ordered(factor(Severity, levels = c("MC", "MN", "MO", "MR")))) %>%
  dplyr::select(AlienCountries, Severity, scientificName) %>%
  left_join(Literature, by = "scientificName") %>%
  left_join(Impact_Distribution, by = "scientificName") %>%
  distinct(scientificName = scientificName, .keep_all = T) %>%
  dplyr::select(AlienCountries, Severity, scientificName, Order, SearchReturns, Evidence, SEPest, Total_area, Total_ports)

#Alien insect distribution (all insect species in GRIIS)
Insect_Distribution_min <- Insect_Distribution %>%
  dplyr::select(order, family, species, countryCode_alpha3, country, isInvasive) %>%
  mutate(order = stringr::str_replace(order, "Coccoidea", "Hemiptera")) %>%
  mutate(order = stringr::str_replace(order, "Coccoidea", "Hemiptera")) %>%
  mutate(order = stringr::str_replace(order, "Homoptera", "Hemiptera")) %>%
  mutate(order = stringr::str_replace(order, "Rhynchota", "Hemiptera")) %>%
  mutate(order = stringr::str_replace(order, "Sternorrhyncha", "Hemiptera")) %>%
  mutate(order = stringr::str_replace(order, "Phasmida", "Phasmatodea")) %>%
  mutate(order = stringr::str_replace(order, "Psocoptera", "Psocodea")) %>%
  mutate(order = stringr::str_replace(order, "Dictyoptera", "Blattodea")) %>%
  mutate(order = as.factor(order)) %>%
  mutate(country = as.factor(country))

#Number of insect species per order in GRIIS
Alien_orders <- Insect_Distribution_min %>%
  distinct(species = species, .keep_all = T) %>%
  group_by(order) %>% 
  summarise(length(species)) %>%
  rename(Counts = "length(species)") %>%
  rename(Order = "order")


#Number of species assessed per order
Orders <- Impacts %>% 
  distinct(scientificName = scientificName, .keep_all = T) %>%
  group_by(Order) %>% 
  summarise(length(scientificName)) %>%
  rename(Counts = "length(scientificName)") 


#Number of species assessed per order (minus NA species)
Orders_noNA <- Impacts.noNA %>% 
  distinct(scientificName = scientificName, .keep_all = T) %>%
  group_by(Order) %>% 
  summarise(length(scientificName)) %>%
  rename(Counts = "length(scientificName)") 


#Number of species with evidence per order
Evidence <- Impacts.nonDD %>%
  distinct(scientificName = scientificName, .keep_all = T) %>%
  group_by(Order) %>%
  summarise(length(scientificName))


#Number of species per order vs number of species assessed vs those with evidence
species_assessed <- full_join(Alien_orders, Orders, by = "Order") %>%
  mutate(Counts.x = replace_na(Counts.x,0)) %>%
  mutate(Counts.y = replace_na(Counts.y,0))
species_assessed <- left_join(species_assessed, Orders_noNA, by = "Order") %>%
  dplyr::mutate(Counts = replace_na(Counts,0))
species_assessed <- left_join(species_assessed, Evidence, by = "Order") %>%
  rename(Evidence = "length(scientificName)") %>%
  rename(Assessed = "Counts.y") %>%
  rename(GRIIS = "Counts.x") %>%
  rename(Assessed_noNA = "Counts") %>%
  mutate(Evidence = replace_na(Evidence, 0)) %>%
  mutate(DataDeficient = Assessed_noNA - Evidence) %>%
  mutate(NotDD = round(Evidence/Assessed_noNA, 2)) %>%
  mutate(DD = round(1 - NotDD, 2))

#Only including orders with assessed (and no NA) species
species_assessed_min <- species_assessed %>%
  dplyr::filter(Assessed_noNA > 0) %>%
  dplyr::select(-GRIIS)

#Number of studies per species
Studies <- Impacts.nonDD %>% 
  group_by(Order, scientificName) %>%
  distinct(PubID = PubID, .keep_all = T) %>%
  summarise(length(PubID)) %>%
  rename(Counts = "length(PubID)")

