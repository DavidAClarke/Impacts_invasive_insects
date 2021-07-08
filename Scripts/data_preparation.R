                                      # Data preparation
                          #What data does information_availability need?
#species_assessed_min

                                #What data does literature need?
#Literature, Literature.min, Impacts.Max

                            #What data does mechanism_severity need?
#Impacts.nonDD

                          #What data does socioeconomic_pests need?
#Literature, Impacts.noNA

                          #What data does islands_mainland need?
#Impacts.nonDD

                          #What data does severity_range need?
#MaxSev_range

                            #What data does world_maps need?
#Insect_Distribution, Pool, lvl_0_sf

# Species pool
Pool <- Pool %>% rename(dwc.Taxon = `Species name`)

# Literature data
Literature <- Literature %>%
  dplyr::select(`Order`, `Species name`, `Number of search returns`, `Final number used in assessments`, `CPC pest datasheet (Yes/No)`) %>%
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
  rename(SEPest = "CPC pest datasheet (Yes/No)")

Literature_min <- Literature %>%
  dplyr::filter(Evidence != 0)

Literature_SE <- Literature %>%
  dplyr::select(`SearchReturns`, `SEPest`) %>%
  arrange(SearchReturns) %>%
  mutate(Rank = seq(1:length(SearchReturns)))

RankSums <- Literature_SE %>%
  dplyr::group_by(SEPest) %>%
  summarise(sum(Rank))

# Impact data preparation
Impacts <- Impacts %>% 
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
  mutate(PubID = as.character(PubID)) %>% 
  mutate(PubYear = as.integer(PubYear))

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
  mutate(LandmassType = factor(LandmassType, levels = c("Continent", "Island")))


#Selecting only data deficient species
Impacts.DD <- Impacts.noNA %>% dplyr::filter(Severity == "DD")


#Selecting only maximum impact information
Impacts.Max <- Impacts.noNA %>% 
  dplyr::filter(GlobalMaxSeverity == "Yes") #%>%
#distinct(scientificName = scientificName, .keep_all = T) %>%
#dplyr::select(Order, scientificName, Severity)
Impacts.Max.min <- Impacts.Max %>% 
  distinct(scientificName = scientificName, .keep_all = T)

#Number of seearch returns compared to maximum impact severity
Literature_min_max <- Literature_min %>% 
  left_join(Impacts.Max.min, by = "scientificName") %>%
  dplyr::select(Order.x, scientificName, SearchReturns, Severity) %>%
  dplyr::mutate(Severity = factor(Severity, levels = c("MC","MN","MO","MR")))

#Number of alien countries per species (for assessed species)
Ins_Dist <- Impact_Distribution %>%
  dplyr::select(scientificName, ISO3, Country, MaxSeverity) %>%
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
  dplyr::select(AlienCountries, Severity, scientificName)

#Alien insect distribution (all insect species in GRIIS)
Insect_Distribution_min <- Insect_Distribution %>%
  dplyr::select(dwc.order, dwc.family, dwc.Taxon, ISO3, ISO3.Country, isInvasive) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Coccoidea", "Hemiptera")) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Coccoidea", "Hemiptera")) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Homoptera", "Hemiptera")) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Rhynchota", "Hemiptera")) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Sternorrhyncha", "Hemiptera")) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Phasmida", "Phasmatodea")) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Psocoptera", "Psocodea")) %>%
  mutate(dwc.order = stringr::str_replace(dwc.order, "Dictyoptera", "Blattodea")) %>%
  mutate(dwc.order = as.factor(dwc.order)) %>%
  mutate(ISO3.Country = as.factor(ISO3.Country))

#~# Number of insect species per order in GRIIS
Alien_orders <- Insect_Distribution_min %>%
  distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
  group_by(dwc.order) %>% 
  summarise(length(dwc.Taxon)) %>%
  rename(Counts = "length(dwc.Taxon)") %>%
  rename(Order = "dwc.order")


#~# Number of species assessed per order
Orders <- Impacts %>% 
  distinct(scientificName = scientificName, .keep_all = T) %>%
  group_by(Order) %>% 
  summarise(length(scientificName)) %>%
  rename(Counts = "length(scientificName)") 


#~# Number of species assessed per order (minus NA species)
Orders_noNA <- Impacts.noNA %>% 
  distinct(scientificName = scientificName, .keep_all = T) %>%
  group_by(Order) %>% 
  summarise(length(scientificName)) %>%
  rename(Counts = "length(scientificName)") 


#~# Number of species with evidence per order
Evidence <- Impacts.nonDD %>%
  distinct(scientificName = scientificName, .keep_all = T) %>%
  group_by(Order) %>%
  summarise(length(scientificName))


#~# Number of species per order vs number of species assessed vs those with evidence
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

#~# Only including orders with assessed (and no NA) species
species_assessed_min <- species_assessed %>%
  dplyr::filter(Assessed_noNA > 0) %>%
  dplyr::select(-GRIIS)

#~# Number of studies per species
Studies <- Impacts.nonDD %>% 
  group_by(Order, scientificName) %>%
  distinct(PubID = PubID, .keep_all = T) %>%
  summarise(length(PubID)) %>%
  rename(Counts = "length(PubID)")

