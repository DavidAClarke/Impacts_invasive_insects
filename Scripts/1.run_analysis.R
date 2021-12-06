                                    #Load required libraries and data
#~# Clears the environment
rm(list=ls())

#Required libraries
library(tidyverse)
library(janitor)
library(sf)
library(readxl)
library(tmap)
library(ggpol) #facet_share
library(ggpubr) #balloon plots
library(MASS)
library(PNWColors)
#Package "rms" needs to be installed

########################################### Load data ################################################
#~# Load impact data
Pool <- read_excel(file.path("Data", "Species_pool.xlsx"), sheet = "Pool")
Literature <- read_excel(file.path("Data", "Assessment_information_FINAL.xlsx"), sheet = "Literature_searches")
Impacts <- read_excel(file.path("Data", "Assessment_information_FINAL.xlsx"), sheet = "Assessment_results_input")
Impact_Distribution <- read_excel(file.path("Data", "Assessment_information_FINAL.xlsx"), sheet = "Impact_distribution")
Insect_Distribution <- read_excel(file.path("Data","Alien_insect_distribution.xlsx"), sheet = "Insect_distribution")

#~# Load GADM level 0 shapefile
load(file.path("Data","lvl_0.RData"))

#Convert shapefile to sf object
lvl_0_sf <- st_as_sf(lvl_0)
rm(lvl_0)
######################################################################################################

source("Scripts/2.data_preparation.R")

source("Scripts/3.information_availability.R")
#Association between impact information availability and taxonomic order?
anova(fit.1, test = "Chisq")
anova(fit.2, fit.1, test = "Chisq")
pchisq(69.834, df = 17, lower.tail = F)
#The probability of seeing such a change in deviance (69.834) if the models really were no different is remote.

#Proportion of DD/nonDD species per order (Figure)
info_plot

source("Scripts/4.literature.R")
#Search returns vs impact studies (Figure)
lit_1
#Literature search returns vs max impact severity (Figure)
lit_2

source("Scripts/5.mechanism_severity.R")
#Impact mechanism and severity variation among taxonomic orders (Figures)
balloonMech 
balloonSev

source("Scripts/6.socioeconomic_pests.R")
#Search returns vs socioeconomic pest status
wilcox.test(SearchReturns ~ SEPest, data = Literature, conf.int = T)
#Sample sizes are large, therefore, approximate normal and find z value
#mean U
(180*172)/2 #15480
#standard deviation
sqrt((180*172*(180+172+1))/12) #954.327
#z value
(9609-15480)/954.327 #-6.151979
#9609 was the lowest U value of the two groups
#effect size
abs(-6.151979)/sqrt(352) #0.33. Medium effect size

#Are socio-economic pests more likely to have environmental impact information?
anova(fit.3, fit.4, test = "Chisq")
anova(fit.0, fit.3, test = "Chisq")
#no effect of socioeconomic pest

#Search returns and socioeconomic pest status (Figure)
SE_lit 

#DD vs nonDD for socioeconomic pest yes/no (Figure)
SE_info

source("Scripts/7.islands_mainland.R")
##Do islands have more harmful impacts than mainland?
anova(fit, test = "Chisq")
#Independence model fits well

#Islands vs Continents (Figure)
Isl_v_cont

source("Scripts/8.severity_range.R")
#Examining ordinality assumption (Figure)
ord_assum

#Ordinal logistic regression results
ctable <- cbind(ctable, "p value" = p)
ci <- confint(m)

#Comparing models to see the effect of AlienCountries
anova(m1,m) #m1 only contains intercept
#They are significantly different

#Variation and prediction of maximum impact severity as function of range (Figures)
sev_range_var
sev_range_pred
sev_range_comb #combined

source("Scripts/9.world_maps.R")
#Map of the world for number of alien insects per country
GRIIS_map

#All species where isInvasive == Invasive
isInvasive_map 

#All species where isInvasive == Null
notisInvasive_map

##All insect species in GRIIS (from my pool)
Pool_map

#All species where isInvasive == Invasive
Pool_isInvasive_map

#All species where isInvasive == Null
Pool_notisInvasive_map

##My impact evidence distribution
#Number of invasive countries (countries with evidence of impact)
Invasive_map






########################################### Data preparation #########################################



  


  


#~# Insect and Impact distribution



#Number of alien countries (from GRIIS)
Country_alien <- Insect_Distribution_min %>%
                 group_by(ISO3, ISO3.Country) %>%
                 distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
                 summarise(length(dwc.Taxon)) %>%
                 rename(GID_0 = "ISO3") %>%
                 rename(Richness = "length(dwc.Taxon)")

#Number of alien countries (from species pool)
Pool_GRIIS <- Pool %>% left_join(Insect_Distribution_min, by = "dwc.Taxon") %>%
  dplyr::select(dwc.Taxon, ISO3, ISO3.Country, isInvasive) %>%
  na.omit(Pool_GRIIS)

Country_alien_pool <- Pool_GRIIS %>%
                      group_by(ISO3, ISO3.Country) %>%
                      distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
                      summarise(length(dwc.Taxon)) %>%
                      rename(GID_0 = "ISO3") %>%
                      rename(Richness = "length(dwc.Taxon)")

#Number of invasive countries (countries with evidence of impact)
Country_invasive <- Impact_Distribution %>%
                    dplyr::select(scientificName, ISO3, Country, MaxSeverity) %>%
                    group_by(ISO3) %>%
                    dplyr::filter(MaxSeverity != "NA" & MaxSeverity != "DD") %>%
                    distinct(scientificName = scientificName, .keep_all = T) %>%
                    summarise(length(scientificName)) %>%
                    rename(Richness = "length(scientificName)") %>%
                    rename(GID_0 = "ISO3")


#Merging GRIIS and GADM
Alien_dist <- left_join(lvl_0_sf, Country_alien, by = "GID_0")
Alien_dist <- Alien_dist %>%
              dplyr::mutate(Richness = replace_na(Richness,0))

#Merging species pool and GADM
Pool_dist <- left_join(lvl_0_sf, Country_alien_pool, by = "GID_0")
Pool_dist <- Pool_dist %>%
             dplyr::mutate(Richness = replace_na(Richness,0))

#Merging Invasive countries and GADM
Inv_dist <- left_join(lvl_0_sf, Country_invasive, by = "GID_0") %>%
            mutate(Richness = replace_na(Richness,0))


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


#Number of invasive countries per species (i.e. countries with impacts)
# ImpDist <- Impact_Distribution %>%
#   dplyr::select(scientificName, ISO3, Country, MaxSeverity) %>%
#   mutate_if(is.character, as.factor) %>%
#   filter(MaxSeverity != "NA" & MaxSeverity != "DD") %>%
#   group_by(scientificName) %>%
#   summarise(length(Country))
# 
# 
# InsectDist <- left_join(InsDist, ImpDist, "scientificName")
# InsectDist <- left_join(InsectDist, Impacts.Max, "scientificName")
# InsectDist <- InsectDist %>% 
#   rename(AlienCountries = "length(Country).x") %>%
#   rename(InvasiveCountries = "length(Country).y") %>% 
#   rename(MaxSeverity = "Severity") %>%
#   dplyr::mutate(InvasiveCountries = replace_na(InvasiveCountries,0)) %>%
#   dplyr::mutate(MaxSeverity = replace_na(MaxSeverity, "DD")) %>%
#   mutate(PropInv = round(InvasiveCountries/AlienCountries,2))
######################################################################################################

# Section 2----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                  #
#              SECTION 2: Analysis                                 #
#                                                                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~# Number of insect species per family in GRIIS
Alien_families <- Insect_Distribution_min %>%
                  distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
                  group_by(dwc.order, dwc.family) %>% 
                  summarise(length(dwc.Taxon)) %>%
                  rename(Counts = "length(dwc.Taxon)")
                  

#~# Number of insect species per order in GRIIS
Alien_orders <- Insect_Distribution_min %>%
                distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
                group_by(dwc.order) %>% 
                summarise(length(dwc.Taxon)) %>%
                rename(Counts = "length(dwc.Taxon)") %>%
                rename(Order = "dwc.order")
  

#~# Number of species assessed per order
Orders <- Impacts %>% distinct(scientificName = scientificName, .keep_all = T) %>%
                        group_by(Order) %>% 
                        summarise(length(scientificName)) %>%
                        rename(Counts = "length(scientificName)") 


#~# Number of species assessed per order (minus NA species)
Orders_noNA <- Impacts.noNA %>% distinct(scientificName = scientificName, .keep_all = T) %>%
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
                        filter(Assessed_noNA > 0) %>%
                        dplyr::select(-GRIIS)



#~# Number of studies per species
Studies <- Impacts.nonDD %>% 
           group_by(Order, scientificName) %>%
           distinct(PubID = PubID, .keep_all = T) %>%
           summarise(length(PubID)) %>%
           rename(Counts = "length(PubID)")

                 



# Section 3----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                  #
#              SECTION 3: Figures                                  #
#                                                                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################################################################################

######################################################################################################


#Number of species assessed per order
ggplot(Orders, aes(x = reorder(Order, desc(Counts)), y = Counts)) +
  geom_col() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 9, angle = 45, vjust = 0.8, hjust = 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(breaks = c(seq(0,70,10)), 
                     labels = c(0,10,20,30,40,50,60,70), 
                     expand = c(0,0), 
                     limits = c(0,70)) +
  ylab("Number of species assessed") +
  xlab("Insect order")


#Proportion of DD/nonDD species per order
ggplot(DDvsNonDD, aes(x = reorder(Order, desc(Proportion)), y = Proportion, fill = Status)) +
       geom_bar(stat = "identity") + 
       facet_share(~Status, dir = "h", scales = "free", reverse_num = F) +
       coord_flip() +
       theme(axis.line = element_line(colour = "black"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank(),
             axis.title = element_text(size = 16),
             axis.text.y = element_text(size = 14, colour = "black"),
             axis.text.x = element_text(size = 14, colour = "black"),
             strip.text = element_text(size = 16),
             legend.title = element_text(size = 16),
             legend.text = element_text(size = 14)) +
       scale_fill_manual("Status", values = c("DD" = "black", "NotDD" = "orange")) +
       scale_y_continuous(breaks = c(-1.00, -0.75, -0.50, -0.25, 0.00, 0.00, 0.25, 0.50, 0.75, 1.00),
                          labels = c("1.00", "0.75", "0.50", "0.25", "0.00", "0.00", "0.25", "0.50", "0.75", "1.00")) +
  ylab("Proportion of species assessed") +
  xlab("Order") +
  geom_text(x=18, y=-1.03, label="6", size = 6) +
  #geom_text(x=18, y=0.08, label="0", size = 6) +
  geom_text(x=17, y=-1.03, label="6", size = 6) +
  #geom_text(x=17, y=0.08, label="0", size = 6) +
  geom_text(x=16, y=-1.03, label="2", size = 6) +
  #geom_text(x=16, y=0.08, label="0", size = 6) +
  geom_text(x=15, y=-1.03, label="1", size = 6) +
  #geom_text(x=15, y=0.08, label="0", size = 6) +
  geom_text(x=14, y=-1.03, label="2", size = 6) +
  #geom_text(x=14, y=0.08, label="0", size = 6) +
  geom_text(x=13, y=-1.03, label="2", size = 6) +
  #geom_text(x=13, y=0.08, label="0", size = 6) +
  geom_text(x=12, y=-1.03, label="5", size = 6) +
  #geom_text(x=12, y=0.08, label="0", size = 6) +
  geom_text(x=11, y=-1.00, label="18", size = 6) +
  geom_text(x=11, y=0.08, label="1", size = 6) +
  geom_text(x=10, y=-0.96, label="10", size = 6) +
  geom_text(x=10, y=0.11, label="1", size = 6) +
  geom_text(x=9, y=-0.93, label="9", size = 6) +
  geom_text(x=9, y=0.12, label="1", size = 6) +
  geom_text(x=8, y=-0.78, label="3", size = 6) +
  geom_text(x=8, y=0.28, label="1", size = 6) +
  geom_text(x=7, y=-0.75, label="19", size = 6) +
  geom_text(x=7, y=0.33, label="8", size = 6) +
  geom_text(x=6, y=-0.72, label="33", size = 6) +
  geom_text(x=6, y=0.37, label="16", size = 6) +
  geom_text(x=5, y=-0.71, label="32", size = 6) +
  geom_text(x=5, y=0.39, label="17", size = 6) +
  geom_text(x=4, y=-0.64, label="20", size = 6) +
  geom_text(x=4, y=0.45, label="14", size = 6) +
  geom_text(x=3, y=-0.48, label="4", size = 6) +
  geom_text(x=3, y=0.60, label="5", size = 6) +
  geom_text(x=2, y=-0.46, label="39", size = 6) +
  geom_text(x=2, y=0.65, label="59", size = 6) +
  geom_text(x=1, y=1.03, label="2", size = 6) #+
  #geom_text(x=1, y=-0.08, label="0", size = 6)

#Alternative
ggplot(species_assessed_min, aes(x = Assessed_noNA, y = DD, label = Order)) +
  geom_point() +
  geom_text(check_overlap = T, size = 5) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black")) +
  ylab("Proportion of data deficient species") +
  xlab("Number of species assessed")


###############################################################################################
#Randomly choosing species to assess
species2choose <- read_csv("Data/Species2choose.csv")

Hymenoptera <- species2choose %>% 
  filter(Order == "Hymenoptera") %>%
  dplyr::select(scientificName)
(Hymenoptera <- sample(Hymenoptera$scientificName, size = 20, replace = F))

Hemiptera <- species2choose %>% 
  filter(Order == "Hemiptera") %>%
  dplyr::select(scientificName)
(Hemiptera <- sample(Hemiptera$scientificName, size = 20, replace = F))

Lepidoptera <- species2choose %>% 
  filter(Order == "Lepidoptera") %>%
  dplyr::select(scientificName)
(Lepidoptera <- sample(Lepidoptera$scientificName, size = 20, replace = F))

Diptera <- species2choose %>% 
  filter(Order == "Diptera") %>%
  dplyr::select(scientificName)
(Diptera <- sample(Diptera$scientificName, size = 20, replace = F))

Coleoptera <- species2choose %>% 
  filter(Order == "Coleoptera") %>%
  dplyr::select(scientificName)
(Coleoptera <- sample(Coleoptera$scientificName, size = 20, replace = F))

Psocodea <- species2choose %>% 
  filter(Order == "Psocodea") %>%
  dplyr::select(scientificName)
(Psocodea <- sample(Psocodea$scientificName, size = 20, replace = F))

