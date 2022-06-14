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

########################################### Load data ###############################################
#~# Load impact data
Pool <- read_excel(file.path("Data", "Species_pool.xlsx"), sheet = "Pool")
Literature <- read_excel(file.path("Data", "Assessment_information_FINAL.xlsx"), sheet = "Literature_searches")
Impacts <- read_excel(file.path("Data", "Assessment_information_FINAL.xlsx"), sheet = "Assessment_results_input")
Impact_Distribution <- read_excel(file.path("Data", "Assessment_information_FINAL.xlsx"), sheet = "Impact_distribution")
# Distribution information obtained from Global Register of Introduced and Invasive Species (https://www.griis.org)
Insect_Distribution <- read_excel(file.path("Data","Alien_insect_distribution.xlsx"), sheet = "Insect_distribution")

#~# Load GADM level 0 shapefile (https://www.gadm.org)
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
# balloonMech 
# balloonSev
# mech_sev_comb
mechnsev

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

