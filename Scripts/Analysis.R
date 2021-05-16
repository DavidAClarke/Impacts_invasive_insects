#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              TITLE: Environmental impacts of alien and invasive insects                        #
#              PhD CHapter: 2                                                                    #
#              Date started:                                                                     #
#              Author: David A Clarke                                                            #
#                                                                                                #         
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Chapter/project description           (Code needs to be cleaned up)

# Section 1----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                                                #
#              SECTION 1: Setup for analysis                                                     #
#                                                                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~# Clears the environment
rm(list=ls())

#Required libraries
library(tidyverse)
library(janitor)
library(sf)
library(readxl)
library(tmap)
#library(magick)
#library(cowplot)
#library(rsvg)
library(ggpol)
library(ggpubr)
#library(ggalluvial)
library(MASS)
#library(FactoMineR)
#library(factoextra)
#library(igraph)

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


########################################### Data preparation #########################################
#~# Species pool
Pool <- Pool %>% rename(dwc.Taxon = `Species name`)



#~# Literature data
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
                  filter(Evidence != 0)

Literature_SE <- Literature %>%
                 dplyr::select(`SearchReturns`, `SEPest`) %>%
                 arrange(SearchReturns) %>%
                 mutate(Rank = seq(1:length(SearchReturns)))

RankSums <- Literature_SE %>%
            dplyr::group_by(SEPest) %>%
            summarise(sum(Rank))

#~# Impact data preparation
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
  filter(Severity != "NA")


#Selecting only those species who have evidence of impact i.e. removing data deficient species
Impacts.nonDD <- Impacts.noNA %>% 
  filter(Severity == "MC" | 
         Severity == "MN" |
         Severity == "MO" | 
         Severity == "MR") %>%
  mutate(Order = factor(Order, levels = c("Blattodea","Coleoptera","Dermaptera",
                                          "Diptera","Hemiptera","Hymenoptera",
                                          "Lepidoptera","Mantodea","Psocodea",
                                          "Siphonaptera","Thysanoptera"))) %>%
  mutate(LandmassType = factor(LandmassType, levels = c("Continent", "Island")))


#Selecting only data deficient species
Impacts.DD <- Impacts.noNA %>% filter(Severity == "DD")


#Selecting only maximum impact information
Impacts.Max <- Impacts.noNA %>% 
               filter(GlobalMaxSeverity == "Yes") #%>%
               #distinct(scientificName = scientificName, .keep_all = T) %>%
               #dplyr::select(Order, scientificName, Severity)
Impacts.Max.min <- Impacts.Max %>% distinct(scientificName = scientificName, .keep_all = T)
  
Literature_min2 <- left_join(Literature_min,Impacts.Max.min, by = "scientificName")
Literature_min2 <- Literature_min2 %>%
                   dplyr::select(Order.x, scientificName, SearchReturns, Severity) %>%
                   dplyr::mutate(Severity = factor(Severity, levels = c("MC","MN","MO","MR")))

#~# Insect and Impact distribution
#Alien insect distribution (all insect species in GRIIS)
Insect_Distribution_min <- Insect_Distribution %>%
                           dplyr::select(dwc.order, dwc.family, dwc.Taxon, ISO3, ISO3.Country) %>%
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
                           


#Number of alien countries (from GRIIS)
Country_alien <- Insect_Distribution_min %>%
                 group_by(ISO3, ISO3.Country) %>%
                 distinct(dwc.Taxon = dwc.Taxon, .keep_all = T) %>%
                 summarise(length(dwc.Taxon)) %>%
                 rename(GID_0 = "ISO3") %>%
                 rename(Richness = "length(dwc.Taxon)")

#Number of alien countries (from species pool)
Pool_GRIIS <- left_join(Pool, Insect_Distribution_min, by = "dwc.Taxon")
Pool_GRIIS <- Pool_GRIIS %>% 
              dplyr::select(dwc.Taxon, ISO3, ISO3.Country) %>%
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
                    filter(MaxSeverity != "NA" & MaxSeverity != "DD") %>%
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
            filter(MaxSeverity != "NA") %>%
            group_by(scientificName) %>%
            summarise(length(ISO3))

MaxSev_range <- left_join(Impacts.Max, Ins_Dist, by = "scientificName")
MaxSev_range <- MaxSev_range %>%
                rename(AlienCountries = "length(ISO3)") %>%
                mutate(AlienCountries = replace_na(AlienCountries,30)) %>% #for some reason the number did not come across with join
                mutate(Severity = ordered(factor(Severity, levels = c("MC", "MN", "MO", "MR")))) %>%
                dplyr::select(AlienCountries, Severity)


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

#For plot (test)
DDvsNonDD <- species_assessed_min %>%
             dplyr::select(Order, DD, NotDD) %>%
             mutate(DD = DD * -1) %>%
             gather("Status", "Proportion", -Order) %>%
             arrange(Proportion)



#~# Number of studies per species
Studies <- Impacts.nonDD %>% 
           group_by(Order, scientificName) %>%
           distinct(PubID = PubID, .keep_all = T) %>%
           summarise(length(PubID)) %>%
           rename(Counts = "length(PubID)")

                 ## Can probably delete the commented out section below ##
#~# Number of DD among order (excluding any NA species i.e. not alien)
# Orders.noNA <- Impacts.noNA %>% distinct(scientificName = scientificName, .keep_all = T) %>%
#                               group_by(Order) %>% 
#                               summarise(length(scientificName)) %>%
#                               rename(Counts = "length(scientificName)")
# 
# 
# Orders.DD <- Impacts.DD %>% distinct(scientificName = scientificName, .keep_all = T) %>%
#                           group_by(Order) %>% 
#                           summarise(length(scientificName)) %>%
#                           rename(Count.DD = "length(scientificName)")
# 
# Data_Deficient <- data.frame(Order = c("Blattodea", "Coleoptera", "Dermaptera", "Diptera","Embioptera",
#                                        "Hemiptera", "Hymenoptera", "Lepidoptera", "Mantodea","Neuroptera",
#                                        "Odonata", "Orthoptera", "Phasmida","Psocodea", "Siphonaptera",
#                                        "Thysanoptera", "Zygentoma"),
#                              DD = c(4,27,8,17,4,26,36,13,0,2,1,2,5,13,3,9,6),
#                              Insects = c(9,40,9,24,4,37,67,18,2,2,1,2,5,14,4,10,6))
# 
# Data_Deficient <- mutate(Data_Deficient,
#                          Non.DD = Insects - DD,
#                          prop.DD = round(DD/Insects,2), 
#                          prop.NonDD = round(Non.DD/Insects,2))
# 
# Order <- c(rep("Blattodea", 9), 
#            rep("Coleoptera", 40), 
#            rep("Dermaptera", 9), 
#            rep("Diptera",24), 
#            rep("Embioptera", 4), 
#            rep("Hemiptera", 37), 
#            #rep("Hymenoptera", 67),
#            rep("Lepidoptera", 18), 
#            rep("Mantodea", 2),
#            rep("Neuroptera", 2), 
#            rep("Odonata", 1), 
#            rep("Orthoptera", 2),
#            rep("Phasmida", 5), 
#            rep("Psocodea", 14), 
#            rep("Siphonaptera", 4), 
#            rep("Thysanoptera", 10), 
#            rep("Zygentoma", 6))
# 
# DD <- c(rep("Y", 4), rep("N", 5), 
#         rep("Y", 27), rep("N", 13), 
#         rep("Y", 8), rep("N", 1), 
#         rep("Y", 17), rep("N",7),
#         rep("Y", 4), rep("N", 0), 
#         rep("Y", 26), rep("N", 11), 
#         #rep("Y", 36), rep("N", 31), 
#         rep("Y", 13), rep("N", 5), 
#         rep("Y", 0), rep("N",2), 
#         rep("Y", 2), rep("N", 0),
#         rep("Y", 1), rep("N", 0),
#         rep("Y", 2), rep("N",0),
#         rep("Y", 5), rep("N",0), 
#         rep("Y", 13), rep("N",1), 
#         rep("Y", 3), rep("N",1),
#         rep("Y", 9), rep("N",1),
#         rep("Y", 6), rep("N",0))

DDtab <- table(Order, DD)
chisq.test(DDtab)
chisq.test(DDtab, simulate.p.value = T)
#the following gives the same result
prop.test(Data_Deficient$DD, Data_Deficient$Insects, alternative = "two.sided")

##log-linear analysis
Order_assessed <- species_assessed_min %>%
                  dplyr::select(Order, Evidence, DataDeficient) %>%
                  rename(NotDD = "Evidence") %>%
                  rename(DD = "DataDeficient") %>%
                  gather(key = "Assessed", value = "Number", -Order)


fit.1 <- glm(Number ~ factor(Order) * factor(Assessed), data = Order_assessed, family = poisson)
fit.2 <- glm(Number ~ factor(Order) + factor(Assessed), data = Order_assessed, family = poisson)
pchisq(deviance(fit.2), df = df.residual(fit.2), lower.tail = F) 
#reject the null that the cell frequencies satisfy the given loglinear model
anova(fit.1, test = "Chisq")
anova(fit.2, fit.1, test = "Chisq")
pchisq(69.834, df = 17, lower.tail = F)
#significant interaction between order and DD. 
#The probability of seeing such a change in deviance (69.834) if the models really were no different is remote.


#Are socio-economic pests more likely to have environmental impact information
Impacts.SE <- Impacts.noNA %>% distinct(scientificName = scientificName, .keep_all = T) %>%
                               mutate(Severity = ifelse(Severity == "DD", "DD", "NotDD")) %>%
                               mutate_if(is.character, as.factor) %>% 
                               dplyr::select(SocioEconomicPest, Severity) %>%
                               count(SocioEconomicPest, Severity) %>%
                               mutate(Props = c(0.66, 0.34, 0.60, 0.40))

Impacts.SE_Ord <- Impacts.noNA %>% 
                  distinct(scientificName = scientificName, .keep_all = T) %>%
                  mutate(Severity = ifelse(Severity == "DD", "DD", "NotDD")) %>%
                  mutate_if(is.character, as.factor) %>% 
                  dplyr::select(Order, SocioEconomicPest, Severity) %>%
                  count(Order, SocioEconomicPest, Severity)

#loglinear analysis
fit.3 <- glm(n ~ factor(Severity) + factor(SocioEconomicPest), data = Impacts.SE, family = poisson)
fit.4 <- glm(n ~ factor(Severity) * factor(SocioEconomicPest), data = Impacts.SE, family = poisson)
fit.0 <- glm(n ~ factor(Severity), data = Impacts.SE, family = poisson)
summary(fit.3)
summary(fit.4)
anova(fit.3, fit.4, test = "Chisq")
anova(fit.0, fit.3, test = "Chisq")
#no effect of socioeconomic pest

#Search returns vs socioeconomic pest status
wilcox.test(SearchReturns ~ SEPest, data = Literature, conf.int = T)
#(U = , n1 = , n2 = , p < 0.05)
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

#Dividing data into training and test set
#Random sampling 
# samplesize <- 0.75*nrow(MaxSev_range)
# set.seed(100)
# index <- sample(seq_len(nrow(MaxSev_range)), size = samplesize)
# #Creating training and test set 
# datatrain <- MaxSev_range[index,]
# datatest <- MaxSev_range[-index,]

#Plotting mean of X for each level of ordinal variable Y
#Graphically testing assumption of ordinality under proportional odds
rms::plot.xmean.ordinaly(MaxSev_range$Severity ~ MaxSev_range$AlienCountries, cr=FALSE)
#Looks pretty good to me

m <- polr(Severity ~ AlienCountries, data = MaxSev_range, Hess = TRUE)
summary(m)
#For every 1 unit increase in the number of countries, the log odds of having increasingly severe
#maximum impact decreases by 0.01. However, this is not significant (95% CI -0.04323880, 0.02249485)
#i.e. CI includes 0. Also the odds ratio is 0.9899607 i.e. not very different from 1
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(m)


#Estimate probabilities at fixed settings of range
#Mean number of countries
mean_range <- predict(m, data.frame(AlienCountries = mean(MaxSev_range$AlienCountries)), type = "probs")

#Median number of countries
median_range <- predict(m, data.frame(AlienCountries = median(MaxSev_range$AlienCountries)), type = "probs")

#Minimum number of countries
min_range <- predict(m, data.frame(AlienCountries = min(MaxSev_range$AlienCountries)), type = "probs")

#Maximum number of countries
max_range <- predict(m, data.frame(AlienCountries = max(MaxSev_range$AlienCountries)), type = "probs")

#Combining estimated probabilities
Est_probs <- rbind(mean_range, median_range, min_range, max_range)


#Model with only the intercept
m1 <- polr(Severity ~ 1, data = MaxSev_range, Hess = TRUE)
summary(m1)

#Comparing models to see the effect of AlienCountries
anova(m1,m)
#it is significant, however, I wouldn't really accept it given the log odds of the coeffecient
#and the high amount of variability shown in the boxplot.


# predictSeverity <- predict(m,newdat)
# table(MaxSev_range$Severity, predictSeverity)
# mean(as.character(MaxSev_range$Severity) != as.character(predictSeverity))


#Setting up for making predictions of maximum severity from how widespread a species is
newdat <- data.frame(AlienCountries = seq(from = 1, to = 125))
newdat <- cbind(newdat, predict(m, newdat, type = "probs"))
head(newdat)
lnewdat <- reshape2::melt(newdat, id.vars = c("AlienCountries"),
                variable.name = "Level", value.name="Probability")
head(lnewdat)


##Do islands have more harmful impacts than mainland (move this code further up)
#Only using studies once; removed laboratory-only studies
Impact_landmass <- Impacts.nonDD %>%
                   distinct(PubID = PubID, .keep_all = T) %>%
                   filter(StudyType != "Laboratory") %>%
                   mutate(Severity = ifelse(Severity == "MO"| Severity == "MR", "Harmful", "NotHarmful")) %>%
                   mutate_if(is.character, as.factor) %>% 
                   dplyr::select(LandmassType, Severity) %>%
                   count(LandmassType, Severity)

#loglinear analysis
fit.5 <- glm(n ~ factor(Severity) + factor(LandmassType), data = Impact_landmass, family = poisson)
pchisq(deviance(fit.5), df = df.residual(fit.5), lower.tail = F)
anova(fit.5, test = "Chisq")
#Independence model fits well




#Multiple correspondence analysis
#Goal MCA: Helps to identify the observations which have a similar profile and 
#also identifies assosiations between the variable categories, i.e assosiations within the levels.
#Need to use distinct study ID's. Maybe I could include more of my variables, 
#even if I have already used them in other analyses
Impacts_MCA_v1 <- Impacts.nonDD %>% 
                  distinct(PubID = PubID, .keep_all = T) %>%
                  dplyr::select(Order, Mechanism, Severity)
res.mca.v1 <- MCA(Impacts_MCA_v1, graph = F)
                
Impacts_MCA_v2 <- Impacts.nonDD %>% 
                  distinct(PubID = PubID, .keep_all = T) %>%
                  dplyr::select(Order, Mechanism, Severity, LandmassType, SocioEconomicPest)
res.mca.v2 <- MCA(Impacts_MCA_v2, graph = F)

#~# Number of species per country and/or number of impact studies per country
ImpDist <- EICAT.nonDD %>% 
  group_by(Country) %>%
  #including the following line will get the number of different species per country
  #otherwise its the total number of impact evidence per country
  distinct(scientificName = scientificName, .keep_all = T) %>% 
  summarise(length(scientificName)) %>%
  rename(Counts = "length(scientificName)")

#~# Number of countries with impact evidence per species
SpecDist <- EICAT.nonDD %>%
  group_by(scientificName) %>%
  distinct(Country = Country, .keep_all = T) %>%
  summarise(length(Country))

# Section 3----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                  #
#              SECTION 3: Figures                                  #
#                                                                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Network plot for order, mechanism, severity (test) (requires iGraph)
# test <- Impacts.nonDD %>% 
#         dplyr:: select(Order, Mechanism, Severity) %>% 
#         unite("z", Order, Mechanism, Severity, sep = ";")
# df <- read.csv2(text = test$z, header = F)
# m <- as.matrix(df)
# g <- graph_from_edgelist(rbind(m[,1:2], m[,2:3]), directed = F)
# l <- layout_with_sugiyama(g, ceiling(match(V(g)$name, m)/nrow(m)))
# plot(g, layout=-l$layout[,2:1])
# 
# 
# test2 <- Impacts.nonDD %>% 
#          dplyr:: select(Order, Mechanism) %>%
#          group_by(Order, Mechanism) %>%
#          count(Order, Mechanism)
# test2.df <- xtabs(n ~ Order + Mechanism, test2)
# 
# 
# test3 <- Impacts.nonDD %>% 
#          dplyr:: select(Order, Mechanism, Severity) %>%
#          group_by(Order, Mechanism, Severity) %>%
#          summarise(weight = n()) %>% 
#          ungroup()
# 
# Node_Order <- Impacts.nonDD %>%
#               dplyr::select(Order) %>%
#               distinct(Order = Order, .keep_all = T) %>%
#               rowid_to_column("id")
# 
# Node_Mechanism <- Impacts.nonDD %>%
#                   dplyr::select(Mechanism) %>%
#                   distinct(Mechanism = Mechanism, .keep_all = T) %>%
#                   rowid_to_column("id")
# 
# Node_Severity <- Impacts.nonDD %>%
#                  dplyr::select(Severity) %>%
#                  distinct(Severity = Severity, .keep_all = T) %>%
#                  rowid_to_column("id")
# 
# edges <- test3 %>% 
#          left_join(Node_Order, by = c("Order")) %>% 
#          rename(from = id)
# edges <- edges %>% 
#          left_join(Node_Mechanism, by = c("Mechanism")) %>% 
#          rename(fromto = id)
# edges <- edges %>% 
#          left_join(Node_Severity, by = c("Severity")) %>% 
#          rename(to = id)
# 
# edges <- dplyr::select(edges, from, fromto, to, weight)
# 
# routes_tidy <- tbl_graph(nodes = Node_Order, edges = edges, directed = TRUE)


#Search returns vs impact studies
ggplot(Literature, aes(x = SearchReturns, y = Evidence)) +
  geom_point() +
  scale_x_log10() +
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
  ylab("Number of impact studies") +
  xlab("Number of literature search returns (log10)")

#Literature search returns vs max impact severity
MaxSev <- Impacts.Max %>% 
          distinct(scientificName = scientificName, .keep_all = T) %>%
          dplyr::select(Severity)
Literature_min_imp <- cbind(Literature_min, MaxSev)            


ggplot(Literature_min_imp, aes(x = SearchReturns, y = Severity)) +
  geom_boxplot() +
  geom_point() +
  scale_x_log10() +
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
  xlab("Number of search returns (log10)") +
  ylab("Maximum impact severity")

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


#Map of the world for number of alien insects per country
Alien_dist_NA <- Alien_dist[-12,] #removing Antarctica
Num_species <- Alien_dist_NA %>%
               filter(Richness > 0)
tmap_mode("plot")
GRIIS_map <- tm_shape(Alien_dist_NA) +
                    tm_fill("white") +
                    tm_borders("lightgrey", lwd = 0.5) +
             tm_shape(Num_species) +
                    tm_polygons("Richness", palette = "-viridis", alpha = 0.8, 
                                breaks = c(1,2,5,10,20,50,150,500),
                                legend.is.portrait = FALSE) +
                    tm_layout(frame = T,
                              legend.width = 1,
                              legend.text.size = 0.8) +
                    tm_format("World")
#tmap_save(GRIIS_map, "Outcome/PDF/GRIIS_map2.pdf", width = 1920, height = 1080, asp = 0)

#Map of the world for number of alien insects per country from species pool
Pool_dist_NA <- Pool_dist[-12,] #removing Antarctica
Num_species <- Pool_dist_NA %>%
               filter(Richness > 0)
tmap_mode("plot")
Pool_map <- tm_shape(Pool_dist_NA) +
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
#tmap_save(Pool_map, "Outcome/PDF/Pool_map.pdf", width = 1920, height = 1080, asp = 0)

#Map of the world for number of invasive insects per country
Inv_dist_NA <- Inv_dist[-12,] #removing Antarctica
Num_Inv_species <- Inv_dist_NA %>%
               filter(Richness > 0)
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
#tmap_save(Invasive_map, "Outcome/PDF/Invasive.pdf", width = 1920, height = 1080, asp = 0)

#Both maps together
maps2 <- tmap_arrange(Pool_map, Invasive_map, ncol = 1)
tmap_save(maps2, "Outcome/PDF/maps2.pdf")



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


# DD_long <- Data_Deficient %>% gather(key = "Info", "value", -c(1:4))
# ggplot(DD_long, aes(Order, value, fill = Info)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.title.x = element_text(size = 12),
#         axis.text.x = element_text(size = 9, 
#                                    angle = 45,
#                                    vjust = 0.8,
#                                    hjust = 0.9),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 12)) +
#   scale_fill_manual(values = c("grey", "black"), 
#                     name = "Information\n available", 
#                     breaks = c("prop.DD", "prop.NonDD"), 
#                     labels = c("No", "Yes")) +
#   scale_y_continuous(breaks = c(seq(0.00,1.00,0.25)), 
#                      labels = c(0,25,50,75,100), 
#                      expand = c(0,0), 
#                      limits = c(0.00,1.00)) +
#   ylab("Species with vs without impact information (%)") +
#   xlab("Insect order")

#DD vs nonDD for socioeconomic pest yes/no (bar chart)
# ggplot(Impacts.SE, aes(SocioEconomicPest, Props, fill = Severity)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 12)) +
#   scale_fill_manual(values = c("orange","darkolivegreen4"), 
#                     name = "Information\navailable", 
#                     breaks = c("DD", "NotDD"), 
#                     labels = c("DD", "NotDD")) +
#   scale_y_continuous(breaks = c(seq(0.00,1.00,0.25)), 
#                      labels = c(0,25,50,75,100), 
#                      expand = c(0,0), 
#                      limits = c(0.00,1.00)) +
#   ylab("Species with vs without impact information (%)") +
#   xlab("Socio-economic pest")

#DD vs nonDD for socioeconomic pest yes/no (bar chart)
ggplot(Impacts.SE, aes(x = SocioEconomicPest, y = n, fill = Severity)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black")) +
  scale_fill_manual(values = c("orange","darkolivegreen4"), 
                    name = "Information\navailable", 
                    breaks = c("DD", "NotDD"), 
                    labels = c("DD", "NotDD")) +
  scale_y_continuous(breaks = c(seq(0,120,20)), 
                     labels = c(0,20,40,60,80,100,120), 
                     expand = c(0,0), 
                     limits = c(0.00,120)) +
  ylab("Species with vs without impact information") +
  xlab("Socio-economic pest") +
  geom_text(x = 0.80, y = 115, label = "66%", size = 6) +
  geom_text(x = 1.22, y = 62, label = "34%", size = 6) +
  geom_text(x = 1.80, y = 104, label = "60%", size = 6) +
  geom_text(x = 2.22, y = 71, label = "40%", size = 6)

#AND/OR (distinguishing by order)
Impacts.SE_Ord_min <- Impacts.SE_Ord %>% filter(Order != "Embioptera" &
                                                Order != "Ephemeroptera" &
                                                Order != "Mantodea" &
                                                Order != "Neuroptera" &
                                                Order != "Odonata" &
                                                Order != "Orthoptera" &
                                                Order != "Phasmatodea" &
                                                Order != "Siphonaptera" &
                                                Order != "Thysanoptera" &
                                                Order != "Zygentoma") %>%
                      dplyr::group_by(Order) %>%
                      arrange(desc(n), .by_group = TRUE)


ggplot(Impacts.SE_Ord_min, aes(y = n, axis1 = Order, axis2 = Severity,label = after_stat(stratum))) +
      geom_alluvium(aes(fill = SocioEconomicPest), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey", position = "identity") +
      geom_label(stat = "stratum", size = 4) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            axis.title.y = element_text(size = 16),
            axis.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
      scale_x_discrete(limits = c("Order", "Information"), expand = c(.05, .05)) +
      scale_fill_manual(values = c("darkblue","lightblue")) +
  ylab("Number of species")

#Search returns and socioeconomic pest status
ggplot(Literature, aes(x = SEPest, y = SearchReturns)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab("Socioeconomic pest status") +
  ylab("Number of search returns (log10)")
        
        

##Max severity vs range
#boxplot
ggplot(MaxSev_range, aes(x = Severity, y = AlienCountries)) +
       geom_boxplot(aes(colour = Severity)) +
       geom_jitter() +
       scale_y_log10() +
  coord_flip()+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("Number of countries with alien populations(log10)")


#Predicting maximum severity from how widespread a species is
my_cols <- c("darkorchid4", "dodgerblue4", "aquamarine4", "yellow2")
ggplot(lnewdat, aes(x = AlienCountries, y = Probability, colour = Level)) +
       geom_line() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  scale_color_manual(name = "Impact\nSeverity", 
                    labels = c("Minimal Concern", "Minor", "Moderate", "Major"),
                    values = my_cols) +
  xlab("Number of countries with alien populations")


# Islands vs Continents
IslvsCont <- data.frame(counts = c(82, 23, 174, 61),
                      Severity = c("Harmful", "Harmful", "NotHarmful", "NotHarmful"),
                      Type = c("Continent", "Island", "Continent", "Island"),
                      props = c(0.3203125, 0.2738095, 0.6796875, 0.7261905))


Impact_landmass_plot <- Impacts.nonDD %>%
                        distinct(PubID = PubID, .keep_all = T) %>%
                        filter(StudyType != "Laboratory") %>%
                        mutate(Severity = ifelse(Severity == "MO"| Severity == "MR", "Harmful", "NotHarmful")) %>%
                        mutate_if(is.character, as.factor) %>% 
                        dplyr::select(LandmassType, Severity) %>%
                        count(LandmassType, Severity) %>%
                        mutate(Props = c(0.34, 0.66, 0.34, 0.66))
                        

ggplot(Impact_landmass_plot, aes(LandmassType, n, fill = Severity)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14, colour = "black"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  scale_fill_manual(values = c("tan", "lightblue"), 
                    name = "Impact\nseverity", 
                    breaks = c("Harmful", "NotHarmful"), 
                    labels = c("Harmful", "NotHarmful")) +
  scale_y_continuous(breaks = c(seq(0,300,50)), 
                     labels = c(0,50,100,150,200,250,300), 
                     expand = c(0,0), 
                     limits = c(0,300)) +
  ylab("Harmful vs Not harmful impacts") +
  xlab("Landmass Type") +
  geom_text(x = 0.8, y = 137, label = "66%", size = 6) +
  geom_text(x = 1.23, y = 260, label = "34%", size = 6) +
  geom_text(x = 1.8, y = 59, label = "66%", size = 6) +
  geom_text(x = 2.23, y = 107, label = "34%", size = 6) 
  
#Number of each severity instance (from every reference) (bar chart)
ggplot(EICAT, aes(Severity)) +
  geom_bar() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(breaks = c(seq(0,120,20)), 
                     labels = c(0,20,40,60,80,100,120), 
                     expand = c(0,0), 
                     limits = c(0,120)) +
  xlab("Impact severity") +
  ylab("Number of impact assessments")

#Number of each mechanism instance (from every reference) (bar chart)
Impacts.nonDD <- Impacts.nonDD %>% 
  mutate(Mechanism = stringr::str_replace(Mechanism, "Competition", "Com")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Predation", "Pre")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Hybridisation", "Hyb")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Transmission of disease", "Tra")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Other", "Oth")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Interaction with other alien species", "Int")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Herbivory", "Her")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Parasitism", "Par")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Poisoning/toxicity", "Poi")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Facilitation of native species", "Fac")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Chemical/physical/structural impact on ecosystem", "Che"))

ggplot(EICAT.nonDD, aes(Mechanism)) +
  geom_bar() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(breaks = c(seq(0,120,20)), 
                     labels = c(0,20,40,60,80,100,120), 
                     expand = c(0,0), 
                     limits = c(0,120)) +
  xlab("Impact mechanism") +
  ylab("Number of impact assessments")

#Number of each severity instance per order
#(from every reference; facet_wrap; bar chart)
Ord.mag <- ggplot(EICAT, aes(Severity)) +
       geom_bar() +
       facet_wrap(~Order) +
       theme_bw() +
       theme(axis.line = element_line(colour = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(),
                         axis.title.x = element_text(size = 12),
                         axis.title.y = element_text(size = 12)) +
       scale_y_continuous(breaks = c(seq(0,60,20)), 
                         labels = c(0,20,40,60), 
                         expand = c(0,0), 
                         limits = c(0,60)) +
       xlab("Impact severity") +
       ylab("Number of impact assessments")

#OR a Balloon plot 

ImpactSeverity <- Impacts.nonDD %>% 
                  distinct(PubID = PubID, .keep_all = T) %>%
                  count(Order, Severity) %>%
                  group_by(Severity) %>%
                  add_tally(n, name = "TotalSev") %>%
                  group_by(Order) %>%
                  add_tally(n, name = "TotalOrd") %>%
                  mutate(PropSev = round(n/TotalSev, 2)) %>%
                  mutate(PropOrd = round(n/TotalOrd, 2))

#Between Orders
balloonSev <- ggballoonplot(ImpactSeverity, size = "PropSev", fill = "n")+
  scale_fill_viridis_c(option = "C", begin = 1, end = 0) +
  theme_bw() +
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2)) +
  ylab("Impact severity") 

#Within Orders
balloonSev_Ord <- ggballoonplot(ImpactSeverity, size = "PropOrd", fill = "n")+
  scale_fill_viridis_c(option = "C", begin = 1, end = 0) +
  theme_bw() +
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2)) +
  ylab("Impact severity") 

#par(mar = c(5,5,5,5))
plot(gridExtra::arrangeGrob(balloonSev, balloonSev_Ord, nrow = 1, ncol = 2))

#Number of each mechanism instance per order
#(from every reference; facet_wrap; bar chart)
Ord.mec <- ggplot(EICAT.nonDD, aes(Mechanism)) +
  geom_bar() +
  facet_wrap(~Order) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 9, angle = 90, vjust = 0.8,hjust = 0.5), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(breaks = c(seq(0,30,10)), 
                     labels = c(0,10,20,30), 
                     expand = c(0,0), 
                     limits = c(0,30)) +
  xlab("Impact mechanism") +
  ylab("Number of impact studies")

#OR a Balloon plot

ImpactMechanism <- Impacts.nonDD %>% 
                   distinct(PubID = PubID, .keep_all = T) %>%
                   count(Order, Mechanism) %>%
                   group_by(Mechanism) %>%
                   add_tally(n, name = "TotalMech") %>%
                   group_by(Order) %>%
                   add_tally(n, name = "TotalOrd") %>%
                   mutate(PropMech = round(n/TotalMech, 2)) %>%
                   mutate(PropOrd = round(n/TotalOrd, 2))

#Between Orders
balloonMech <- ggballoonplot(ImpactMechanism, size = "PropMech", fill = "n")+
  scale_fill_viridis_c(option = "D", begin = 1, end = 0) +
  theme_bw() +
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2)) +
  ylab("Impact mechanism")

#Within Orders
balloonMech_Ord <- ggballoonplot(ImpactMechanism, size = "PropOrd", fill = "n")+
  scale_fill_viridis_c(option = "D", begin = 1, end = 0) +
  theme_bw() +
  theme(axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 12, angle = 45, vjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2)) +
  ylab("Impact mechanism")

#par(mar = c(5,5,5,5))
plot(gridExtra::arrangeGrob(balloonMech, balloonMech_Ord, nrow = 1, ncol = 2))
#Number of each max severity (n = number of nonDD species) (bar chart)

#Number of each max mechanism (n = number of nonDD species) (bar chart)

# MCA plots
fviz_mca_var(res.mca.v2, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal(),
             col.var = "cos2") #squared cosine (cos2) is the quality of representation of variable categories

fviz_mca_var(res.mca.v1, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal(),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) #contribution of variable categories to definition of the dimensions

# Contributions of rows to dimension 1
fviz_contrib(res.mca.v2, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca.v2, choice = "var", axes = 2, top = 15)

##Using ggplot for biplot (from: https://rpubs.com/gaston/MCA))

cats <- apply(Impacts_MCA_v2, 2, function(x) nlevels(as.factor(x)))

res.mca.v2.varsdf <- data.frame(res.mca.v2$var$coord, Variable = rep(names(cats), cats)) #variables
res.mca.v2.obsdf <- data.frame(res.mca.v2$ind$coord) #individuals

ggplot(data = res.mca.v2.varsdf, aes(x = Dim.1, y = Dim.2)) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_point(colour = "gray50", alpha = 0.7) + 
  geom_density2d(colour = "gray80") + 
  geom_text(data = res.mca.v2.varsdf, aes(x = Dim.1, y = Dim.2, label = rownames(res.mca.v2.varsdf), colour = Variable)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ggtitle("MCA plot of variables") + 
  scale_colour_discrete(name = "Variable") +
  xlab("Dim. 1 (8.4%)") +
  ylab("Dim. 2 (6.8%)")

####################################################################################################
# Miscellaneous code

# #~# Finding which species had no occurrences
# species <- unique(sppOcc.min$name)
# NoOcc <- setdiff(sppList, species) 
# 
# 
# #~# If you switched the order, you get those with synonyms/different spelling
# Syns <- setdiff(species,sppList)
# 
# #~# Need to convert lat and long to numeric
# sppOcc.min$decimalLongitude <- as.numeric(sppOcc.min$decimalLongitude)
# sppOcc.min$decimalLatitude <- as.numeric(sppOcc.min$decimalLatitude)
# 
# #~# Changing coordinate column names
# #This is done in order for the coordinate matching function to work
# colnames(sppOcc.min)[3:4] <- c("Long", "Lat")
# save(sppOcc.min, file = "Outcome/sppOcc.min.RData", compress = "xz")
#
# #~# Matching occurrence lat/longs to eco-region polygons (or any polygon; this is NOT from sf)
# ero <- xy_match(sppOcc.min, EcoOcc.pre, ecoreg)
# save(ero, file = "Outcome/ero.RData", compress = "xz")
# 
# #~# Removing rows where ECO_NAME is NA
# #!is.na(ero$ECO_NAME) means all the values of ECO_NAME that are NOT(!) "NA"
# ero.NoNA <- ero[!is.na(ero$ECO_NAME),]
# save(ero.NoNA, file = "Outcome/ero.NoNA.RData", compress = "xz")
# 
# 
# #~# Converting dataframe into a site by species frequency matrix - requires reshape2
# #acast(df, site~species, length)
# ero.mat <- acast(ero.NoNA, ECO_NAME~name, length) #ECO_NAME~name means site x species
# 
# 
# #~# Converting frequency matrix into presence-absence matrix
# ero.mat.bin <- as.binary(ero.mat)

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

