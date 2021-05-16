#~# Change synonyms to accepted names, and binomial to trinomial (where necessary)----
sppOcc.min <- sppOcc.min %>% 
  
  #Afranthidium repetitum
  mutate(scientificName = stringr::str_replace(scientificName, "Immanthidium repetitum", "Afranthidium repetitum")) %>% 
  mutate(scientificName = stringr::str_replace(scientificName, "Anthidium albolineatum", "Afranthidium repetitum")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Anthidium pallidicinctum", "Afranthidium repetitum")) %>%
  
  #Vespa velutina nigrithorax
  mutate(scientificName = stringr::str_replace(scientificName, "Vespa auraria", "Vespa velutina nigrithorax")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Vespa velutina nigrithorax nigrithorax", "Vespa velutina nigrithorax")) %>%
  
  #Bombus pascuorum
  mutate(scientificName = stringr::str_replace(scientificName, "Bombus agrorum", "Bombus pascuorum")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Megabombus pascuorum", "Bombus pascuorum")) %>%
  
  #Adelges abietis
  mutate(scientificName = stringr::str_replace(scientificName, "Sacchiphantes abietis", "Adelges abietis")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Chermes abietis", "Adelges abietis")) %>%
  
  #Adelges cooleyi
  mutate(scientificName = stringr::str_replace(scientificName, "Gilletteella cooleyi", "Adelges cooleyi")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Chermes cooleyi", "Adelges cooleyi")) %>%
  
  #Coptotermes gestroi
  mutate(scientificName = stringr::str_replace(scientificName, "Coptotermes havilandi", "Coptotermes gestroi")) %>%
  
  #Uroleucon nigrotuberculatum
  mutate(scientificName = stringr::str_replace(scientificName, "Dactynotus nigrotuberculatus", "Uroleucon nigrotuberculatum")) %>%
  
  #Insignorthezia insignis
  mutate(scientificName = stringr::str_replace(scientificName, "Orthezia insignis", "Insignorthezia insignis")) %>%
  
  #Hyphantria cunea
  mutate(scientificName = stringr::str_replace(scientificName, "Hyphantria textor", "Hyphantria cunea")) %>%
  
  #Paratrechina pubens
  mutate(scientificName = stringr::str_replace(scientificName, "Nylanderia pubens", "Paratrechina pubens")) %>%
  
  #Apis mellifera
  mutate(scientificName = stringr::str_replace(scientificName, "Apis mellifica", "Apis mellifera")) %>%
  
  #Paratrechina longicornis
  mutate(scientificName = stringr::str_replace(scientificName, "Prenolepis longicornis", "Paratrechina longicornis")) %>%
  
  #Linepithema humile
  mutate(scientificName = stringr::str_replace(scientificName, "Iridomyrmex humilis", "Linepithema humile")) %>%
  
  #Lymantria dispar
  mutate(scientificName = stringr::str_replace(scientificName, "Porthetria dispar", "Lymantria dispar")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Ocneria dispar", "Lymantria dispar")) %>%
  
  #Lysiphlebus testaceipes
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus persicaphidis", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus tritici", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Aphidius citraphis", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus piceiventris", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus myzi", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus coquilletti", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus cucurbitaphidis", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus abutilaphidis", "Lysiphlebus testaceipes")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lysiphlebus baccharaphidis", "Lysiphlebus testaceipes")) %>%
  
  #Myrmica rubra
  mutate(scientificName = stringr::str_replace(scientificName, "Myrmica laevinodis", "Myrmica rubra")) %>%
  
  #Pachycondyla chinensis
  mutate(scientificName = stringr::str_replace(scientificName, "Brachyponera chinensis", "Pachycondyla chinensis")) %>%
  
  #Vespula vulgaris
  mutate(scientificName = stringr::str_replace(scientificName, "Vespa vulgaris", "Vespula vulgaris")) %>%
  
  #Paratrechina fulva
  mutate(scientificName = stringr::str_replace(scientificName, "Nylanderia fulva", "Paratrechina fulva")) %>%
  
  #Cinara cupressi cupressi
  mutate(scientificName = stringr::str_replace(scientificName, "Cinara cupressivora", "Cinara cupressi cupressi")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Lachnus sabinae", "Cinara cupressi cupressi")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Cinara sabinae", "Cinara cupressi cupressi")) %>%
  
  #Aedes japonicus
  mutate(scientificName = stringr::str_replace(scientificName, "Ochlerotatus japonicus", "Aedes japonicus")) %>%
  
  #Pterostichus melanarius
  mutate(scientificName = stringr::str_replace(scientificName, "Platysma vulgare", "Pterostichus melanarius")) %>%
  
  #Zizeeria labradus
  mutate(scientificName = stringr::str_replace(scientificName, "Zizina labradus", "Zizeeria labradus")) %>%
  
  #Vespula germanica
  mutate(scientificName = stringr::str_replace(scientificName, "Vespa germanica", "Vespula germanica")) %>%
  
  #Monomorium destructor
  mutate(scientificName = stringr::str_replace(scientificName, "Trichomyrmex destructor", "Monomorium destructor")) %>%
  
  #Chaetosiphon fragaefolii
  mutate(scientificName = stringr::str_replace(scientificName, "Pentatrichopus fragaefolii", "Chaetosiphon fragaefolii")) %>%
  mutate(scientificName = stringr::str_replace(scientificName, "Myzus fragaefolii", "Chaetosiphon fragaefolii")) %>%
  
  #Rhyzopertha dominica
  mutate(scientificName = stringr::str_replace(scientificName, "Rhizopertha dominica", "Rhyzopertha dominica")) %>%
  
  #Rhopalosiphum maidis
  mutate(scientificName = stringr::str_replace(scientificName, "Aphis cookii", "Rhopalosiphum maidis"))