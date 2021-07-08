                                          # Impact mechanisms
#Requires: Impacts.nonDD

#Number of each mechanism instance (from every reference) (bar chart)
Impacts.nonDD <- Impacts.nonDD %>% 
  #mutate(Mechanism = stringr::str_replace(Mechanism, "Competition", "Com")) %>%
  #mutate(Mechanism = stringr::str_replace(Mechanism, "Predation", "Pre")) %>%
  #mutate(Mechanism = stringr::str_replace(Mechanism, "Hybridisation", "Hyb")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Transmission of disease", "Transmission")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Other", "Other")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Interaction with other alien species", "Interaction")) %>%
  #mutate(Mechanism = stringr::str_replace(Mechanism, "Herbivory", "Her")) %>%
  #mutate(Mechanism = stringr::str_replace(Mechanism, "Parasitism", "Par")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Poisoning/toxicity", "Poison")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Facilitation of native species", "Facilitation")) %>%
  mutate(Mechanism = stringr::str_replace(Mechanism, "Chemical/physical/structural impact on ecosystem", "Chemical"))

ImpactMechanism <- Impacts.nonDD %>% 
  distinct(PubID = PubID, .keep_all = T) %>%
  count(Order, Mechanism) %>%
  group_by(Mechanism) %>%
  add_tally(n, name = "TotalMech") %>%
  group_by(Order) %>%
  add_tally(n, name = "TotalOrd") %>%
  mutate(PropMech = round(n/TotalMech, 2)) %>%
  mutate(PropOrd = round(n/TotalOrd, 2)) %>%
  rename(NumStudies = "n")

#Getting table order vs mechanism
# OrderMech <- ImpactMechanism %>%
#   dplyr::select(Mechanism, Order, NumStudies) %>%
#   spread(Mechanism, NumStudies) %>%
#   mutate(across(everything(), ~replace_na(.x,0)))

#Between Orders
balloonMech <- ggballoonplot(ImpactMechanism, size = "PropMech", fill = "NumStudies")+
  scale_fill_viridis_c(option = "D", begin = 1, end = 0) +
  theme_bw() +
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.text.x = element_text(colour = "black", size = 14, angle = 90, vjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "top",
        plot.margin = unit(c(1,1,1,1), "cm")) +
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2)) +
  ylab("Impact mechanism") +
  xlab("Taxonomic order")

                                              #Impact severities
ImpactSeverity <- Impacts.nonDD %>% 
  distinct(PubID = PubID, .keep_all = T) %>%
  count(Severity, Order) %>%
  group_by(Severity) %>%
  add_tally(n, name = "TotalSev") %>%
  group_by(Order) %>%
  add_tally(n, name = "TotalOrd") %>%
  mutate(PropSev = round(n/TotalSev, 2)) %>%
  mutate(PropOrd = round(n/TotalOrd, 2)) %>%
  rename(NumStudies = "n")

#Getting table order vs severity
# OrderSev <- ImpactSeverity %>%
#   dplyr::select(Severity, Order, NumStudies) %>%
#   spread(Severity, NumStudies) %>%
#   mutate(across(everything(), ~replace_na(.x,0)))

balloonSev <- ggballoonplot(ImpactSeverity, size = "PropSev", fill = "NumStudies")+
  scale_fill_viridis_c(option = "C", begin = 1, end = 0) +
  theme_bw() +
  theme(axis.text.y = element_text(colour = "black", size = 14),
        axis.text.x = element_text(colour = "black", size = 14, vjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "top",
        plot.margin = unit(c(1,1,1,1), "cm")) +
  guides(fill = guide_colorbar(order = 1),
         size = guide_legend(order = 2)) +
  ylab("Taxonomic order") +
  xlab("Impact severity")