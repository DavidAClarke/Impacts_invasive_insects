                                    # Islands versus mainland
#Requires: Impacts.nonDD
#Include stats stuff here as well

##Do islands have more harmful impacts than mainland?
#Only using studies once; removed laboratory-only studies
Impact_landmass <- Impacts.nonDD %>%
  distinct(PubID = PubID, .keep_all = T) %>%
  dplyr::filter(StudyType != "Laboratory") %>%
  mutate(Severity = ifelse(Severity == "MO"| Severity == "MR", "Harmful", "NotHarmful")) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(LandmassType, Severity) %>%
  count(LandmassType, Severity)

#loglinear analysis
fit <- glm(n ~ factor(Severity) + factor(LandmassType), data = Impact_landmass, family = poisson)
pchisq(deviance(fit), df = df.residual(fit), lower.tail = F)
anova(fit, test = "Chisq")
#Independence model fits well

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


Isl_v_cont <- ggplot(Impact_landmass_plot, aes(LandmassType, n, fill = Severity)) +
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