                                                  #Literature
#Requires: Literature, Literature.min, Impacts.Max

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