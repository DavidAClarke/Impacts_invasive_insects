# Confidence
conf_plot <- ggplot(Impacts.nonDD, aes(x = Confidence, fill = Confidence)) +
  geom_bar(width = .5) + 
  scale_fill_manual(values = c("Low" = "#00AFBB", "Medium" = "#E7B800", "High"="#FC4E07")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,400)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("Frequency")

#Minimum set
minset <- Impacts.nonDD %>% dplyr::select(`Order`, `Mechanism`, `Severity`, `Confidence`)

#Multiple correspondence analysis
minsetMCA <- MCA(minset, graph = F)

catMCA <- fviz_mca_var(minsetMCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal(),
             title = "")

indMCA <- fviz_mca_ind(minsetMCA, 
             label = "none", # hide individual labels
             habillage = "Confidence", # color by groups 
             palette = c("#00AFBB", "#E7B800","#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", #confidence ellipse around mean point
             ggtheme = theme_minimal(),
             title = "")

#Combine plots
ggarrange(catMCA, indMCA, nrow = 2, ncol = 1)
