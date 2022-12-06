                            # Maximum impact severity and geographic range
#Requires: MaxSev_range

#Cumulative link models
cm1 <- ordinal::clmm(Severity ~ log(Total_area) + Total_ports + AlienCountries + (1|Order), data = na.omit(MaxSev_range), nAGQ = 10)
cm2 <- ordinal::clm(Severity ~ log(Total_area) + Total_ports + AlienCountries, data = na.omit(MaxSev_range), nAGQ = 10)
cm3 <- ordinal::clm(Severity ~ log(Total_area) + AlienCountries, data = na.omit(MaxSev_range), nAGQ = 10)
anova(cm1,cm2) #random effects not important
anova(cm2,cm3)
anova(cm2)
cm4 <- ordinal::clm(Severity ~ AlienCountries, data = na.omit(MaxSev_range), nAGQ = 10)
anova(cm2,cm4) #no significant difference including area or ports
cm5 <- ordinal::clm(Severity ~ 1, data = cm4$model, nAGQ = 50)
anova(cm4,cm5) #significant difference between models

#Confirms that cm4 is the "best" model.
step.model <- stepAIC(cm2, direction = "backward", trace = 1)

ordinal::nominal_test(cm4) ## no evidence of non-proportional odds.
ordinal::scale_test(cm4) ## no evidence of scale effects.
ordinal::convergence(cm4) ## Paramter accuracy. Looks all good.

#Confidence intervals
pr <- profile(cm4)
plot(pr)
exp(confint(pr))


#Setting up for making predictions of maximum severity from how widespread a species is
#taking into account total area
AlienCountries <- seq(1,100, by=1)

# pred_grid <- expand.grid(AlienCountries = AlienCountries, 
#                          Total_area = log(MaxSev_range$Total_area),
#                          Total_ports = MaxSev_range$Total_ports)
# newdat <- data.frame(pred_grid, predict(cm2, pred_grid, type = "prob"))
# lnewdat <- reshape2::melt(newdat, id.vars = c("AlienCountries", "Total_area"),
#                           variable.name = "Level", value.name="Probability")
# lnewdat <- lnewdat %>% filter(Level != "Total_ports")
# 
# 
# pred_grid <- expand.grid(AlienCountries = AlienCountries, 
#                          Total_area = log(MaxSev_range$Total_area))
# newdat <- data.frame(pred_grid, predict(cm3, pred_grid, type = "prob"))
# lnewdat <- reshape2::melt(newdat, id.vars = c("AlienCountries", "Total_area"),
#                           variable.name = "Level", value.name="Probability")

pred_grid <- expand.grid(AlienCountries = AlienCountries)
newdat <- data.frame(AlienCountries, predict(cm4, newdata = pred_grid, type = "prob")$fit)
lnewdat <- reshape2::melt(newdat, id.vars = c("AlienCountries"),
                          variable.name = "Level", value.name="Probability")

##Max severity vs range
#boxplot
sev_range_var <- ggplot(MaxSev_range, aes(x = Severity, y = AlienCountries)) +
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
  ylab("Number of countries with alien populations(log10)") +
  xlab("Maximum impact severity") +
  guides("colour" = "none") +
  scale_colour_manual(values = pnw_palette("Bay", 4))

#Predicting maximum severity from how widespread a species is based on
#model that includes effect of total area
sev_range_pred <- ggplot(data = lnewdat, aes(x = AlienCountries,y = Probability, group = Level)) +
  geom_line(aes(linetype = Level, col = Level)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "top") +
  scale_color_manual(name = "Impact Severity", 
                     values = pnw_palette("Bay", 4), 
                     labels = c("Minimal Concern","Minor","Moderate","Major")) +
  scale_linetype_manual(name = "Impact Severity", 
                        values = c(1,2,3,4),
                        labels = c("Minimal Concern","Minor","Moderate","Major")) +
  xlab("Number of countries with alien populations") #+
  # annotate("text", x = 5, y = 0.34, label = "Bessa remota", colour = "black", size = 3, fontface = 3) +
  # annotate("text", x = 6, y = 0.10, label = "Adelges tsugae", colour = "black", size = 3, fontface = 3) +
  # annotate("text", x = 98, y = 0.33, label = "Paratrechina longicornis", colour = "black", size = 3, fontface = 3) +
  # annotate("text", x = 85, y = 0.48, label = "Tapinoma melanocephalum", colour = "black", size = 3, fontface = 3) +
  # annotate("point", x = 85, y = 0.485, colour = "aquamarine4") +
  # annotate("point", x = 2, y = 0.35, colour = "aquamarine4") +
  # annotate("point", x = 98, y = 0.32, colour = "yellow2") +
  # annotate("point", x = 6, y = 0.09, colour = "yellow2")

#Combined figures
sev_range_comb <- ggarrange(sev_range_var, 
                            sev_range_pred, 
                            ncol = 1, 
                            nrow = 2, 
                            labels = c("A", "B"), 
                            common.legend = T,
                            legend = "bottom")
