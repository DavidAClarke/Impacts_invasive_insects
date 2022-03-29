                            # Maximum impact severity and geographic range
#Requires: MaxSev_range

#Plotting mean of X for each level of ordinal variable Y
#Graphically testing assumption of ordinality under proportional odds
ord_assum <- rms::plot.xmean.ordinaly(MaxSev_range$Severity ~ MaxSev_range$AlienCountries, cr=FALSE)
#Looks pretty good

#Run ordinal logistic regression
m <- polr(Severity ~ AlienCountries, data = MaxSev_range, Hess = TRUE)
summary(m)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(m)

#Model with only the intercept
m1 <- polr(Severity ~ 1, data = MaxSev_range, Hess = TRUE)
summary(m1)

#Comparing models to see the effect of AlienCountries
anova(m1,m)
#They are significantly different

#Setting up for making predictions of maximum severity from how widespread a species is
newdat <- data.frame(AlienCountries = seq(from = 1, to = 125))
newdat <- cbind(newdat, predict(m, newdat, type = "probs"))
lnewdat <- reshape2::melt(newdat, id.vars = c("AlienCountries"),
                          variable.name = "Level", value.name="Probability")

#############################Estimate probabilities at fixed settings of range################
#Mean number of countries
#mean_range <- predict(m, data.frame(AlienCountries = mean(MaxSev_range$AlienCountries)), type = "probs")

#Median number of countries
#median_range <- predict(m, data.frame(AlienCountries = median(MaxSev_range$AlienCountries)), type = "probs")

#Minimum number of countries
#min_range <- predict(m, data.frame(AlienCountries = min(MaxSev_range$AlienCountries)), type = "probs")

#Maximum number of countries
#max_range <- predict(m, data.frame(AlienCountries = max(MaxSev_range$AlienCountries)), type = "probs")

#Combining estimated probabilities
#Est_probs <- rbind(mean_range, median_range, min_range, max_range)
###############################################################################################

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

#Predicting maximum severity from how widespread a species is
#my_cols <- c("darkorchid4", "dodgerblue4", "aquamarine4", "yellow2")
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
