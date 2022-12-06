                                    # Socioeconomic pests
#Requires: Literature, Impacts.noNA
#Include stats stuff here as well

#Wilcoxon rank sum test: search returns vs socioeconomic pest status
rstatix::wilcox_test(Literature, formula = SearchReturns ~ SEPest)
rstatix::wilcox_effsize(Literature, formula = SearchReturns ~ SEPest)

#DO socio-economic pests have more environmental impact information?
m1 <- glmer(Evidence ~ log(SearchReturns) * SEPest + (1|Order) + (1|scientificName), 
            family = poisson, data = Literature)

m2 <- glmer(Evidence ~ log(SearchReturns) + SEPest + (1|Order) + (1|scientificName), 
            family = poisson, data = Literature)

m3 <- glmer(Evidence ~ log(SearchReturns) + (1|Order) + (1|scientificName), 
            family = poisson, data = Literature)

mnull <- glmer(Evidence ~ 1 + (1|Order) + (1|scientificName), 
            family = poisson, data = Literature)

m4 <- glm(Evidence ~ log(SearchReturns) * SEPest, family = poisson, data = Literature)

anova(mnull, m3, m2, m1)
#m1 is the "best" model
anova(m1,m4)
#Random effects are required

summary(m1)

MuMIn::r.squaredGLMM(m1, mnull)

res1 <- simulateResiduals(m1, plot = T, refit = F)
res2 <- simulateResiduals(m2, plot = T, refit = F)

coefplot2::coefplot2(m1, intercept = TRUE, main = "Fixed effect coefficient")
coefplot2::coefplot2(m1, ptype = "vcov", intercept = TRUE, main = "Random effect variance")

pp <- list(layout.widths = list(left.padding = 0, right.padding = 0),
           layout.heights = list(top.padding = 0, bottom.padding = 0))
r2 <- ranef(m1, condVar = TRUE)
d2 <- lattice::dotplot(r2, par.settings = pp)

gridExtra::grid.arrange(d2$scientificName, d2$Order, nrow = 1)

##Search returns had the strongest effect and there was a slight interaction between returns and SE status

#Are socio-economic pests more likely to have environmental impact information?
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

# #loglinear analysis
# fit.3 <- glm(n ~ factor(Severity) + factor(SocioEconomicPest), data = Impacts.SE, family = poisson)
# fit.4 <- glm(n ~ factor(Severity) * factor(SocioEconomicPest), data = Impacts.SE, family = poisson)
# fit.0 <- glm(n ~ factor(Severity), data = Impacts.SE, family = poisson)
# summary(fit.3)
# summary(fit.4)
# anova(fit.3, fit.4, test = "Chisq")
# anova(fit.0, fit.3, test = "Chisq")
# #no effect of socioeconomic pest


#Search returns and socioeconomic pest status
SE_lit <- ggplot(Literature, aes(x = SEPest, y = SearchReturns)) +
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

#DD vs nonDD for socioeconomic pest yes/no (bar chart)
SE_info <- ggplot(Impacts.SE, aes(x = SocioEconomicPest, y = n, fill = Severity)) +
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


# ggplot(Impacts.SE_Ord_min, aes(y = n, axis1 = Order, axis2 = Severity,label = after_stat(stratum))) +
#   geom_alluvium(aes(fill = SocioEconomicPest), width = 1/12) +
#   geom_stratum(width = 1/12, fill = "black", color = "grey", position = "identity") +
#   geom_label(stat = "stratum", size = 4) +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         axis.title.y = element_text(size = 16),
#         axis.text = element_text(size = 14),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         #panel.border = element_blank(),
#         panel.background = element_blank(),
#         legend.position = "bottom",
#         legend.direction = "horizontal",
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 16)) +
#   scale_x_discrete(limits = c("Order", "Information"), expand = c(.05, .05)) +
#   scale_fill_manual(values = c("darkblue","lightblue")) +
#   ylab("Number of species")