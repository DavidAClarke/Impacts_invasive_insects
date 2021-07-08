                      #Information availability between taxonomic orders
#Requires: species_assessed_min

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

#For following plot
DDvsNonDD <- species_assessed_min %>%
  dplyr::select(Order, DD, NotDD) %>%
  mutate(DD = DD * -1) %>%
  gather("Status", "Proportion", -Order) %>%
  arrange(Proportion)

#Proportion of DD/nonDD species per order
info_plot <- ggplot(DDvsNonDD, aes(x = reorder(Order, desc(Proportion)), y = Proportion, fill = Status)) +
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