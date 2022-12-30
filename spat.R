ggplot(SpatMaster, aes(spat)) + geom_histogram()

zero_rm <- SpatMaster %>%
  filter(spat != 0) %>%
  mutate(year = year - 2000)

m1 <- glmmTMB(formula = spat ~ year, 
    family = inverse.gaussian(link = "log"), data = zero_rm)

summary(m1)
plot(simulateResiduals(m1))
