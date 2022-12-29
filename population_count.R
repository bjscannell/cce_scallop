

# zero inflated negative binomial for adult scallops ----------------------


nbinom <- fitdistr(Scallop$adults, "Negative Binomial")
qqp(Scallop$adults, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
gf <- goodfit(Scallop$adults, type="nbinomial")
summary(gf)
plot(gf)


nb_mod <- GLMMadaptive::mixed_model(adults ~ period + season, random = ~ 1|site, data = Scallop,
                                    family = zi.negative.binomial(), zi_fixed = ~ period + season, zi_random = ~ 1|site)

plot(simulateResiduals(nb_mod))


scallop_fv <- Scallop %>% 
  mutate(fv = fitted(nb_mod))




# Two way anova -------------------------------------------------------------------

# make it balanced?
# 22.4.2 what type of anova we want 
two.way <- aov(fv ~ period*season , data = scallop_fv)
summary(two.way)

plot(simulateResiduals(two.way))

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way









