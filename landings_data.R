#organizing and showing annual data
annuallandings <- read_excel("all_scallop_data.xlsx",
                             sheet = "cal_year_landings")
annuallandings <- annuallandings %>%
  rename(peconic_landings = 2) %>%
  mutate(peconic_landings_lbs = peconic_landings*1000)
plot(annuallandings$peconic_landings_lbs)

#histogram
hist(annuallandings$peconic_landings_lbs) #right skew
hist(log(annuallandings$peconic_landings_lbs)) # better but still not normal when log transformed yo 

#some distribution tests
gamma_test(annuallandings$peconic_landings_lbs) #gamma. high p value means it passes this test and is likely gamma
ig_test(annuallandings$peconic_landings_lbs, method = "transf") #inverse gaussian. small p value is bad!

# using this package https://vincenzocoia.com/post/gam/ 

#assuming data are normal
normalgam <- gam(peconic_landings_lbs ~ s(year, bs = "cr"), data = annuallandings)
summary(normalgam)
simulationOutput <- simulateResiduals(fittedModel = normalgam)
plot(simulationOutput) 
plot(normalgam)

#assuming data are gamma
gammagam <- gam(peconic_landings_lbs ~ s(year, bs = "cr"), data = annuallandings, family = Gamma(link = log))
summary(gammagam)
gammasimulationOutput <- simulateResiduals(fittedModel = gammagam)
plot(gammasimulationOutput) 
plot(gammagam)

#assuming data are inverse gaussian
ingausfitgam <- gam(peconic_landings_lbs ~ s(year, bs = "cr"), data = annuallandings, family = inverse.gaussian(link="1/mu^2")) #cubic splines
#won't converge likely due to link function but that doesn't matter

