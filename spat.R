ggplot(SpatMaster, aes(spat)) + geom_histogram()



glm(formula = spat ~ year, 
    family = inverse.gaussian(link = "log"), data = SpatMaster)
