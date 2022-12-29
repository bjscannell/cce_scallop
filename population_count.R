#import data
raw <- read.csv("ScallopDensities.csv")

nb_mod <- GLMMadaptive::mixed_model(adults ~ period + season, random = ~ 1|site, data = Scallop,
                                    family = zi.negative.binomial(), zi_fixed = ~ period + season, zi_random = ~ 1|site)

plot(simulateResiduals(nb_mod))


scallop_fv <- Scallop %>% 
  mutate(fv = fitted(nb_mod))

#Graph - Help


baycount <- Scallop %>% 
  group_by(embayment, year, season) %>% 
  mutate(embay_count = sum(adults)) %>% 
  group_by(embayment, year, season) %>% 
  slice(1)


# rainplot 
ggplot(baycount , aes(x = period, y = adults)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")

library(ggbeeswarm)

period_labs <- list(
  'before'="Before",
  'after'="After")

# split it by season 
ggplot(baycount, aes(x = factor(season,levels = c("spr", "fall "), labels = c("Spring", "Fall")),
                     y = adults, fill = season)) + 
  geom_boxplot() +
  geom_quasirandom( size =1.5, alpha = 0.4, width =0.2) +
  theme_minimal() +
  facet_wrap(~ factor(period,levels = c("Before", "After"), labels = c("Before Collapse", "Post Collapse")),
             nrow = 1, strip.position = "bottom") +
  labs(x = "Time Period", y = "Adult Scallop Counts",) +
  theme(panel.spacing = unit(0, "cm"),
        strip.placement = "outside",
        legend.position = "none",
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = +3),
        plot.margin = margin(0.7,0.7,0.7,0.7, "cm")) +
  scale_fill_manual(values = c( "#f2bf3b","#588b8b"))


# dumbbell plot ------------------------------------------------------------


db_wide <- baycount %>% 
  group_by(embayment, period) %>% 
  summarise(n = sum(adults)) %>% 
  pivot_wider(names_from = period, values_from = n) %>% 
  mutate(diff = Before - After) %>% 
  filter(!is.na(diff)) %>% 
  mutate(mid= mean(c(After, Before))) %>%  ungroup()

library(ggplot2)
library(ggalt)
library(tidyverse)

ggplot(data = db_wide) +
  geom_segment(aes(y=embayment, yend=embayment, x=0, xend=600), # plot background 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=embayment, x=After, xend=Before), # dumbbells
                size=0.8, color="#b2b2b2", size_x=3, size_xend = 3,
                colour_x = "orange", colour_xend = "lightblue") +
  geom_segment(aes(x = mid, xend = mid - 0.01, y = embayment, yend = embayment), 
               arrow = arrow(angle = 30, length = unit(.1, "inches"), 
                             type = "closed"), 
               color = "darkred") +
  geom_text(data=filter(db_wide, embayment == "SB"), # before label
            aes(x=Before, y=embayment, label="Before Collapse"),
            color="lightblue", size=3, vjust=-1.5, fontface="bold") +
  geom_text(data=filter(db_wide, embayment == "SB"), # after label
            aes(x=After, y=embayment, label="After Collapse"),
            color="orange", size=3, vjust=-1.5, fontface="bold") +
  geom_text(aes(x=After, y=embayment, label=After), # add after number
            color="orange", size=2.75, vjust=2.5) +
  geom_text(aes(x=Before, y=embayment, label=Before), # add before number
            color="lightblue", size=2.75, vjust=2.5) +
  geom_rect(aes(xmin=600, xmax= 700, ymin=-Inf, ymax=Inf), fill="grey") + # diff col
  geom_text(aes(label=paste0(round(((Before-After)/Before)*100, 2), "%"), y=embayment, x=650), fontface="bold", size=3) + # add the diff numbers
  geom_text(data=filter(db_wide, embayment=="SB"),
            aes(x=650, y=embayment, label="Decrease"),
            color="black", size=3.1, vjust=-2, fontface="bold") + # add the word difference 
  labs(x = "Scallop Count", y = "Embayment") +
  theme_bw() +
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  )




scall2 <- Scallop %>%
  group_by(year, season) %>%
  mutate(adults_mean = mean(adults)) %>% 
  mutate(ID = paste(embayment, site, sep = "-"),
         time_ID = as.factor(paste(season, period, sep = "-")),
         rownum = row_number())


fall <- scall2 %>% 
  filter(season == "fall ") %>% 
  group_by(period) %>% 
  sample_n(200) %>% 
  mutate(id  = row_number()) %>%
  ungroup()


ggplot(fall, aes(x = adults_mean, color = period))+
  geom_density()+
  theme_classic()




# anova -------------------------------------------------------------------

# make it balnced
# 22.4.2 what type of anova we want 
two.way <- aov(fv ~ period*season , data = scallop_fv)
summary(two.way)

plot(simulateResiduals(two.way))

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way

kw_test <- kruskal.test(adults ~ time_ID, data = scall2)
kw_test


pairwise.wilcox.test(scall2$adults, scall2$time_ID,
                     p.adjust.method = "BH")

scall2 %>%
  ungroup() %>% 
  dplyr::select(adults, period, time_ID) %>% 
  rstatix::anova_test(dv = adults,
                      wid = time_ID,
                      within = c(period),
                      detailed = TRUE)

nbinom <- fitdistr(Scallop$adults, "Negative Binomial")
qqp(Scallop$adults, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
gf <- goodfit(Scallop$adults, type="nbinomial")
summary(gf)
plot(gf)







# Collapse to baywide


bp <- Scallop %>% 
  dplyr::filter(season == "fall ") %>% 
  mutate(year = year + 2000) %>% 
  group_by(year) %>% 
  slice_sample(n=48) %>% 
  ungroup() %>% group_by(year) %>% 
  summarise(n = sum(adults))






