
# before/after & spring/fall boxplots -------------------------------------

baycount <- Scallop %>% 
  group_by(embayment, year, season) %>% 
  mutate(embay_count = sum(adults)) %>% 
  group_by(embayment, year, season) %>% 
  slice(1) 

bayaverage <- Scallop %>%
  group_by(embayment, year, season) %>%
  summarise(avg = mean(adults)) %>%
  mutate(period = case_when(year <= 18 ~ "Before", year > 18 ~ "After")) %>%
  pivot_wider(names_from = season, values_from = avg) %>%
  mutate(n = `fall ` - spr) %>%
  ungroup() %>%
  group_by(embayment, period) %>%
  summarise(n = mean(n)) %>%
  filter(embayment != "Napegue Hbr") %>%
  pivot_wider(names_from = period, values_from = n) %>% 
  mutate(diff = Before - After) %>% 
  filter(!is.na(diff)) %>% 
  mutate(mid= mean(c(After, Before))) %>%  ungroup()
  mutate(across(c(After, Before, diff, mid), round))
  

period_labs <- list(
  'before'="Before",
  'after'="After")


# split it by season 
ggplot(baycount, aes(x = factor(season,levels = c("spr", "fall "), labels = c("Spring", "Fall")),
                     y = adults, fill = season)) + 
  geom_boxplot() +
  geom_quasirandom( size =1.5, alpha = 0.4, width =0.2) +
  theme_minimal() +
  facet_wrap(~ factor(period,levels = c("Before", "After"), labels = c("2008-2018", "2019-2020")),
             nrow = 1, strip.position = "bottom") +
  labs( y = expression(paste("Adult Scallop Counts per 50 m"^"2"))) +
  theme(panel.spacing = unit(0, "cm"),
        strip.placement = "outside",
        legend.position = "none",
        axis.title.y =element_text(vjust = +3),
        axis.title.x = element_blank(),
        plot.margin = margin(0.7,0.7,0.7,0.7, "cm")) +
  scale_fill_manual(values = c( "#f2bf3b","#588b8b")) 

ggsave("plots/boxplot.png", dpi = 360, bg = "white")
  
  


# Change in scallop dumbbell plot -----------------------------------------


db_wide <- baycount %>% 
  group_by(embayment, period) %>% 
  summarise(n = sum(adults)) %>% 
  pivot_wider(names_from = period, values_from = n) %>% 
  mutate(diff = Before - After) %>% 
  filter(!is.na(diff)) %>% 
  mutate(mid= mean(c(After, Before))) %>%  ungroup()



ggplot(data = db_wide) +
  geom_segment(aes(y=embayment, yend=embayment, x=-150, xend=600), # plot background 
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
            color="lightblue", size=3, vjust=-2.8, fontface="bold") +
  geom_text(data=filter(db_wide, embayment == "SB"), # after label
            aes(x=After, y=embayment, label="After Collapse"),
            color="orange", size=3, vjust= 3.6, fontface="bold") +
  geom_text(aes(x=After, y=embayment, label=After), # add after number
            color="orange", size=2.75, vjust=2.5) +
  geom_text(aes(x=Before, y=embayment, label=Before), # add before number
            color="lightblue", size=-2.75, vjust=2.5) +
  geom_rect(aes(xmin=-125, xmax= -60, ymin=-Inf, ymax=Inf), fill="grey") + # diff col
  geom_text(aes(label=paste0(round(((Before-After)/Before)*-100, 2), "%"), 
                y=embayment, x=-95), fontface="bold", size=3) + # add the diff numbers
  geom_text(data=filter(db_wide, embayment=="SB"),
            aes(x=-95, y=embayment, label="get rid of this text"),
            color="black", size=3.1, vjust=-2, fontface="bold") + # add the word difference 
  labs(x = "Scallop Count", y = "Embayment") +
  theme_bw() +
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    #axis.text.x=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  ) 


#dumbbell for change in delta density 
#data is right, graph is not, round?
ggplot(data = bayaverage) +
  geom_segment(aes(y=embayment, yend=embayment, x=-50, xend = 10), # plot background 
               color="#b2b2b2", size=0.15) +
  geom_dumbbell(aes(y=embayment, x=After, xend=Before), # dumbbells
                size=0.8, color="#b2b2b2", size_x=3, size_xend = 3,
                colour_x = "orange", colour_xend = "lightblue") +
  geom_segment(aes(x = mid, xend = mid - 0.01, y = embayment, yend = embayment), 
               arrow = arrow(angle = 30, length = unit(.1, "inches"), 
                             type = "closed"), 
               color = "darkred") +
  geom_text(data=filter(bayaverage, embayment == "SB"), # before label
            aes(x=Before, y=embayment, label="Before Collapse"),
            color="lightblue", size=3, vjust=-2.8, fontface="bold") +
  geom_text(data=filter(bayaverage, embayment == "SB"), # after label
            aes(x=After, y=embayment, label="After Collapse"),
            color="orange", size=3, vjust= 3.6, fontface="bold") +
  geom_text(aes(x=After, y=embayment, label=After), # add after number
            color="orange", size=2.75, vjust=2.5) +
  geom_text(aes(x=Before, y=embayment, label=Before), # add before number
            color="lightblue", size=-2.75, vjust=2.5) +
  geom_rect(aes(xmin=-50, xmax= -60, ymin=-Inf, ymax=Inf), fill="grey") + # diff col
  geom_text(aes(label=paste0(round(((Before-After)/Before)*-100, 2), "%"), 
                y=embayment, x=-30), fontface="bold", size=3) + # add the diff numbers
  geom_text(data=filter(bayaverage, embayment=="SB"),
            aes(x=-30, y=embayment, label="get rid of this text"),
            color="black", size=3.1, vjust=-2, fontface="bold") + # add the word difference 
  labs(x = "Scallop Count", y = "Embayment") +
  theme_bw() +
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    #axis.text.x=element_blank(),
    plot.title=element_text(size = 16, face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  ) + coord_flip()
