#. https://rpubs.com/MarkusLoew/12164

mod <- glm(n ~ 1, family = "poisson", data = bp)

fit_seg <- segmented(mod, seg.Z = ~year, npsi = 3)
mod_sum <- summary(fit_seg)



# get the slopes
slope(fit_seg)

# get the fitted data
my.fitted <- fitted(fit_seg)
my.model <- data.frame(year = bp$year, n = my.fitted)
my.lines <- fit_seg$psi[, 2]

params <- data.frame(confint.segmented(fit_seg), y = c(208, 292, 335))


# plot the fitted model
bp_plot <- ggplot(my.model, aes(x = year, y = n)) + geom_line() +
  geom_vline(xintercept = my.lines, linetype = "dashed", color = "black") +
  geom_point(data = bp, aes(year,n), shape = 4) +
  geom_point(data = params, aes(x = Est., y =y), color = "red", shape = 1, size = 5) +
  geom_segment(x = params[1,2], y = 5, xend = params[1,3], yend = 5, colour = "grey43") +
  geom_segment(x =  params[2,2], y = 5, xend =  params[2,3], yend = 5, colour = "grey43") +
  geom_segment(x = params[3,2], y = 5, xend = params[3,3], yend = 5, colour = "grey43") + 
  theme_minimal() +
  labs(x = "Year",
       y = "Baywide Fall Scallop Count") +
  theme(
    axis.title.y = element_text(vjust = +3),
    plot.margin = margin(0.7,0.7,0.7,0.7, "cm"))


bp_plot


ggsave("bp_plot.png", bp_plot, dpi = 360, bg = "white")
