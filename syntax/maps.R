library(tidyverse)
library(sf)
library(showtext)
library(patchwork)
library(ragg)
font_add_google("Quattrocento")
showtext_auto()

load("./slides/data/seattle_tracts.RData")
load("./slides/data/seattle_all_data.RData")
load("./slides/data/seattle_union.RData")


# agg_png("./slides/img/letters_plot.png", width = 1500, height = 1000, res = 144, background = "white")


letters_plot <- letter_sf %>% 
  mutate(letter_type = fct_relevel(letter_type, "Personal", "BLM", "Nazi")) %>%
  ggplot(aes(color = mailed)) + 
  geom_sf(data = seattle_union, inherit.aes = FALSE, fill = exp_colors[5], color = exp_colors[1], size = 0.25) + 
  geom_sf(color = "#342c5c", size = 1.25) +
  geom_sf(size = 1) + 
  facet_wrap(~letter_type) + 
  theme_void(base_size = 30) + 
  coord_sf(expand = FALSE, ylim = c(y_min, y_max)) +
  scale_color_manual(values = c("Mailed" = exp_colors[2], "Unmailed" = exp_colors[3])) +
  theme(legend.position = "none",
        text = element_text(family = "Quattrocento"),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(0,0,0,0),
        strip.text = element_text(vjust = 1))

letters_ce_plot <- ggplot(seattle_all_data) +
  geom_sf(data = seattle_union, inherit.aes = FALSE, fill = exp_colors[5], color = exp_colors[1], size = 0.25) + 
  geom_sf(aes(fill = collective_efficacy), size = NA) + 
  geom_sf(data = letter_sf %>% filter(letter_type == "Personal"), color = "black", size = 1.25) +
  geom_sf(data = letter_sf %>% filter(letter_type == "Personal"), aes(color = mailed), size = 1, show.legend = FALSE) +
  scale_color_manual(values = c("Mailed" = exp_colors[2], "Unmailed" = exp_colors[3])) +
  coord_sf(expand = FALSE, ylim = c(y_min, y_max)) +
  theme_void(base_size = 20) + 
  scale_fill_gradient2("Collective\nEfficacy\n(2003)", low = exp_colors[3], mid = exp_colors[1], high = exp_colors[2]) +
  theme(text = element_text(family = "Quattrocento"),
        legend.position = c(0.92, 0.5),
        legend.title = element_text(lineheight = 0.5, hjust = 0.5),
        legend.text.align = 0.5)