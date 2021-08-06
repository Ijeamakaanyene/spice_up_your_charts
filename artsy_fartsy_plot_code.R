women_research = tribble(
  ~country, ~years, ~ percentage,
  "EU28", "1996-2000", 0.32,
  "EU28", "2011-2015", 0.41,
  "United States", "1996-2000", 0.31,
  "United States", "2011-2015", 0.40,
  "United Kingdom", "1996-2000", 0.31,
  "United Kingdom", "2011-2015", 0.40,
  "Canada", "1996-2000", 0.32,
  "Canada", "2011-2015", 0.42,
  "Australia", "1996-2000", 0.33,
  "Australia", "2011-2015", 0.44,
  "France", "1996-2000", 0.34,
  "France", "2011-2015", 0.40,
  "Japan", "1996-2000", 0.15,
  "Japan", "2011-2015", 0.20,
  "Portugal", "1996-2000", 0.41,
  "Portugal", "2011-2015", 0.49) %>%
  mutate(percentage = percentage * 100) %>%
  pivot_wider(names_from = years, values_from = percentage)

women_research_sorted = women_research %>%
  mutate(country = forcats::fct_reorder(country, -`1996-2000`)) %>%
  arrange(-`1996-2000`) %>%
  mutate(country_y = seq(1, by = 2, length.out = 8),
         x_label = seq(99, 99.5, length.out = 8),
         xend_dots = seq(90, 95, length.out = 8))



# Colorful Version --------------------------------------------------------

ggplot(data = women_research_sorted) +
  # horizontal line + label at 40%
  geom_segment(aes(y = 0, yend = 16,
                   x = 40, xend = 40),
               size = 1.1,
               color = "#873a40",
               linetype = "solid") +
  geom_text(aes(x = 39.5, y = 16, label = "Min % for female\ngender balance"),
            color = "white",
            family = "Open Sans",
            size = 2.5,
            lineheight = 0.75,
            hjust = 0) +
  # creating legend
  geom_text(aes(x = 79, y = 8.5, 
                label = "% of women among\ntotal published reasearchers"), 
            color = "#fbbc54",
            fontface = "bold",
            family = "Open Sans",
            size = 4,
            hjust = 0.5,
            lineheight = 0.75) +
  geom_point(aes(x = 76.5, y = 14, shape = 21), 
             size = 2,
             color = "white") +
  geom_text(aes(x = 77, y = 8.5, label = "1996-2000"), 
            color = "white",
            family = "Open Sans",
            hjust = 0.5, 
            size = 3) +
  geom_point(aes(x = 75.75, y = 14, shape = 16), 
             size = 2,
             color = "white") +
  geom_text(aes(x = 76, y = 8.5, label = "2011-2015"),
            color = "white",
            family = "Open Sans",
            hjust = 0.5, 
            size = 3) +
  # Actual Data
  geom_segment(aes(x = 0, xend = `1996-2000`,
                   y = country_y, yend = country_y),
               linetype = "solid",
               size = .5,
               color = "white") +
  geom_segment(aes(x = `1996-2000`, 
                   xend = `2011-2015`,
                   y = country_y, 
                   yend = country_y),
               size = 1,
               color = "white") +
  geom_point(aes(x = `1996-2000`, 
                 y = country_y,
                 shape = 21),
             color = "white",
             fill = "#273249") +
  geom_point(aes(x = `2011-2015`, 
                 y = country_y,
                 shape = 16),
             color = "white") +
  geom_segment(aes(x = `2011-2015` + 2, 
                   xend = 75,
                   y = country_y,
                   yend = country_y),
               color = "white",
               linetype = "dotted",
               size = 0.5) +
  # horizontal line at 75% 
  geom_segment(aes(y = 0, yend = 16,
                   x = 75, xend = 75),
               size = 1.1,
               color = "#873a40",
               linetype = "solid") +
  # Country Labels
  geom_text(aes(x = x_label, 
                y = country_y, 
                label = country),
            color = "white",
            family = "Open Sans",
            fontface = "bold",
            size = 3,
            hjust = 1) +
  # Percentage Labels
  geom_text(aes(x = `2011-2015` + 1,
                y = country_y,
                label = paste0(`2011-2015`, "%")),
            fontface = "bold",
            color = "#fbbc54",
            size = 2.25) +
  labs(title = "Overall Improvement in Percentage of\nPublications by Female Researchers",
       caption = "Gender in the Global Research Landscape by Elsevier") +
  xlim(0, 100) +
  scale_y_continuous(limits = c(-22, 16),
                     expand = c(0.005, 0)) +
  scale_shape_identity() +
  coord_polar(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#273249",
                                    color = "#273249"),
    panel.background = element_rect(fill = "#273249",
                                    color = "#273249"),
    text = element_text(family = "Open Sans"),
    plot.title = element_text(size = 30, 
                              lineheight = 0.75,
                              margin = margin(t = 20),
                              hjust = 0.5,
                              face = "bold",
                              color = "#fbbc54"),
    plot.caption = element_text(color = "white")
  )


ggsave("artsy_fartsy_plot2.png", 
       plot = last_plot(), 
       path = here::here("images"),
       device = "png", 
       dpi = 300, width = 9, height = 10, units = "in")



# Black + White Version ---------------------------------------------------

ggplot(data = women_research_sorted) +
  # horizontal line + label at 40%
  geom_segment(aes(y = 0, yend = 16,
                   x = 40, xend = 40),
               size = 1.1,
               linetype = "solid") +
  geom_text(aes(x = 39.5, y = 16, label = "Min % for female\ngender balance"),
            family = "Open Sans",
            size = 2.5,
            lineheight = 0.75,
            hjust = 0) +
  # creating legend
  geom_text(aes(x = 79, y = 8.5, 
                label = "% of women among\ntotal published reasearchers"), 
            fontface = "bold",
            family = "Open Sans",
            size = 4,
            hjust = 0.5,
            lineheight = 0.75) +
  geom_point(aes(x = 76.5, y = 14, shape = 21), 
             size = 2) +
  geom_text(aes(x = 77, y = 8.5, label = "1996-2000"), 
            family = "Open Sans",
            hjust = 0.5, 
            size = 3) +
  geom_point(aes(x = 75.75, y = 14, shape = 16), 
             size = 2) +
  geom_text(aes(x = 76, y = 8.5, label = "2011-2015"),
            family = "Open Sans",
            hjust = 0.5, 
            size = 3) +
  # Actual Data
  geom_segment(aes(x = 0, xend = `1996-2000`,
                   y = country_y, yend = country_y),
               linetype = "solid",
               size = .5) +
  geom_segment(aes(x = `1996-2000`, 
                   xend = `2011-2015`,
                   y = country_y, 
                   yend = country_y),
               size = 1) +
  geom_point(aes(x = `1996-2000`, 
                 y = country_y,
                 shape = 21),
             fill = "white") +
  geom_point(aes(x = `2011-2015`, 
                 y = country_y,
                 shape = 16)) +
  geom_segment(aes(x = `2011-2015` + 2, 
                   xend = 75,
                   y = country_y,
                   yend = country_y),
               linetype = "dotted",
               size = 0.5) +
  # horizontal line at 75% 
  geom_segment(aes(y = 0, yend = 16,
                   x = 75, xend = 75),
               size = 1.1,
               linetype = "solid") +
  # Country Labels
  geom_text(aes(x = x_label, 
                y = country_y, 
                label = country),
            family = "Open Sans",
            fontface = "bold",
            size = 3,
            hjust = 1) +
  # Percentage Labels
  geom_text(aes(x = `2011-2015` + 1,
                y = country_y,
                label = paste0(`2011-2015`, "%")),
            family = "Open Sans",
            fontface = "bold",
            size = 2.25) +
  labs(title = "Overall Improvement in Percentage of\nPublications by Female Researchers",
       caption = "Gender in the Global Research Landscape by Elsevier") +
  xlim(0, 100) +
  scale_y_continuous(limits = c(-22, 16),
                     expand = c(0.005, 0)) +
  scale_shape_identity() +
  coord_polar(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "Open Sans"),
    plot.title = element_text(size = 30, 
                              lineheight = 0.75,
                              margin = margin(t = 20),
                              hjust = 0.5,
                              face = "bold")
  )


ggsave("artsy_fartsy_plot3.png", 
       plot = last_plot(), 
       path = here::here("images"),
       device = "png", 
       dpi = 300, width = 9, height = 10, units = "in")
