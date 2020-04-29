library(tidyverse)
library(tidycovid19)
library(paletteer)
library(ggplot2)
library(delabj)
library(patchwork)

merged <- download_merged_data(cached = TRUE, silent = TRUE)

sc <- plot_covid19_stripes(
  merged,
  type = "confirmed",
  min_cases = 10000,
  sort_countries = "start"
) +
  scale_color_continuous(name = "Daily change in confirmed case\n(7 days average)",
                        type = "viridis",
                        #option = "inferno",
                        trans = "log10",
                        direction = -1,
                        na.value = "gray80"
                        #breaks = c(1e-03, 1e-01, 1e+01, 1e+03), label = c(0.001, 0.01, 10, 1000),
                       ) +
  #delabj::theme_delabj() +
  theme(text = element_text(family="ITC Officina Sans LT Book"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
         plot.title = element_text(size = 13, family="ITC Officina Sans LT Bold"),
        plot.caption = element_text(hjust = 0, size = 10, family="ITC Officina Sans LT Book")) +
labs(title = "News daily covid-19 confirmed cases",
     caption = "") 
     
 sd <- plot_covid19_stripes(
  merged,
  type = "deaths",
  min_cases = 1000,
  sort_countries = "start"
) +
  scale_color_continuous(name = "Daily change in death\n(7 days average)",
                        type = "viridis",
                        #option = "inferno",
                        trans = "log10",
                        direction = -1,
                        na.value = "gray80"
                        #breaks = c(1e-03, 1e-01, 1e+01, 1e+03), label = c(0.001, 0.01, 10, 1000),
                       ) +
  #delabj::theme_delabj() +
  theme(text = element_text(family="ITC Officina Sans LT Book"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
         plot.title = element_text(size = 13, family="ITC Officina Sans LT Bold"),
        plot.caption = element_text(hjust = 0, size = 10, family="ITC Officina Sans LT Book")) +
labs(title = "News daily covid-19 confirmed deaths",
    caption = "Source: Johns Hopkins CSSE \nwww.fishwongy.com") 
    
 
 sc / sd +
   ggsave("~/stripe_patch.png", dpi = 420, width = 5.8, height = 13)
