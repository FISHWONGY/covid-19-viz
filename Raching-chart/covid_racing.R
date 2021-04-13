suppressPackageStartupMessages({
  library(tidyverse)
  library(tidycovid19)
  library(paletteer)
})

covid19_dta <- download_merged_data(silent = TRUE, cached = TRUE)


# Changing country name
covid19_dta$country[covid19_dta$country == "US"] <- "United States"
covid19_dta$country[covid19_dta$country == "Taiwan*"] <- "Taiwan"
covid19_dta$country[covid19_dta$country == "Korea, South"] <- "Korea"

# Caluculate the rank per country
covid_rank <- covid19_dta %>%  
  select(country, date, deaths, region) %>%  
  group_by(date) %>%  
  arrange(date, -deaths) %>%  
  mutate(rank = 1:n()) %>%  
  filter(rank <= 10) %>%
  mutate(deaths = deaths)


plot_df <- covid_rank 


plot_df$country[plot_df$country == "United States"] <- "US"
plot_df$country[plot_df$country == "United Kingdom"] <- "UK"
plot_df <- plot_df %>% filter(date >= as.Date("2020-03-01")) #Filter date after March
plot_df$deaths <- round(plot_df2$deaths, 0)


# Add a corona png on the graph
mypng <- readPNG('cartoon.png')

p <- 
  ggplot(plot_df2, aes(group = country)) + 
  geom_rect_pattern(
    aes(
      xmin = 0, 
      xmax = deaths, 
      ymin = rank - 0.45, 
      ymax = rank + 0.45,
      pattern_filename = I(flag),
    fill = deaths), 
    pattern = 'none'
  ) +   
  theme_delabj_dark() +
  geom_text(aes(y = rank, label = country), x = -500, col = "white",  
                hjust = "right", size = 6, family="ITC Officina Sans LT Book") +
  geom_text(aes(label = as.character(date)), x = 20000 , y = -10, 
                 size = 10, col = "white", family="ITC Officina Sans LT Book") +
  scale_y_reverse() + 
  scale_fill_viridis_b(direction = -1)+
  labs(x = '\n\nDeaths\n', fill = NULL,
       title = "Deaths due to Covid-19 over time",
       caption = "Source: Johns Hopkins CSSE\nwww.fishwongy.com") +  
  theme(text = element_text(size = 8.5, family="ITC Officina Sans LT Book"),
        legend.position = "none",
        axis.text.y     = element_blank(),
        axis.title.y    = element_blank(),
        axis.ticks.y    = element_blank(),
        legend.key.size = unit(2, 'cm'),
        axis.text       = element_text(size = 20),
        axis.title      = element_text(size = 20),
        plot.title = element_text(size = 15, face = "bold", family="ITC Officina Sans LT Bold"),
        plot.caption = element_text(size = 13, hjust = 0)
  ) +  
  
  annotation_raster(mypng, ymin = -9, ymax= -6, xmin = 17000, xmax = 25000) + #Adding the png file
  
  scale_x_continuous(  
    limits = c(-5000, 25000)) +
  gganimate::transition_time(date)


animate(p, duration = 35)

anim_save("covid_racing.gif", width = 11, height = 8)
