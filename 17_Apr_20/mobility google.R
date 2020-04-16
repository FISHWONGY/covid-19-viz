# Scraping google

library(needs)
needs(tidyverse, magrittr, animation, pdftools, png, scales)
library(delabj)
library(RcppRoll)
library(ggrepel)


process_google_mobility <- function(country_code, start_date, end_date){

  pdf_convert(paste0("https://www.gstatic.com/covid19/mobility/",end_date,"_",country_code,"_Mobility_Report_en.pdf"), format = "png", pages = 1, dpi = 300, filenames = "IMG1.png")
  

  pdf_convert(paste0("https://www.gstatic.com/covid19/mobility/",end_date,"_",country_code,"_Mobility_Report_en.pdf"), format = "png", pages = 2, dpi = 300, filenames = "IMG2.png")

  img <- readPNG("IMG1.png")

  r <- img[,,1] * 255
  g <- img[,,2] * 255
  b <- img[,,3] * 255
  
  val <- c(853:1328) %>%
    map_dbl(~{
      # find the vertical pixels that match our specified rgb() colour, and average them to get the middle pixel of the line on the chart
      mean(which(
        r[1500:1822,.x]==66 & g[1500:1822,.x]==133 & b[1500:1822,.x]==244
      ))
    }) %>%
    # rescale this value so instead of being a pixel number, it’s a value from -100 to 80 (the domain of the y-axis)
    scales::rescale(to=c(80, -100), from = c(1, 322))

  date <- seq.Date(as.Date(start_date), as.Date(end_date), length.out = length(val))

  retail <- tibble(date, val) %>%
    group_by(date) %>%
    summarise(val = mean(val)) %>%
    mutate(measure = "Retail", country_code)

  val <- c(853:1328) %>%
    map_dbl(~{
      mean(which(
        r[2476:2798,.x]==66 & g[2476:2798,.x]==133 & b[2476:2798,.x]==244
      ))
    }) %>%
    scales::rescale(to=c(80, -100), from = c(1, 322))
  date <- seq.Date(as.Date(start_date), as.Date(end_date), length.out = length(val))
  parks <- tibble(date, val) %>%
    group_by(date) %>%
    summarise(val = mean(val)) %>%
    mutate(measure = "Parks", country_code)
  
  # transit is on page two, so we need to load in the new image
  
  img <- readPNG("IMG2.png")
  
  r <- img[,,1] * 255
  g <- img[,,2] * 255
  b <- img[,,3] * 255
  
  # transit
  val <- c(785:1259) %>%
    map_dbl(~{
      mean(which(
        r[222:544,.x]==66 & g[222:544,.x]==133 & b[222:544,.x]==244
      ))
    }) %>%
    scales::rescale(to=c(80, -100), from = c(1, 322))
  date <- seq.Date(as.Date(start_date), as.Date(end_date), length.out = length(val))
  transit <- tibble(date, val) %>%
    group_by(date) %>%
    summarise(val = mean(val)) %>%
    mutate(measure = "Transit", country_code)
  
  # then combine retail, parks and transit into one table, and return this
  return(bind_rows(retail, parks, transit))
  
}

# Loop through countries of interest, processing Google’s two reports (to date) for each of them, and joining them together
countries <- c("GB", "US", "FR", "DE", "ES", "IT", "NO", "NL", "SE", "BE", "AT")
countries %>%
  map_dfr(~{
    data <- bind_rows(
      process_google_mobility(.x, "2020-02-16", "2020-03-29"),
      process_google_mobility(.x, "2020-02-23", "2020-04-05")
    ) 
  }) -> GCM_master

# PLot this dataset as small multiples; one chart for each topic, one line on each per country
GCM_master %>% 
  # Get rid of any missing data
  filter(!is.na(val)) %>%
  # Make sure dates are read as dates (days) not date-times
  mutate(
    date = as.character(as.Date(date, "%Y-%m-%d")),
    date = as.Date(date)
  ) %>%
  # Make sure we only have one value per country per topic per day
  group_by(measure, date, country_code) %>%
  summarise(val = mean(val)) %>%
  group_by(measure, country_code) %>%
  # Create a smoothed moving average to iron out noise in the daily data (if you want)
  mutate(smoothed = roll_meanr(val, 7)) %>% 
  # Filter for only a subset of countries of interest for this plot
  filter(country_code %in% c("GB", "IT", "FR", "ES", "SE", "DE", "US")) %>%
  
  
  # Plot moving average (or raw data) vs date
  ggplot(aes(x = date, y = smoothed)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(col = country_code), size = 0.5) +
  geom_line(size = 1.5, data = . %>% filter(country_code == "GB"), col = "white") +
  geom_line(size = 1, data = . %>% filter(country_code == "GB"), col = "black") +
  geom_text_repel(aes(col = country_code, label = country_code), direction = "y", 
                  data = . %>% group_by(country_code, measure) %>% top_n(1, date), hjust = 0,
                  family = "ITC Officina Sans LT Book") +
 scale_x_date(limits = c(as.Date("2020-02-21"), as.Date("2020-04-14")), 
               breaks = c(as.Date("2020-02-22"), as.Date("2020-04-05")), 
               labels = function(x)format(x,"%d %b")) +
  scale_y_continuous(limits = c(-100, 40), breaks = seq(-80, 40, 20), expand = c(0,0), 
                     sec.axis = dup_axis()) +
  facet_wrap(~measure, nrow = 1) +
  #theme_minimal() +
  theme_delabj() +
  scale_color_delabj() +
  labs(x = "\nDate",
       title = "Mobility - Country level",
       subtitle = "By destination") +
  theme(
    text = element_text(size = 12, family="ITC Officina Sans LT Book"),
    axis.title.x = element_text(family = "ITC Officina Sans LT Bold"),
    plot.title = element_text(size = 15, face = "bold", family="ITC Officina Sans LT Bold"),
    plot.subtitle = element_text(hjust = 0, vjust = 2.5,family="ITC Officina Sans LT Book"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(),
    axis.title = element_blank(),
    legend.position = "none"
  )



# Apple map

mobi <- suppressWarnings(read_csv("/Volumes/My Passport for Mac/R/convid-19/15 Apr 20/Mobility google/applemobilitytrends-2020-04-13.csv", na = "NULL"))

mobi <- mobi %>% gather(key = "date", value = "val", "2020-01-13" : "2020-04-13")
mobi$date <- as.Date(mobi$date, "%Y-%m-%d")



mobi <- as.data.frame(mobi)

# Add a continent colum
mobi$continent <- countrycode(sourcevar = mobi[, "region"],
                            origin = "country.name",
                            destination = "continent")

mobi_c <- mobi %>% filter(geo_type == "country/region")

mobi_c$transportation_type[mobi_c$transportation_type == "driving"] <- "Driving"
mobi_c$transportation_type[mobi_c$transportation_type == "walking"] <- "Walking"
mobi_c$transportation_type[mobi_c$transportation_type == "transit"] <- "Transit"
mobi_c$region[mobi_c$region == "United States"] <- "US"

mobi_plot <- mobi_c %>%
  mutate(smoothed = roll_meanr(val, 7)) %>% 
  filter(region %in% c("UK", "Italy", "France", "Spain", "Sweden", "Germany", "US")) # Select the same country as above

country <- mobi_plot %>%
  ggplot(aes(date, smoothed)) +
  geom_line(aes(col = region), size = 0.5) +
  scale_x_date(limits = c(as.Date("2020-02-21"), as.Date("2020-04-14")), 
               breaks = c(as.Date("2020-02-22"), as.Date("2020-04-05")), 
               labels = function(x)format(x,"%d %b")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 140, 20), 
                     expand = c(0,0), 
                     sec.axis = dup_axis()) +
  facet_wrap(~transportation_type, nrow = 1) +
  theme_delabj() +
  scale_color_delabj(name = "") +
  labs(x = "Date",
       title = "Mobility - Country level") +
  theme(
    text = element_text(size = 12, family="ITC Officina Sans LT Book"),
    axis.title.x = element_text(family = "ITC Officina Sans LT Bold"),
    plot.title = element_text(size = 15, family="ITC Officina Sans LT Bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(),
    axis.title = element_blank(),
    legend.position = "top"
  ) +
  ggsave("apple_mobility.png", dpi = 320, width = 12, height = 8)



mobi_plot2 <- aggregate(mobi_c$val, list(mobi_c$transportation_type, mobi_c$date, mobi_c$continent), mean)
colnames(mobi_plot2) <- c("trans", "date", "region", "val")

mobi_plot2 <- mobi_plot2 %>%
  mutate(smoothed = roll_meanr(val, 7))

mobi_plot2 <- mobi_plot2 %>%
  mutate(end_label = ifelse(date == max(date), region, NA))
mobi_plot2

conti <- mobi_plot2 %>%
  ggplot(aes(x = date, y = smoothed, color = region,  group = region)) +
  geom_line(size = 0.5) +
  scale_x_date(limits = c(as.Date("2020-02-21"), as.Date("2020-04-14")), 
               breaks = c(as.Date("2020-02-22"), as.Date("2020-04-05")), 
               labels = function(x)format(x,"%d %b")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 140, 20), 
                     expand = c(0,0), 
                     sec.axis = dup_axis()) +
  facet_wrap(~trans, nrow = 1) +
  theme_delabj() +
  scale_color_delabj(name = "") +
  labs(x = "\nDate",
       title = "Mobility - Continent level",
       subtitle = "By transportation type",
       caption = "Sources: Apple Maps; Google\n  www.fishwongy.com") +
  theme(
    text = element_text(size = 12, family="ITC Officina Sans LT Book"),
    plot.title.position = "plot",
    plot.title = element_text(size = 15, family="ITC Officina Sans LT Bold"),
    plot.caption = element_text(hjust = -0.18, size = 11, family="ITC Officina Sans LT Book"),
    axis.title.x = element_text(family = "ITC Officina Sans LT Bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(),
    axis.title = element_blank(),
    legend.position = "top"
  ) 
  #ggsave("cont_mobility.png", dpi = 320, width = 12, height = 8)


# patchwork
ft / (conti | country) +
  plot_annotation(
                  theme = theme(plot.title = element_text(size = 20), 
                                plot.subtitle = element_text(size = 14))) &
  theme(plot.title = element_text(family = "ITC Officina Sans LT Bold"),
    text = element_text("ITC Officina Sans LT Book")) 

+
  ggsave("/Volumes/My Passport for Mac/R/convid-19/15 Apr 20/Photo/mobility/mobility_patch.png", dpi = 320, width = 10.8, height = 12)

# create data frame of maps::world data
worlddata <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

worlddata <- worlddata %>% mutate(region = str_replace(region, "United Kingdom", "UK"),
                                  region = str_replace(region, "USA", "US"))


mobi_c_map <- mobi_c %>% filter(date == as.Date("2020-04-13"))
mobi_c_map <- aggregate(mobi_c_map$val, list(mobi_c_map$region, mobi_c_map$date), mean)
colnames(mobi_c_map) <- c("region", "date", "val")

# create map
map <- 
  ggplot() +
  geom_map(data = worlddata, map = worlddata, aes(map_id = region), fill = 'grey90', colour = 'grey60', size = 0.5) +
  expand_limits(x = worlddata$long, y = worlddata$lat) +
  geom_map(data = mobi_c_map, map = worlddata,
           aes(fill = val, map_id = region), alpha = 0.8) +
   scale_fill_viridis(option = "inferno", direction = -1, name = "Mobility",
                     guide = guide_colorbar(direction = "horizontal", 
                                            barheight = unit(2, units = "mm"),
                                            barwidth = unit(50, units = "mm"),
                                            draw.ulim = F, title.position = 'top', 
                                            title.hjust = 0.5, label.hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white')) +
  theme_delabj() +
  labs(x = "", y = "",
       title = "Where are you going?",
       subtitle = "Mobility per country, April 2020") +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        #legend.key.width = unit(2, 'cm'),
        legend.position = 'top',
        legend.title = element_text(family = 'ITC Officina Sans LT Book', size = 10),
        legend.text = element_text(family = 'ITC Officina Sans LT Book', size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 12, family="ITC Officina Sans LT Book"),
        plot.title = element_text(size = 15, face = "bold", family="ITC Officina Sans LT Bold"),
        plot.subtitle = element_text(hjust = 0, vjust = 2.5,family="ITC Officina Sans LT Book"),
        plot.caption = element_text(hjust = 0, size = 10)) +
  ggsave("map_mobility.png", dpi = 320, width = 12, height = 8)

# patchwork
map / (conti | ft) +
  plot_annotation(theme = theme(plot.title = element_text(size = 20)))& 
                                #plot.subtitle = element_text(size = 14))) &
  theme(plot.title = element_text(family = "ITC Officina Sans LT Bold"),
    text = element_text("ITC Officina Sans LT Book")) 

+
  ggsave("mobility_patch2.png", dpi = 320, width = 10.8, height = 12)
