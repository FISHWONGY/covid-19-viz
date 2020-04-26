interactive <- 
  tm_shape(g) +
  tm_lines(col = "grey", alpha = .3) +
tm_shape(w_yesterday2) +
  tm_polygons(c("Confirmed", "Deaths"),
    palette = "-viridis",
    style = "log10") +
  tm_style("cobalt") +
 tm_layout(title = "Covid-19 data",title.size = 0.8,title.fontfamily = "ITC Officina Sans LT Bold", title.position = c(0.75, 0.95),
           legend.title.size = 1,
          legend.text.size = 0.6,
           #legend.title.color = "black",
           #legend.text.color = "black",
            fontfamily = "ITC Officina Sans LT Book",
            legend.title.fontfamily = "ITC Officina Sans LT Bold") +
  tm_view(view.legend.position = c(0.81, 0.05) #0.01, 0.1
          )
          
interactive <- tmap_leaflet(interactive)
interactive

saveWidget(interactive, "US_map.html")
