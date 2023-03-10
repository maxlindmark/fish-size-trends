library(sf)

sf::sf_use_s2(FALSE)

# Specify map ranges
ymin = 48; ymax = 64; xmin = -8; xmax = 14

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", continent = "europe")

# Crop the polygon for plotting and efficiency:
# st_bbox(map_data) # find the rough coordinates
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

# Transform our map into UTM 33 coordinates, which is the equal-area projection we fit in:
utm_zone31N <- 32631
ns_coast_proj <- sf::st_transform(ns_coast, crs = utm_zone31N)

# Define plotting theme for main plot
theme_plot <- function(base_size = 11, base_family = "") {
  theme_light(base_size = base_size, base_family = "") +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(0.2, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, -5, -5, -5),
      strip.text = element_text(size = 9, colour = 'gray10', margin = margin(b = 1, t = 1)),
      strip.background = element_rect(fill = "grey95"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Define plotting theme for facet_wrap map with years
theme_facet_map <- function(base_size = 11, base_family = "") {
  theme_light(base_size = base_size, base_family = "") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text = element_text(size = 7),
      strip.text = element_text(size = 8, colour = 'gray10', margin = margin(b = 1, t = 1)),
      strip.background = element_rect(fill = "gray95"),
      legend.direction = "horizontal",
      legend.margin = margin(1, 1, 1, 1),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.key.height = unit(0.4, "line"),
      legend.key.width = unit(2, "line"),
      legend.spacing.x = unit(0.1, 'cm'),
      legend.position = "bottom",
    )
}

# https://pbs-assess.github.io/sdmTMB/articles/pretty-plots.html
sf::st_boundary(ns_coast_proj)

# Make default base map plot
xmin2 <- 100000
xmax2 <- 1171064*0.785

ymin2 <- 5326582*1.06
ymax2 <- 6906891*0.986

plot_map <- 
  ggplot(ns_coast_proj) + 
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3) + 
  theme_plot() +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5),
         fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  NULL

plot_map

# plot_map_fc <- 
#   ggplot(swe_coast_proj) + 
#   xlim(xmin2, xmax2) +
#   ylim(ymin2, ymax2) +
#   labs(x = "Longitude", y = "Latitude") +
#   geom_sf(size = 0.3) + 
#   theme_facet_map() +
#   guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5),
#          fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
#   NULL

# plot_map_labels <- 
#   plot_map + 
#   annotate("text", label = "Sweden", x = xmin2 + 0.25*xrange, y = ymin2 + 0.75*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Denmark", x = xmin2 + 0.029*xrange, y = ymin2 + 0.32*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Germany", x = xmin2 + 0.07*xrange, y = ymin2 + 0.022*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Poland", x = xmin2 + 0.55*xrange, y = ymin2 + 0.08*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Russia", x = xmin2 + 0.95*xrange, y = ymin2 + 0.18*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Lithuania", x = xmin2 + 1*xrange, y = ymin2 + 0.43*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Latvia", x = xmin2 + 0.99*xrange, y = ymin2 + 0.65*yrange, color = "black", size = 1.9, angle = 75)
# 
# plot_map_labels_fc <- 
#   plot_map_fc + 
#   annotate("text", label = "Sweden", x = xmin2 + 0.25*xrange, y = ymin2 + 0.75*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Denmark", x = xmin2 + 0.029*xrange, y = ymin2 + 0.32*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Germany", x = xmin2 + 0.07*xrange, y = ymin2 + 0.022*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Poland", x = xmin2 + 0.55*xrange, y = ymin2 + 0.08*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Russia", x = xmin2 + 0.95*xrange, y = ymin2 + 0.18*yrange, color = "black", size = 1.9) +
#   annotate("text", label = "Lithuania", x = xmin2 + 1*xrange, y = ymin2 + 0.43*yrange, color = "black", size = 1.9, angle = 75) +
#   annotate("text", label = "Latvia", x = xmin2 + 0.99*xrange, y = ymin2 + 0.65*yrange, color = "black", size = 1.9, angle = 75)

# Diet map plot here!