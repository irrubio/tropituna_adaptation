#### Description: this script produces manuscript Figure1
#### INPUT: data from marineregions.org and ggplot2 package
#### OUTPUT: manuscript Figure1
#### Date: 08/04/2020
#### Author: Iratxe Rubio
#######################################################################

library(tidyverse)
library(rgdal) #readOGR

# Load EEZ polygons, downloaded from http://www.marineregions.org/downloads.php
eezs <- readOGR(dsn = "data/World_EEZ_v11_20191118/eez_boundaries_v11.shp")
eez_table <- fortify(eezs)

#Approximative fishing area
data <- data.frame(lon = c(-26, -16, -6, -5, 4, 3, -18), 
                   lat = c(12, 5, 0, -10, -16, -7, -3)) #positions of "fish, triangle"
data2 <- data.frame(lon = c(-28, -18, -8, -7, 2, 1, -20), 
                    lat = c(12, 5, 0, -10, -16, -7, -3)) #positions of "fish, point"

# Plot the world map with country boundaries
world <- map_data("world")

jpeg("Figure1.jpg", 
     width = 7, height = 12, units = 'in', res = 300)

ggplot() + 
  geom_path(data = eez_table, 
            aes(x = long, y = lat, group = group))  +
  geom_map(data = world, 
           map = world,
           aes(long, lat, group = group, map_id = region),
           fill = "gray88", 
           colour = "gray38") +
  coord_quickmap(xlim = c(-35,19), ylim = c(-30,42)) +
  theme_bw() + 
  xlab("lon") +
  theme(axis.text.x = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.title = element_text(size = 34)) +
  annotate("point", x = -2, y = 43, colour = "deeppink1", size = 14) +
  annotate("rect", xmin = c(-17.6, -4.3), xmax = c(-17.1, -3.8), 
           ymin = c(14.9, 5) , ymax = c(14.4, 5.5), 
           colour = "black", size = 10) +
  geom_point(data = data, aes(x = lon, y = lat), 
             shape = "\u25C4", 
             size = 11,
             colour = "turquoise4") +
  geom_point(data = data2, aes(x = lon, y = lat),
             shape = 19,
             size = 15,
             colour = "turquoise4") 

dev.off()
