#### Description: this script produces manuscript Figure1 and calculates
#### the average catch 2013-2018 within EEZs in Table 1.
#### INPUT: ICCAT data, shape files and "data_within_eez.csv"
#### OUTPUT: manuscript Figure1 and average catch 2013-2018 within EEZs
#### Date: 11/06/2020
#### Author: Iratxe Rubio
#######################################################################

library(readxl)#read_xlsx function
library(tidyverse)
library(sp) #spatial objects
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal) #readOgr
library(rgeos) # gBuffer, gOverlap


#1.Download ICCAT data and read the data####
#https://www.iccat.int/Data/t2ce_PS91-18_bySchool.7z
data_orig <- read_xlsx("data/t2ce_PS91-18_bySchool.xlsx", sheet = 1, skip = 6)

data <- filter(data_orig, Flag == "EU.España")

#2.Delete species we are not interested in####
data$ALB <- NULL
data$BLF <- NULL
data$LTA <- NULL
data$FRI <- NULL
data$TOTAL <- NULL

#Add variable total catch of tropical tuna species
data$total <- data$YFT + data$BET + data$SKJ

#Calculate total catch by latitude and longitude point
d <- data %>%
      group_by(yLat, xLon, YearC) %>%
      summarise(total = sum(total, na.rm = T))

#3.DELETE LAND POINTS####
p <- d #save table data into p and turn it into a SpatialPointsDataFrame
coordinates(p) <- c("xLon", "yLat")
# tell R that catch coordinates are in the same lat/lon reference system
# as the oceans data (for over function, both points and polygon need to have
#the same ref system)
proj4string(p) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#download oceans shape file into ocean folder inside data folder
# download.file(url = "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip",
#               destfile = "data/ocean.zip")
#unzip("data\\ocean\\ocean.zip", exdir="data/ocean")
oceans <- readOGR("data/ne_110m_ocean.shp",
                  stringsAsFactors = F, verbose = F)

# combine is.na() with over() to do the containment test; note that we
# need to "demote" oceans to a SpatialPolygons object first
inside.ocean <- !is.na(over(p, as(oceans, "SpatialPolygons")))

# use 'over' again, this time with oceans as a SpatialPolygonsDataFrame object,
# to determine which observation (if any) is outside the ocean, and
# store if the observation is ocean as an attribute of the "points" data
p$ok <- over(p, oceans)$featurecla

#draw a map
plot(coordinates(p), type = "n")
plot(oceans, border = "black", add = TRUE)
legend("topright", cex = 0.85,
       c("Points ocean", "Points land"),
       pch = c(16, 1),
       col = c("blue", "red"), bty = "n")
title(expression(paste(italic("Tunas"), ", onland data")))

#Now plot points with separate colors inside and outside of oceans
points(p[inside.ocean, ], pch = 16, col = "blue")
points(p[!inside.ocean, ], pch = 1, col = "red")
land <- p[!inside.ocean, ] #save points inside continent in an object
land <- SpatialPoints(land)

squares<-gBuffer(land, byid = T, capStyle = "SQUARE", width = 0.5)
#Create squares of 1x1 degrees around catch data
proj4string(squares)<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(squares, add = T)

#Download the continents "shapefile" from
#https://www.arcgis.com/home/item.html?id=a3cb207855b348a297ab85261743351d      
#into "continent" folder
#unzip it with 7 zip in "continent" folder, extract v104
cont <- readOGR(dsn = "data/v106/continent.gdb")
afr <- cont[cont$CONTINENT == "Africa",]#select Africa for case study

#visualizing intersection between land and squares (clip in blue)
plot(squares)
plot(afr, add = T)
plot(land,pch = 1,col="red",size=3, add = T)

i <- gOverlaps(afr, squares, byid = TRUE) #index including what points of the polygons overlap

true_land <- land[!i, ] #determine TRUE land points
plot(true_land,col = "blue", add = T)
land <- as.data.frame(true_land) #data.frame with coordinates of land points

p_land <- semi_join(d, land) #points land
tablenew <- anti_join(d, land) #save table without points inland

# Load EEZ polygons, downloaded from http://www.marineregions.org/downloads.php
eezs <- readOGR(dsn = "data/World_EEZ_v11_20191118/eez_boundaries_v11.shp")
eez_table <- fortify(eezs)

world <- map_data("world")

tablenew2 <- tablenew %>%
              group_by(yLat, xLon) %>%
              summarise(total = sum(total, na.rm = T))

#size for dots
tablenew2$size <- (tablenew2$total/1000)/28

#4.Plot and save Figure1####
jpeg("Figure1.jpg", 
     width = 15, height = 20, units = 'in', res = 300)

ggplot(tablenew2, aes(xLon, yLat)) + 
  geom_map(data = world, 
           map = world,
           aes(long, lat, group = group, map_id = region),
           fill = "gray88", 
           colour = "gray38") +
  coord_quickmap(xlim = c(-35,19), ylim = c(-21,42)) +
  geom_point(aes(size = size), shape = 21,
              fill = "darkblue",color = "black") +
  scale_size(range = c(-1,10)) +
  geom_path(data = eez_table,
            aes(x = long, y = lat, group = group)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(size = "Average yearly \ncatch (t)") +
  theme(axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        axis.title = element_text(size = 40),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30)) +
  annotate("point", x = -2.7, y = 43.4, colour = "deeppink1", size = 19) + #Bermeo coordinates
  annotate("rect", xmin = c(-17.9, -4.1), xmax = c(-16.9, -3.1), #cordinates Dakar, Abidjan
           ymin = c(14.2, 4.6) , ymax = c(13.2, 5.6), #Dakar port: x = -17.4, y = 14.7 (+0.5,-0.5)
           colour = "black", size = 13) #Abidjan port: x = -3.6, y = 5.1 (+0.5,-0.5)
dev.off()

#5. Calculate average catch 2013-2018 within EEZs####
tablenew3 <- filter(tablenew, YearC >= 2013) %>%
              group_by(yLat, xLon) %>%
              summarise(total = sum(total, na.rm = T))

#I save the data to open it in QGIS since eezs as polygons --> R crashes
#eezs_poly <- readOGR(dsn = "data/World_EEZ_v11_20191118/eez_v11.shp")
write.csv(tablenew3, "data/data_EEZs.csv")
#I save the points within EEZs through QGIS and then export the attribute table
#Estimation
data_eez <- read.csv2("data/data_within_eez.csv")
#total catch within EEZs from 2013 to 2018
sum(data_eez$total)
#proportion
(sum(data_eez$total)*100)/sum(tablenew3$total)
