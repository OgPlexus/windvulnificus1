############################################################
##Making Maps in R, specifically a Florida Map with NOAA buoys
##written by Andrea Ayala, last change 1/5/23
##https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
##https://geocompr.robinlovelace.net/spatial-class.html
##https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r
##https://www.thinkingondata.com/something-about-viridis-library/
##https://viz-ggplot2.rsquaredacademy.com/ggplot2-labels.html
##https://stackoverflow.com/questions/49622822/control-colour-of-geom-text-repel
##http://r-statistics.co/Complete-Ggplot2-Tutorial-Part2-Customizing-Theme-With-R-Code.html
##https://geanders.github.io/navy_public_health/3-2-basic-mapping.html
##https://stackoverflow.com/questions/66943405/change-colours-of-filling-and-not-just-borders-in-geom-sf
##https://stackoverflow.com/questions/71772390/how-do-i-fill-certain-counties-on-a-us-map-in-r
##https://community.rstudio.com/t/adding-county-names-to-map-in-maps-package/55386
#####################################################################################

############################################################

rm(list=ls()) #this clears the workspace to make sure no leftover variables are left. Not strictly needed but often a good idea
#graphics.off(); #close all graphics windows, in case there are still some open from previous stuff we did

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
library("sf")
library(dplyr, warn.conflicts = FALSE)
library(tigris, warn.conflicts = FALSE)

#Setting the theme
theme_set(theme_bw())

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## [1] "sf"  
## [1] "data.frame"

(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
                                                                      26.83)))

##   longitude latitude
## 1 -80.14401 26.47901
## 2 -80.10900 26.83000

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)

#State names are part of this data, as the ID variable. 
#A simple (but not necessarily optimal) way to add state name is to compute 
#the centroid of each state polygon as the coordinates where to draw their names. 
#Centroids are computed with the function st_centroid, their coordinates extracted with st_coordinates, 
#both from the package sf, and attached to the state object:

states <- cbind(states, st_coordinates(st_point_on_surface(states))) #changed st_centroid to st_point_on_surface in the code

library("tools")
states$ID <- toTitleCase(states$ID)
head(states)
View(states)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

states$nudge_y <- -1
states$nudge_y[states$ID == "Florida"] <- 0.5
states$nudge_y[states$ID == "South Carolina"] <- -1.5

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)
View(counties)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#Extracting counties by buoy for map

TRDF1_counties <- counties %>% 
  filter(ID %in% c("florida,brevard", "florida,volusia"))
View(TRDF1_counties)

ARPF1_counties <- counties %>% 
  filter(ID %in% c("florida,pasco", "florida,citrus", "florida,hernando"))
View(ARPF1_counties)

PCLF1_counties <- counties %>% 
  filter(ID %in% c("florida,santa rosa", "florida,escambia", "florida,okaloosa"))
View(PCLF1_counties)

F41114_counties <- counties %>% 
  filter(ID %in% c("florida,indian river", "florida,st lucie"))
View(F41114_counties)

NPSF1_counties <- counties %>% 
  filter(ID %in% c("florida,lee", "florida,collier"))
View(NPSF1_counties)

LKWF1_counties <- counties %>% 
  filter(ID %in% c("florida,palm beach", "florida,martin"))
View(LKWF1_counties)

SAPF1_counties <- counties %>% 
  filter(ID %in% c("florida,hillsborough", "florida,pinellas", "florida,manatee"))
View(SAPF1_counties)

FRDF1_counties <- counties %>% 
  filter(ID %in% c("florida,duval", "florida,nassau"))
View(FRDF1_counties)

VAKF1_counties <- counties %>% 
  filter(ID %in% c("florida,broward", "florida,miami-dade"))
View(VAKF1_counties)

SAUF1_counties <- counties %>% 
  filter(ID %in% c("florida,st johns", "florida,flagler"))
View(SAUF1_counties)

VENF1_counties <- counties %>% 
  filter(ID %in% c("florida,sarasota", "florida,charlotte"))
View(VENF1_counties)

PACF1_counties <- counties %>% 
  filter(ID %in% c("florida,bay", "florida,walton"))
View(PACF1_counties)

APCF1_counties <- counties %>% 
  filter(ID %in% c("florida,wakulla", "florida,gulf", "florida,franklin"))
View(APCF1_counties)

KYWF1_counties <- counties %>% 
  filter(ID %in% c("florida,monroe"))
View(KYWF1_counties)

KTNF1_counties <- counties %>% 
  filter(ID %in% c("florida,taylor", "florida,dixie"))
View(KTNF1_counties)

CDKF1_counties <- counties %>% 
  filter(ID %in% c("florida,levy"))
View(CDKF1_counties)

#NOAA buoys (point data)
#To make a more complete map of Florida, buoys will be added to the map. 
#We first prepare a data frame with the relevant buoys in the state of Florida, 
#and their geographic coordinates:
  
NOAA <- data.frame(state = rep("Florida", 16), buoy = c("TRDF1", 
                                                        "ARPF1", 
                                                        "PCLF1", 
                                                        "41114", 
                                                        "NPSF1",
                                                        "LKWF1",
                                                        "SAPF1",
                                                        "FRDF1",
                                                        "VAKF1",
                                                        "SAUF1",
                                                        "VENF1",
                                                        "PACF1",
                                                        "APCF1",
                                                        "KYWF1",
                                                        "KTNF1",
                                                        "CDRF1"), 
                   lat = c(28.416, 
                           28.433, 
                           30.404, 
                           27.552, 
                           26.132,
                           26.613,
                           27.761,
                           30.675,
                           25.731,
                           29.857,
                           27.072,
                           30.152,
                           29.724,
                           24.556,
                           29.819,
                           29.136), 
                   lng = c(-80.593,
                           -82.667, 
                           -87.211, 
                           -80.216, 
                           -81.807,
                           -80.034,
                           -82.627,
                           -81.465,
                           -80.162,
                           -81.265,
                           -82.453,
                           -85.667,
                           -84.980,
                           -81.808,
                           -83.594,
                           -83.029))

(NOAA <- st_as_sf(NOAA, coords = c("lng", "lat"), remove = FALSE, 
                      crs = 4326, agr = "constant"))

#Extracting the NOAA buoys one by one 

NOAA_TRDF1<-data.frame(state = rep("Florida", 1), buoy = c("TRDF1"),lat = c(28.416), lng = c(-80.593))
(NOAA_TRDF1 <- st_as_sf(NOAA_TRDF1, coords = c("lng", "lat"), remove = FALSE, 
                  crs = 4326, agr = "constant"))
NOAA_ARPF1<-data.frame(state = rep("Florida", 1), buoy = c("ARPF1"),lat = c(28.433), lng = c(-82.667))
(NOAA_ARPF1 <- st_as_sf(NOAA_ARPF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_PCFL1<-data.frame(state = rep("Florida", 1), buoy = c("PCFL1"),lat = c(30.404), lng = c(-87.211))
(NOAA_PCFL1 <- st_as_sf(NOAA_PCFL1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_41114<-data.frame(state = rep("Florida", 1), buoy = c("41114"),lat = c(27.552), lng = c(-80.216))
(NOAA_41114 <- st_as_sf(NOAA_41114, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_NPSF1<-data.frame(state = rep("Florida", 1), buoy = c("NPSF1"),lat = c(26.132), lng = c(-81.807))
(NOAA_NPSF1 <- st_as_sf(NOAA_NPSF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_LKWF1<-data.frame(state = rep("Florida", 1), buoy = c("LKWF1"),lat = c(26.613), lng = c(-80.034))
(NOAA_LKWF1 <- st_as_sf(NOAA_LKWF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_SAPF1<-data.frame(state = rep("Florida", 1), buoy = c("SAPF1"),lat = c(27.761), lng = c(-82.627))
(NOAA_SAPF1 <- st_as_sf(NOAA_SAPF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_FRDF1<-data.frame(state = rep("Florida", 1), buoy = c("FRDF1"),lat = c(30.675), lng = c(-81.465))
(NOAA_FRDF1 <- st_as_sf(NOAA_FRDF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_VAKF1<-data.frame(state = rep("Florida", 1), buoy = c("VAKF1"),lat = c(25.731), lng = c(-80.162))
(NOAA_VAKF1 <- st_as_sf(NOAA_VAKF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_SAUF1<-data.frame(state = rep("Florida", 1), buoy = c("SAUF1"),lat = c(29.857), lng = c(-81.265))
(NOAA_SAUF1 <- st_as_sf(NOAA_SAUF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_VENF1<-data.frame(state = rep("Florida", 1), buoy = c("VENF1"),lat = c(27.072), lng = c(-82.453))
(NOAA_VENF1 <- st_as_sf(NOAA_VENF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_PACF1<-data.frame(state = rep("Florida", 1), buoy = c("PACF1"),lat = c(30.152), lng = c(-85.667))
(NOAA_PACF1 <- st_as_sf(NOAA_PACF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_APCF1<-data.frame(state = rep("Florida", 1), buoy = c("APCF1"),lat = c(29.724), lng = c(-84.980))
(NOAA_APCF1 <- st_as_sf(NOAA_APCF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_KYWF1<-data.frame(state = rep("Florida", 1), buoy = c("KYWF1"),lat = c(24.556), lng = c(-81.808))
(NOAA_KYWF1 <- st_as_sf(NOAA_KYWF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_KTNF1<-data.frame(state = rep("Florida", 1), buoy = c("KTNF1"),lat = c(29.819), lng = c(-83.594))
(NOAA_KTNF1 <- st_as_sf(NOAA_KTNF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))
NOAA_CDKF1<-data.frame(state = rep("Florida", 1), buoy = c("CDKF1"),lat = c(29.136), lng = c(-83.029))
(NOAA_CDKF1 <- st_as_sf(NOAA_CDKF1, coords = c("lng", "lat"), remove = FALSE, 
                        crs = 4326, agr = "constant"))

library("ggrepel")
library("ggspatial")
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = gray(.9), color = gray(.5)) +
  geom_sf(data = TRDF1_counties, fill = "#482677FF", color = gray(.5)) +
  geom_sf(data = ARPF1_counties, fill = "#440154FF", color = gray(.5)) +
  geom_sf(data = PCLF1_counties, fill = "#481567FF", color = gray(.5)) +
  geom_sf(data = F41114_counties, fill = "#B8DE29FF", color = gray(.5)) +
  geom_sf(data = NPSF1_counties, fill = "#404788FF", color = gray(.5)) +
  geom_sf(data = LKWF1_counties, fill = "#39568CFF", color = gray(.5)) +
  geom_sf(data = SAPF1_counties, fill = "#33638DFF", color = gray(.5)) +
  geom_sf(data = FRDF1_counties, fill = "#2D708EFF", color = gray(.5)) +
  geom_sf(data = VAKF1_counties, fill = "#287D8EFF", color = gray(.5)) +
  geom_sf(data = SAUF1_counties, fill = "#DCE319FF", color = gray(.5)) +
  geom_sf(data = VENF1_counties, fill = "#95D840FF", color = gray(.5)) +
  geom_sf(data = PACF1_counties, fill = "#73D055FF", color = gray(.5)) +
  geom_sf(data = APCF1_counties, fill = "#453781FF", color = gray(.5)) +
  geom_sf(data = KYWF1_counties, fill = "#55C667FF", color = gray(.5)) +
  geom_sf(data = KTNF1_counties, fill = "#3CBB75FF", color = gray(.5)) +
  geom_sf(data = CDKF1_counties, fill = "#238A8DFF", color = gray(.5)) +
  geom_point(data = NOAA, aes(x = lng, y = lat), color = "green") +
  geom_text_repel(data = NOAA_TRDF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#482677FF", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_ARPF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#440154FF", nudge_x = c(-1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_PCFL1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#481567FF", nudge_x = c(-1, -1.5, 2, 3, -1), nudge_y = c(-0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_41114, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#B8DE29FF", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_NPSF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#404788FF", nudge_x = c(-1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_LKWF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#39568CFF", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_SAPF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#33638DFF", nudge_x = c(-1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_FRDF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#2D708EFF", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_VAKF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#287D8EFF", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_SAUF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#DCE319FF", nudge_x = c(1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) + 
  geom_text_repel(data = NOAA_VENF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#95D840FF", nudge_x = c(-1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) + 
  geom_text_repel(data = NOAA_PACF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#73D055FF", nudge_x = c(-1, -1.5, 2, 2, -1), nudge_y = c(-0.5, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) + 
  geom_text_repel(data = NOAA_APCF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#453781FF", nudge_x = c(-1, -1.5, 2, 2, -1), nudge_y = c(-0.5, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) + 
  geom_text_repel(data = NOAA_KYWF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#55C667FF", nudge_x = c(-1, -1.5, 2, 2, -1), nudge_y = c(0.25, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) + 
  geom_text_repel(data = NOAA_KTNF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#3CBB75FF", nudge_x = c(-0.25, -1.5, 2, 3, -1), nudge_y = c(-0.4, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  geom_text_repel(data = NOAA_CDKF1, aes(x = lng, y = lat, label = buoy), 
                  fontface = "bold", colour = "#238A8DFF", nudge_x = c(-1, -1.5, 2, 3, -1), nudge_y = c(-0.1, 
                                                                                 -0.25, 0.5, 0.5, -0.5)) +
  
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
  annotation_scale() +
  xlab('Latitude') + ylab('Longtitude')
  


