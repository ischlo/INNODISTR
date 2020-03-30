#####INSTALL PACKAGES IF NOT IN THE LIST ####

install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"), repos = "https://www.stats.bris.ac.uk/R/")

#sf: simple features, standard way to encode spatial vector data
#tmap: layer-based and easy approach to make thematic maps
#tmaptools: set of tools for reading and processing spatial data
#RSQLite: embeds the SQLite database engine in R

install.packages("rgdal") # FOR MAC

install.packages("raster") # TO MANIPULATE RASTERS

install.packages("shinyjs") #FOR COOL COLORED MAPS

install.packages("leaflet.extras")
install.packages("igraph")
install.packages("stplanr")


#### LOAD LIBRARIEs ####

library(rgdal)
library(sf) #TO READ SHAPEFILES 
library(sp) 
library(tidyverse) #TO MANIPULATE CSV FILES 
library(tmap) #TO PLOT MAPS
library(tmaptools) 
library(readr)
library(RSQLite)  #TO CONNECT CSV DATA TO GEOPACKAGE
library(raster)
library(tibble)
library(leaflet)
library(leaflet.extras)
library(maptools)
library(rgeos)
library(rgdal)
library(osmdata)
library(ggmap)
library(ggplot2)
library(osmar)
library(igraph)
library(stplanr)
library(png)
library(gdalUtils)
library(plotly)
library(spatstat)

getwd() #GET WORKING DIRECTORY
setwd("/Users/ivann/Desktop/GIID/Melbourne") #TO SET WORKING DIRECTORY

######## READING DATA FROM OSM AT THE END OF THE FILE 

tmap_mode("view")

######## MAPPING THE BUILDINGS #####
tm_shape(pbuild$osm_polygons) +
  tm_polygons("amenity",
              id = "name",
              alpha = 0.5, 
              style = "cat", 
              legend.show = F,
              colorNA ="grey",
              textNA = "grey",
              border.col = "black",
              border.alpha = 0.7
              )
######### MAPPING SOME Building POINTS #######
tm_shape(pbuild$osm_points) +
  tm_dots(
    col = "name",
    size = 0.02,
    labels = "name",
    legend.show = F
  ) +
  tmap_options(max.categories = 1000)
#tm_fill(palette = "YlOrRd")

#####MAPPING THE PARKS AND AREAS#####
  tm_shape(pamenity$osm_polygons) +
  tm_polygons("name", 
              alpha = 0.5, 
              style = "cat", 
              legend.show = FALSE,
              colorNA ="grey",
              textNA = "grey",
              border.col = "black",
              border.alpha = 0.7
              )


 
###### MAPPING THE ROAD NETWORK DATA ######
#points
tm_shape(phigh$osm_points) +
  tm_dots(
    col = "name",
    size = 0.06,
    labels = "name",
    legend.show = F,
    colorNA = "grey",
    shapes.labels = "name"
  )

#roads
tm_shape(phigh$osm_lines) +
  tm_lines(
      col = "name",
      lwd = 3,
      lty = "solid",
      alpha = 1,
      scale = 1,
      lwd.legend = NULL,
      lwd.legend.labels = NULL,
      n = 20,
      style = "cat",
      breaks = NULL,
      interval.closure = NULL,
      palette = "Set1",
      #labels = NULL,
      midpoint = NA,
      stretch.palette = TRUE,
      #contrast = NA,
      colorNA = "grey",
      textNA = "Missing",
      showNA = TRUE,
      colorNULL = "grey",
      legend.col.show = F
      ) +
  tmap_options(max.categories = 1000)

#### PROJECTION OF THE MAP  ####
crs <-  "+proj=longlat +datum=WGS84 +no_defs"

#### TRANSFORMING THE ROADS NETWORK FROM LINESTRING OBJECTS TO A SPATIAL LINES NETWORK ####
# INSPIRED BY CHAPTER 12.8 https://geocompr.robinlovelace.net/transport.html#route-networks 
waysMel <- as(roadNetwork,"Spatial") %>% SpatialLinesNetwork() 

# CHECKS 
slotNames(waysMel)
waysMel@weightfield
class(waysMel@g)

waysMel@nb
##
 
#########CALCULATING BETWEENNESS #############
#WITH THE NODS OF THE NETWORK TO CALCULATE THE CONNECTIVITY OF A ROAD
e = edge_betweenness(waysMel@g)
summary(e)

#########SAVING THE CONNECTIVITY IMAGE############

png(filename="networkAnalysis.png")
tm_shape(waysMel@sl) + tm_lines(lwd = e/3000) + tmap_options(limits = c(facets.view = 1,facets.plot = 1))
dev.off()

#Betweeneness map

#tm_shape(waysMel@sl) + tm_lines(lwd = e/3000) + tmap_options(limits = c(facets.view = 1,facets.plot = 1))

###### (NOT USED)  FIND THE ROUTES BETWEEN SPECIFIED START AND END POINTS#######
#VERY TIME CONSUMING

# network_routes <- sum_network_routes(waysMel,
#                        start = waysMel@sl$id[1],
#                        end = waysMel@sl$id,
#                        combinations = T,
#                        sumvars = "length"
#                        )
# bons <- filter(network_routes@data, network_routes@data$pathfound == TRUE) 
# bonsID <- bons$ID
# length(bonsID)
# qtm(network_routes[bonsID,]) + qtm(phigh$osm_lines$geometry, lines.col = "green")


#class(network_routes)
####  ADAPTING CONNECTIVITY DATA  ####

view(waysMel@sl)
view(e)

e <-as.data.frame(e)
e$id <- c(1:length(e$e))
colnames(e)

waysMelSL <-merge(waysMel@sl,log(e), by.x = "id", by.y = "id")

########### COLORMAP OF CONNECTIVITY  (a la space syntax) #############

# normalisation of e #
hist(log1p(e$e))
range(log1p(e$e))
log(max(e$e))
waysMelSL$e <- log1p(e$e)

tm_shape(waysMelSL) + 
  tm_lines(col = "e"
           ,style = "fixed"
           ,palette = "viridis"
           ,lwd =2.5
           ,n=7
           ,contrast = c(0, 1)
           ,breaks = c(0,1,7,11,13)  ## log or lin scale  log : c(0,2,4,6,8,10,12) ; lin : c(0,100,1000,10000,350000,60000)
           ,alpha = 1
           ,legend.col.show = F
  ) +
  # tm_shape(melbourneID) + 
  # tm_borders(col = "black"
  #            ,lwd = 5
  #            ) +
  # tmap_options(limits = c(facets.view = 1,facets.plot = 1)) +
  tmap_options(max.categories = 1000)

##### connectivity map with porosity file commercial ponits included ######

# tm_shape(waysMel@sl) + 
#   tm_lines(#col = "name"
#            alpha = 0.8
#            ,lwd = log(e$e/5) ### logarythm to have a bigger dumping and a wider range, otherwise use e/3000
#            ) + 
#   tmap_options(limits = c(facets.view = 1,facets.plot = 1)) +
tm_shape(porosity) +
  tm_dots(
    col = "amenity",
    id = "name",
    size = 0.06,
    labels = "amenity",
    legend.show = F,
    colorNA = "grey",
  ) +
  tmap_options(max.categories = 1000)

# Sorted by type of amenity : 

tm_shape(Commercedata) +
  tm_dots(col = "legend",
          id = "name",
          title = "Type of amenity",
          size = 0.02
            )

#Density
tm_shape(waysMelSL) + 
  tm_lines(col = "black"
           ,lwd =1
           ,alpha = 0.9
           ,legend.col.show = F
  ) + tm_shape(Commercedata) +
  tm_dots(col = "orange",
          alpha = 0.7,
          size = 0.01)

tm_shape(melbourneID) +
  tm_fill(col = "employment",
          breaks = c(0,500,1000,2000,3000,4000,8000,12000)
          )





#### Attempt to make a density plot of commercial points ####

# Commercedata$lon <- latlong$lon
# Commercedata$lat <- latlong$lat
# 
# latlong <- do.call(rbind, st_geometry(Commercedata)) %>% 
#   as_tibble() %>% setNames(c("lon","lat"))
# 
# densowin <- owin(xrange = c(min(latlong$lon),max(latlong$lon)),yrange = c(min(latlong$lat),max(latlong$lat)))
# 
# commercedens <- ppp(latlong$lon,latlong$lat,densowin)
# 
# Q <- quadratcount(commercedens, nx= , ny=100)
# Q.d <- intensity(Q)
# 
# view(Q)
# 
# plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
#plot(Commercedata, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# ggplot(Commercedata, aes(x = lon, y = lat)) + 
#   coord_equal() + 
#   xlab('Longitude') + 
#   ylab('Latitude') + 
#   stat_density2d(aes(fill = ..level..), alpha = .5,
#                  geom = "polygon", data = Commercedata) + 
#   scale_fill_viridis_c() + 
#   theme(legend.position = 'none')
# 


view(porosity)

############################### CLEANING THE DATA #####################################

# FROM THE OSM API DATA WE KEEP ONLY THE FOLLOWING COLUMNS: c("osm_id","name","amenity","geometry")

view(colnames(porosity))

#### AMENITY 

# amenity points

sumamenpoint <- vector(mode = "list",length = length(pamenity$osm_points))
names(sumamenpoint) <- colnames(pamenity$osm_points)

for (i in 3:length(pamenity$osm_points)-1) {
  sumamenpoint[[i]] <- summary(as.factor(pamenity$osm_points[[i]]))
}
#summary of amenity points data
view(sumamenpoint)
#checks
view(pamenity$osm_points[is.na(pamenity$osm_points$microbrewery) == F, ]) %>% qtm()

view(pamenity$osm_points[which(pamenity$osm_points$amenity %in% x),]) %>% qtm()

#Extracting points of interest for commerce commercial activity study

x <- c("bar","bank","clinic","dentist","post_office","bbq","cafe","car_rental","stripclub","studio","car_wash","cinema","fast_food","food_court","ice_cream","internet_cafe","marketplace","nightclub","pharmacy","pub","restaurant")

y <- c("Bar","Bank","Clinic","Dentist","Post Office","BBQ","Cafe","Car Rental","Stripclub","Studio","Car Wash","Cinema","Fast Food","Food Court","Ice Cream","Internet Cafe","Marketplace","Nightclub","Pharmacy","Pub","Restaurant")

z <- list(x,y)
names(z) <- c("osm_value","legend")
z$legend[which(z$osm_value== "stripclub")] 
Commercedata <- pamenity$osm_points[which(pamenity$osm_points$amenity %in% x),]

k <- Commercedata$amenity

for (i in 1:length(k)) {
  k[i] <- z$legend[which(z$osm_value== k[i])] 
}
# Giving nice name to legend column
Commercedata$legend <- k

tm_shape(pamenity$osm_points) + 
  tm_dots(col = "amenity",
          id = "name",
          size = 0.06,
          legend.show = F
          ) +
  tmap_options(max.categories = 72)


# AMENITY POINTS CLEANING
#keep only those rows for which an amenity type is specified. amenity column != NA
pamenity$osm_points <-  pamenity$osm_points[is.na(pamenity$osm_points$amenity)==F,c("osm_id","name","amenity","geometry")]

## amenity lines

view(pamenity$osm_lines)

pamenity$osm_lines <- NULL #there was no information in this file 

# AMENITY POLYGONS, KEEP THE ONES THAT HAVE EITHER AMENITY OR NAME SPECIFIED
view(pamenity$osm_polygons)
pamenity$osm_polygons <-  pamenity$osm_polygons[is.na(pamenity$osm_polygons$amenity)==F | is.na(pamenity$osm_polygons$name)== F,c("osm_id","name","amenity","geometry")]

qtm(pamenity$osm_polygons)

#  AMENITY MULTILINES

view(pamenity$osm_multilines)

pamenity$osm_multilines <- NULL #only 2 elements, not usefull 

# amenity multipolygons

view(pamenity$osm_multipolygons$geometry)
qtm(pamenity$osm_multipolygons$geometry)

pamenity$osm_multipolygons <- NULL # No use as well for this data

# BUILDINGS
#points
pbuild$osm_points <- pbuild$osm_points[is.na(pbuild$osm_points$name) == F | is.na(pbuild$osm_points$amenity) == F,c("osm_id","name","amenity","geometry")]

view(pbuild$osm_points) %>% qtm()

#lines is fully empty so it's just erased

pbuild$osm_lines <- NULL

# for the buildings, the polygons column is not touched because it contains all the shapes of the buildings. 

view(pbuild$osm_polygons[is.na(pbuild$osm_polygons$name) == T & is.na(pbuild$osm_polygons$amenity)==T & is.na(pbuild$osm_polygons$building) == T,] ) %>% qtm()

# multilines

view(pbuild$osm_multilines) %>% qtm()

pbuild$osm_multilines <- NULL

# multipolygons, not changed

view(pbuild$osm_multipolygons) %>% qtm()

#HIGHWAYS 
#points
view(phigh$osm_points[is.na(phigh$osm_points$name)== F | is.na(phigh$osm_points$amenity)== F,])
length(phigh$osm_points$highway[is.na(phigh$osm_points$highway) == F])
length(phigh$osm_points$highway)

view(phigh$osm_points[1,])

c <- colnames(phigh$osm_points)
view(c)

x <-c(1,2,9,13:18,25:30,33:35,38:40,55,57:59,68:72,77,82,87:97,100)

phigh$osm_points <- phigh$osm_points[,x]

sumhighpoints <-  vector(mode = "list", length = 44)
names(sumhighpoints) <- colnames(phigh$osm_points)
x <- colnames(phigh$osm_points)

# sumhighpoints <- NULL

for (i in 3:43){
  sumhighpoints[[i]] <- summary(as.factor(phigh$osm_points[[i]]))
}

view(sumhighpoints)

qtm(phigh$osm_points[is.na(phigh$osm_points$railway) == F,])


# Lines 
summary(as.factor(phigh$osm_lines$footway))
view(phigh$osm_lines[is.na(phigh$osm_lines$highway) == F,])

view(phigh$osm_lines[is.na(phigh$osm_lines$highway) == T,]) %>% qtm()

chighlines <- colnames(phigh$osm_lines)

view(chighlines)

x1 <- c(1,2,10,14:16,19:20,22,26:35,50,51,56,57,65,69:72,91,92,106,107,118,119,122,125:131,152)

phigh$osm_lines <- phigh$osm_lines[,x1]

sumhighlines <- vector(mode = "list",length = 43)
names(sumhighlines) <- colnames(phigh$osm_lines)

for (i in 3:40) {
  sumhighlines[[i]] <- summary(as.factor(phigh$osm_lines[[i]]))
}

view(sumhighlines)

view(phigh$osm_lines[which(phigh$osm_lines$junction == "roundabout"),])

typeof(phigh$osm_lines$footway)

view(phigh$osm_lines[which(phigh$osm_lines$name == "Elizabeth Street"),]) %>% qtm()

qtm(phigh$osm_lines[which(phigh$osm_lines$footway == "sidewalk"),])

# roadnetwork that is studied for walking connectivity

roadNetwork <- phigh$osm_lines[which(phigh$osm_lines$highway =="footway" | 
                          is.na(phigh$osm_lines$cycleway) == F | 
                          phigh$osm_lines$highway == "steps" |
                          phigh$osm_lines$highway == "pedestrian" |
                          phigh$osm_lines$highway == "corridor" |
                          phigh$osm_lines$highway == "service" |
                          phigh$osm_lines$highway == "tertiary" |
                          phigh$osm_lines$highway == "residential" |
                          phigh$osm_lines$junction == "roundabout" |
                          phigh$osm_polygons$junction == "roundabout" |
                          phigh$osm_lines$highway == "primary"),]

qtm(roadNetwork)

view(phigh$osm_lines[which(phigh$osm_lines$junction == "roundabout"),]) %>% qtm()

view(phigh$osm_lines[is.na(phigh$osm_lines$cycleway) == F,])

qtm(phigh$osm_lines)

#qtm(phigh$osm_lines)

# HIGHWAY Polygons

view(phigh$osm_polygons)

sumhighpoly <- vector(mode = "list",length = length(phigh$osm_polygons))
names(sumhighpoly) <- colnames(phigh$osm_polygons)

for (i in 3:length(phigh$osm_polygons)-1) {
  sumhighpoly[[i]] <- summary(as.factor(phigh$osm_polygons[[i]]))
}

view(sumhighpoly)

qtm(phigh$osm_polygons[which(phigh$osm_polygons$junction == "roundabout"),])

# highway Multilines

view(phigh$osm_multilines) #%>% qtm()
phigh$osm_multilines <- NULL

# highways multipolygons
# unused
class(phigh$osm_multipolygons)
view(phigh$osm_multipolygons)

##
############ POROSITY #########
colnames(porosity)
class(porosity)

view(porosity[is.na(porosity$religion)== F,])
view(porosity)
summary(porosity$amenity)

porosityGIID <- porosity[grep("^GIID",colnames(porosity))]

porosity <- porosity[is.na(porosity$name) == F,]
porosity <- porosity[,-2]

view(porosity)
summary(porosity$name)

############################### LOADING THE DATA ######################################

# Melbourne inno district shape

melbourneID <- st_read("MelbourneID/melbourne.shp")
head(melbourneID)
qtm(melbourneID)
melbourneID$employment

#porosity <- NULL
porosity <- st_read("Porosity-Data/200303_GIID_Porosity-Data.shp")

# QUERY TO GET THE OSM DATA FROM A BBOX BASED ON KEYS

#BBOX
bbMel <- c(144.9370,-37.8175,144.9951,-37.7841)
# Large bbox : c(144.9370,-37.8175,144.9951,-37.7841)
# inno distr bbox : c(144.9507,-37.8112,144.9764,-37.7913)

# KEY = BUILDING FOR ALL THAT IS BUILT
pbuild <- opq(bbox = bbMel,timeout = 600) %>% #timeout in seconds
  add_osm_feature(key = "building"
                  #,value = "appartments"
                  ,key_exact = TRUE
                  #,value_exact = FALSE
                  #,match_case = FALSE
                  ) %>%
  osmdata_sf()
pbuild

# KEY = HIGHWAYS FOR ALL THE ROADS 
phigh <- opq(bbox = bbMel,timeout = 600) %>% 
  add_osm_feature(key = "highway"
                  #,value = "appartments"
                  ,key_exact = TRUE
                  #,value_exact = FALSE
                  #,match_case = FALSE
                  ) %>%
  osmdata_sf()
phigh

### KEY = AMENITY 

pamenity <- opq(bbox = bbMel,timeout = 600) %>%
  add_osm_feature(key = "amenity"
                  #,value = "appartments"
                  ,key_exact = TRUE
                  #,value_exact = FALSE
                  #,match_case = FALSE
                  ) %>%
  osmdata_sf()

pamenity

summary(pbuild)
summary(phigh)
summary(pamenity)

#################### write buildings to dxf ############## 
#DOES NOT WORK

buildDXF <- as(pbuild$osm_polygons,"Spatial")
class(buildDXF)
writeOGR(obj = buildDXF,
         dsn = "buildings",
         layer = "polygons",
         driver = "DXF",
         verbose = T,
         overwrite_layer = T
         )

#INSPIRED BY http://r-sig-geo.2731867.n2.nabble.com/save-SpatialPolygonsDataFrame-as-dxf-file-td7589253.html

 writeOGR(obj = buildDXF[,c(1,2)],
          dsn = "buildings",
          layer = "polygons",#
          driver="ESRI Shapefile",
          overwrite_layer = T
          )

 ogr2ogr("buildings/polygons.shp", "buildings.dxf", "polygons", "DXF")

# INSPIRED BY: https://cran.r-project.org/web/packages/rgdal/rgdal.pdf

 td <- file.path(getwd(), "buildings"); dir.create(td)
 td
 # BDR 2016-12-15 (MapInfo driver fails writing to directory with ".")
 if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
   OLDPWD <- getwd()
   setwd(td)
   td <- "."
 }
 writeOGR(buildDXF, td, "polygons", driver="DXF")
 try(writeOGR(buildDXF, td, "polygons", driver="DXF"))
 writeOGR(buildDXF, td, "polygons", driver="DXF", overwrite_layer=TRUE)
 ogrDrivers()
