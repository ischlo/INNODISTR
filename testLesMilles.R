#INSTALL PACKAGES IF NOT IN THE LIST 

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


# LOAD LIBRARIES

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
library(osmar)
library(igraph)
library(stplanr)
library(png)
library(gdalUtils)

getwd() #GET WORKING DIRECTORY
setwd("/Users/ivann/Desktop/GIS/Experiment/testsLesMilles") #TO SET WORKING DIRECTORY

tmap_mode("plot")

# MAPPING THE BUILDINGS 
tm_shape(pbuild$osm_polygons) +
  tm_polygons("name", 
              alpha = 0.5, 
              style = "cat", 
              legend.show = FALSE,
              colorNA ="grey",
              textNA = "grey",
              border.col = "black",
              border.alpha = 0.7
              )

#MAPPING THE PARKS AND AREAS
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

# SOME POINTS 
tm_shape(pbuild$osm_points) +
  tm_dots(
    col = "name",
    size = 0.02,
    labels = "name",
    legend.show = F
  ) +
  tmap_options(max.categories = 1000)
  #tm_fill(palette = "YlOrRd")
 
#  MAPPING THE ROAD NETWORK

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

# PROJECTION OF THE MAP  
crs <-  "+proj=longlat +datum=WGS84 +no_defs"

#TRANSFORMING THE ROADS NETWORK FROM LINESTRING OBJECTS TO A SPATIAL LINES NETWORK
# INSPIRED BY CHAPTER 12.8 https://geocompr.robinlovelace.net/transport.html#route-networks 
waysMel <- as(phigh$osm_lines,"Spatial") %>% SpatialLinesNetwork() 

# CHECKS 
slotNames(waysMel)
weightfield(waysMel)
class(waysMel@g)
##
 
# DOING SOMETHING WITH THE NODS OF THE NETWORK TO CALCULATE THE CONNECTIVITY OF A ROAD
e = edge_betweenness(waysMel@g)
summary(e)
#####################

png(filename="networkAnalysis.png")
tm_shape(waysMel@sl) + tm_lines(lwd = e/3000) + tmap_options(limits = c(facets.view = 1,facets.plot = 1))
dev.off()

# FIND THE ROUTES BETWEEN SPECIFIED START AND END POINTS
#VERY TIME CONSUMING

network_routes <- sum_network_routes(waysMel,
                       start = waysMel@sl$id[1],
                       end = waysMel@sl$id,
                       combinations = T,
                       sumvars = "length"
                       )
bons <- filter(network_routes@data, network_routes@data$pathfound == TRUE) 
bonsID <- bons$ID
length(bonsID)
qtm(network_routes[bonsID,]) + qtm(phigh$osm_lines$geometry, lines.col = "green")


class(network_routes)

############################### LOADING THE DATA ######################################
# QUERY TO GET THE OSM DATA FROM A BBOX BASED ON KEYS

#BBOX
bbMel <- c(144.9507,-37.8112,144.9764,-37.7913)

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

pamenity <- opq(bbox = bbMel,timeout = 600) %>%
  add_osm_feature(key = "amenity"
                  #,value = "appartments"
                  ,key_exact = TRUE
                  #,value_exact = FALSE
                  #,match_case = FALSE
                  ) %>%
  osmdata_sf()


summary(pbuild)
summary(phigh)
summary(pamenity)

################# write buildings to dxf ############## DOES NOT WORK

buildDXF <- as(pbuild$osm_polygons,"Spatial")
class(buildDXF)
# writeOGR(obj = buildDXF,
#          dsn = "buildings",
#          layer = "polygons",
#          driver = "DXF",
#          verbose = T,
#          overwrite_layer = T
#          )

#INSPIRED BY http://r-sig-geo.2731867.n2.nabble.com/save-SpatialPolygonsDataFrame-as-dxf-file-td7589253.html

# writeOGR(obj = buildDXF[,c(1,2)], 
#          dsn = "buildings", 
#          layer = "polygons", 
#          driver="ESRI Shapefile",
#          overwrite_layer = T
#          )
# 
# ogr2ogr("buildings/polygons.shp", "buildings.dxf", "polygons", "DXF")

# INSPIRED BY: https://cran.r-project.org/web/packages/rgdal/rgdal.pdf 

# td <- file.path(getwd(), "buildings"); dir.create(td)
# td
# # BDR 2016-12-15 (MapInfo driver fails writing to directory with ".")
# if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
#   OLDPWD <- getwd()
#   setwd(td)
#   td <- "."
# }
# writeOGR(buildDXF, td, "polygons", driver="DXF")
# try(writeOGR(buildDXF, td, "polygons", driver="DXF"))
# writeOGR(buildDXF, td, "polygons", driver="DXF", overwrite_layer=TRUE)
# ogrDrivers()
