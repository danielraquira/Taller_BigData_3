################################################
#                 Taller 2                     #
#                 LIMPIEZA                     #
# Colaboradores                                #
# Daniel Raquira - 201914059                   #
# Santiago Becerra - 201911587                 #
################################################


### Primero revisamos los datos y los limpiamos ###
rm(list=ls())
require(pacman) 
p_load(tidyverse,rio,
       sf, 
       leaflet, 
       tmaptools, 
       osmdata,
       spdep,
       secr,
       osmdata,
       here) 
setwd("C:/Users/Santiago Becerra/Desktop/Santiago/Andes/Materias y Trabajos/Octavo Semestre/Big Data/Problem set 3/Taller_BigData_3/Stores")
train <- readRDS("train.Rds")
test <- readRDS("test.RDS")

train <- train %>%
  mutate_at(.vars = c(
    "property_type","operation_type", "city"),
    .funs = factor)

test <- test %>%
  mutate_at(.vars = c(
    "property_type","operation_type", "city"),
    .funs = factor)

## Datos espaciales ##
train<-train %>% mutate(latb=lat,longb=lon)
train<-st_as_sf(train,coords=c('longb','latb'),crs="WGS84") #crs utilizado por google earth
test<-test %>% mutate(latb=lat,longb=lon)
test<-st_as_sf(test,coords=c('longb','latb'),crs="WGS84")

##Revisamos la variable city que e sla que nos interesa ##
train %>%
  group_by(city) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) 

test %>%
  group_by(city) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) 

## Separamos de la base train Bogotá y Medellín ##
train_bog<-subset(train,city %in% c("Bogotá D.C"))
train_med<-subset(train,city %in% c("Medellín"))

################# Empezamos con Bogotá #####################
point = geocode_OSM(paste(train$lat[1]," , ",train$lon[1]), as.sf=T) 
leaflet() %>% addTiles() %>% addCircles(data=point)
leaflet() %>% addTiles() %>% addCircles(data=train_bog)

#### Supermercados ####
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm  %>% osmdata_sf()
osm_sf

marketplace = osm_sf$osm_points %>% select(osm_id,amenity) 
marketplace
### Distancias market place ##
dist_market = st_distance(x=train_bog , y=marketplace)
dist_market
min_dist_market = apply(dist_market , 1 , min)
min_dist_market
train_bog$min_dist_market<-min_dist_market

leaflet() %>% addTiles() %>% 
  addCircles(data=marketplace , col="black" , weight=2)%>% 
  addCircles(data=train_bog , col="black" , weight=2)

## Ahora con estaciones de bus ##

osm2 = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm2)

osm_sf2 = osm2 %>% osmdata_sf()
osm_sf2

estaciones_de_bus = osm_sf2$osm_points %>% select(osm_id,amenity) 
estaciones_de_bus

leaflet() %>% addTiles() %>% addCircleMarkers(data=estaciones_de_bus , col="red") %>% 
  addCircleMarkers(data=train_bog, col="blue")

### Distancias buses ###
distancias_bus = st_distance(x=train_bog , y=estaciones_de_bus)

min_distancias_bus = apply(distancias_bus , 1 , min)

train_bog$min_distancias_bus<-min_distancias_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=point , col="red" , weight=2) %>% 
  addCircles(data=estaciones_de_bus , col="black" , weight=2) 

################# Seguimos con Medellín #####################

#### Super mercados ####
osm3 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm3)

## extraer Simple Features Collection
osm_sf3 = osm3 %>% osmdata_sf()
osm_sf3

marketplace = osm_sf3$osm_points %>% select(osm_id,amenity) 
marketplace

leaflet() %>% addTiles() %>% addCircleMarkers(data=marketplace , col="blue")

### distanciaa supermercados ###
distancias_market = st_distance(x=train_med, y=marketplace)
distancias_market

min_distancias_market = apply(distancias_market , 1 , min)
min_distancias_market

train_med$min_distancias_market<-min_distancias_market

leaflet() %>% addTiles() %>% 
  addCircles(data=marketplace , col="black" , weight=2)  

### Ahora estaciones de buses ###

osm4 = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm4)

## extraer Simple Features Collection
osm_sf4 = osm4 %>% osmdata_sf()
osm_sf4

estaciones_de_busM = osm_sf4$osm_points %>% select(osm_id,amenity) 
estaciones_de_busM

leaflet() %>% addTiles() %>% addCircleMarkers(data=estaciones_de_busM , col="red") %>% 
  addCircleMarkers(data=train_med, col="blue")

#### distancia buses ####
distancia_bus = st_distance(x=train_med , y=estaciones_de_busM)

min_distancia_bus = apply(distancia_bus , 1 , min)

train_med$min_distancia_bus<-min_distancia_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=train_med , col="red" , weight=2) %>% 
  addCircles(data=estaciones_de_busM , col="black" , weight=2)

## Creación test conjunta

test_final<-rbind(test_bog,test_med) ### revision


##### Predicción en areas según vecinos cercanos
path = here('')
## Primero Bogota

mzbog<-st_read(here(path,"11_BOGOTA/URBANO/MGN_URB_MANZANA.shp"))
######## este es el que no me sale
