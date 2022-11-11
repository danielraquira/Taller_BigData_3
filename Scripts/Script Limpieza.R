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

## Empezamos con Bogotá ##
point = geocode_OSM(paste(train$lat[1]," , ",train$lon[1]), as.sf=T) 
leaflet() %>% addTiles() %>% addCircles(data=point)
leaflet() %>% addTiles() %>% addCircles(data=train_bog)

##Supermercados##
osm = opq(bbox = getbb("Bogot? Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

marketplace = osm_sf$osm_points %>% select(osm_id,amenity) 
marketplace

leaflet() %>% addTiles() %>% addCircleMarkers(data=marketplace , col="blue")
