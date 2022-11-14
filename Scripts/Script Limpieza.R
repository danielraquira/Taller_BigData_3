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
path = here('')
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

##Revisamos la variable city que es la que nos interesa ##
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

min_dist_bus = apply(distancias_bus , 1 , min)

train_bog$min_dist_bus<-min_dist_bus


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

### distancia supermercados ###
distancias_market = st_distance(x=train_med, y=marketplace)
distancias_market

min_dist_market = apply(distancias_market , 1 , min)
min_dist_market

train_med$min_dist_market<-min_dist_market

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

min_dist_bus = apply(distancia_bus , 1 , min)

train_med$min_dist_bus<-min_dist_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=train_med , col="red" , weight=2) %>% 
  addCircles(data=estaciones_de_busM , col="black" , weight=2)

train_final<-rbind(train_bog,train_med) 


## Hacemos lo mismo para Cali que es el test ##

point = geocode_OSM(paste(test$lat[1]," , ",test$lon[1]), as.sf=T) 
leaflet() %>% addTiles() %>% addCircles(data=point)
leaflet() %>% addTiles() %>% addCircles(data=test)

## objeto osm marketplace
osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

marketplace = osm_sf$osm_points %>% select(osm_id,amenity) 
marketplace

leaflet() %>% addTiles() %>% addCircleMarkers(data=marketplace , col="blue")

##distancia marketplace
dist_market = st_distance(x=test , y=marketplace)
dist_market
min_dist_market = apply(dist_market , 1 , min)
min_dist_market
test$min_dist_market<-min_dist_market

leaflet() %>% addTiles() %>% 
  addCircles(data=marketplace , col="black" , weight=2)


## objeto osm buses
osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

estaciones_busC = osm_sf$osm_points %>% select(osm_id,amenity) 
estaciones_busC

leaflet() %>% addTiles() %>% addCircleMarkers(data=estaciones_busC , col="red") %>% 
  addCircleMarkers(data=test, col="blue")


## distancia buses
dist_bus = st_distance(x=test , y=estaciones_busC)

min_dist_bus = apply(dist_bus , 1 , min)

test$min_dist_bus<-min_dist_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=point , col="red" , weight=2) %>% 
  addCircles(data=estaciones_busC , col="black" , weight=2) 


## Hacemos predicciones para Bogotá ##

#Predicción a partir de descripción
table(is.na(train_bog$surface_total))
table(is.na(train_bog$surface_covered))

train_bog = train_bog %>% mutate(new_surface=surface_total)
train_bog = train_bog %>% mutate(new_surface=ifelse(is.na(new_surface)==T,surface_covered,new_surface))

table(is.na(train_bog$new_surface))

train_bog = train_bog %>% mutate(new_desc=str_to_lower(description))

p1 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m" ## pattern
p2 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m2" ## pattern

train_bog$new_surface2<-NA

train_bog = train_bog %>% 
  mutate(new_surface2 = str_extract(string=train_bog$new_desc , pattern= p1))
train_bog = train_bog %>% 
  mutate(new_surface2 = ifelse(is.na(new_surface2)==T,
                               str_extract(string=train_bog$new_desc , pattern= p2),
                               new_surface2))
train_bog$new_surface3<-NA
train_bog = train_bog %>% 
  mutate(new_surface3 = str_extract(string=train_bog$new_surface2 , pattern= "[:digit:]{2,3}"))
train_bog$new_surface3 <- as.numeric(train_bog$new_surface3)
train_bog = train_bog %>% mutate(new_surface3 = ifelse(new_surface3<=40,NA,new_surface3))

train_bog = train_bog %>% mutate(new_surface = ifelse(is.na(new_surface)==T,new_surface3,new_surface))
table(is.na(train_bog$new_surface))

# buffer
house_buf<-NULL
house_buf_mean<-NULL
house_buf = st_buffer(train_bog,dist=100)
house_buf = st_join(house_buf,train_bog[,"new_surface"])
st_geometry(house_buf) = NULL
house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(new_surface.y,na.rm=T))
train_bog = left_join(train_bog,house_buf_mean,"property_id")

train_bog = train_bog %>% mutate(new_surface = ifelse(is.na(new_surface)==T,train_bog$surface_new_3,new_surface))
table(is.na(train_bog$new_surface))

train_bog = train_bog %>% mutate(new_surface = ifelse(is.na(new_surface)==T,mean(train_bog[is.na(train_bog$new_surface)==F,]$new_surface),new_surface))
table(is.na(train_bog$new_surface))
table(is.na(train_bog$property_type))


##Prediccion si tiene balcon / terraza
p1 = "balcon" ## pattern
p2 = "balcón" ## pattern
p3 = "terraza" ## pattern

train_bog$balcon<-NULL

train_bog = train_bog %>% 
  mutate(balcon = str_extract(string=train_bog$new_desc , pattern= p1))
train_bog = train_bog %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_bog$new_desc , pattern= p2),balcon))
train_bog = train_bog %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_bog$new_desc , pattern= p3),balcon))

train_bog = train_bog %>% mutate(balcon_terr = ifelse(is.na(balcon)==T, 0 ,1))


train_bog <- train_bog %>%
  mutate_at(.vars = c(
    "balcon_terr"),
    .funs = factor)

## Hacemos predicciones para Medellín ##

#Predicción a partir de descripción
table(is.na(train_med$surface_total))
table(is.na(train_med$surface_covered))
train_med$new_surface<-NULL
train_med = train_med %>% mutate(new_surface=surface_total)
table(is.na(train_med$new_surface))
train_med = train_med %>% mutate(new_surface=ifelse(is.na(new_surface)==T,surface_covered,new_surface))
table(is.na(train_med$new_surface))


train_med = train_med %>% mutate(new_desc=str_to_lower(description))


p1 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m" 
p2 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m2"

train_med$new_surface2<-NA

train_med = train_med %>% 
  mutate(new_surface2 = str_extract(string=train_med$new_desc , pattern= p1))
train_med = train_med %>% 
  mutate(new_surface2 = ifelse(is.na(new_surface2)==T,
                               str_extract(string=train_med$new_desc , pattern= p2),
                               new_surface2))
train_med$new_surface3<-NA

train_med = train_med %>% 
  mutate(new_surface3 = str_extract(string=train_med$new_surface2 , pattern= "[:digit:]{2,3}"))
train_med$new_surface3 <- as.numeric(train_med$new_surface3)
train_med = train_med %>% mutate(new_surface3 = ifelse(new_surface3<=40,NA,new_surface3))

train_med = train_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,new_surface3,new_surface))
table(is.na(train_med$new_surface))


# buffer
house_buf<-NULL
house_buf_mean<-NULL
house_buf = st_buffer(train_med,dist=100)
house_buf = st_join(house_buf,train_med[,"new_surface"])
st_geometry(house_buf) = NULL
house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(new_surface.y,na.rm=T))
train_med = left_join(train_med,house_buf_mean,"property_id")

train_med = train_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,train_med$surface_new_3,new_surface))
table(is.na(train_med$new_surface))

train_med = train_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,mean(train_med[is.na(train_med$new_surface)==F,]$new_surface),new_surface))
table(is.na(train_med$new_surface))
table(is.na(train_med$property_type))


##Prediccion si tiene balcon / terraza
p1 = "balcon" ## pattern
p2 = "balcón" ## pattern
p3 = "terraza" ## pattern

train_med$balcon<-NULL

train_med = train_med %>% 
  mutate(balcon = str_extract(string=train_med$new_desc , pattern= p1))
train_med = train_med %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_med$new_desc , pattern= p2),balcon))
train_med = train_med %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_med$new_desc , pattern= p3),balcon))

train_med = train_med %>% mutate(balcon_terr = ifelse(is.na(balcon)==T, 0 ,1))


train_med <- train_med %>%
  mutate_at(.vars = c(
    "balcon_terr"),
    .funs = factor)

## Predicción para Cali / test ##

#A partir de descripción
table(is.na(test$surface_total))
table(is.na(test$surface_covered))
test$new_surface<-NULL
test = test %>% mutate(new_surface=surface_total)
table(is.na(test$new_surface))
test = test %>% mutate(new_surface=ifelse(is.na(new_surface)==T,surface_covered,new_surface))
table(is.na(test$new_surface))


test = test %>% mutate(new_desc=str_to_lower(description))


p1 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m" 
p2 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m2" 

test$new_surface2<-NA

test = test %>% 
  mutate(new_surface2 = str_extract(string=test$new_desc , pattern= p1))
test = test %>% 
  mutate(new_surface2 = ifelse(is.na(new_surface2)==T,
                               str_extract(string=test$new_desc , pattern= p2),
                               new_surface2))
test$new_surface3<-NA

test = test %>% 
  mutate(new_surface3 = str_extract(string=test$new_surface2 , pattern= "[:digit:]{2,3}"))
test$new_surface3 <- as.numeric(test$new_surface3)
test = test %>% mutate(new_surface3 = ifelse(new_surface3<=40,NA,new_surface3))

test = test %>% mutate(new_surface = ifelse(is.na(new_surface)==T,new_surface3,new_surface))
table(is.na(test$new_surface))


## make buffer
house_buf<-NULL
house_buf_mean<-NULL
house_buf = st_buffer(test,dist=100)
house_buf = st_join(house_buf,test[,"new_surface"])
st_geometry(house_buf) = NULL
house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(new_surface.y,na.rm=T))
test = left_join(test,house_buf_mean,"property_id")

test = test %>% mutate(new_surface = ifelse(is.na(new_surface)==T,test$surface_new_3,new_surface))
table(is.na(test$new_surface))

test = test %>% mutate(new_surface = ifelse(is.na(new_surface)==T,mean(test[is.na(test$new_surface)==F,]$new_surface),new_surface))
table(is.na(test$new_surface))
table(is.na(test$property_type))


##Prediccion si tiene balcon / terraza
p1 = "balcon" ## pattern
p2 = "balcón" ## pattern
p3 = "terraza" ## pattern

test$balcon<-NULL

test = test %>% 
  mutate(balcon = str_extract(string=test$new_desc , pattern= p1))
test = test %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=test$new_desc , pattern= p2),balcon))
test = test %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=test$new_desc , pattern= p3),balcon))

test = test %>% mutate(balcon_terr = ifelse(is.na(balcon)==T, 0 ,1))

test <- test %>%
  mutate_at(.vars = c(
    "balcon_terr"),
    .funs = factor)

## Revisamos NA´s ##
table(is.na(test$new_surface))
table(is.na(test$bedrooms))
table(is.na(test$min_dist_bus))
table(is.na(test$min_dist_market))
table(is.na(test$property_type))
table(is.na(train_bog$new_surface))
table(is.na(train_bog$bedrooms))
table(is.na(train_bog$min_dist_bus))
table(is.na(train_bog$min_dist_market))
table(is.na(train_bog$property_type))
table(is.na(train_bog$price))
table(is.na(train_med$new_surface))
table(is.na(train_med$bedrooms))
table(is.na(train_med$min_dist_bus))
table(is.na(train_med$min_dist_market))
table(is.na(train_med$property_type))
table(is.na(train_med$price))

train_final <- rbind(train_bog, train_med)

save(train_bog, train_med, train_final, test, file = "datos_limpios.RData")
