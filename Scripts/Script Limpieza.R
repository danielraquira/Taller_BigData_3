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
    "property_type","operation_type"),
    .funs = factor)

test <- test %>%
  mutate_at(.vars = c(
    "property_type","operation_type"),
    .funs = factor)

## Datos espaciales ##
train<-train %>% mutate(latb=lat,longb=lon)
train<-st_as_sf(train,coords=c('longb','latb'),crs="WGS84") #crs utilizado por google earth
test<-test %>% mutate(latb=lat,longb=lon)
test<-st_as_sf(test,coords=c('longb','latb'),crs="WGS84")


