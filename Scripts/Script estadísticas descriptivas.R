################################################
#                 Taller 2                     #
#               Descriptivas                   #
# Colaboradores                                #
# Daniel Raquira - 201914059                   #
# Santiago Becerra - 201911587                 #
################################################

rm(list=ls())
require("pacman")
p_load("here")
p_load("readr")
p_load(ggplot2) 
p_load(scales) 
p_load(ggpubr) 
p_load(rio)  
p_load(tidyverse) 
p_load(e1071) 
p_load(EnvStats) 
p_load(tidymodels) 
p_load(ggplot2) 
p_load(scales) 
p_load(ggpubr) 
p_load(knitr) 
p_load(kableExtra) 
p_load(dplyr)
p_load(caret)
p_load(glmnet)
p_load(pls)
p_load(tidyr)
p_load(tibble)
p_load(gtsummary)
p_load(skimr)

p_load(tidyverse,rio,
       sf, 
       leaflet, 
       tmaptools, 
       osmdata,
       spdep,
       secr,
       osmdata,
       here) 

load("datos_limpios.RData")

  
train<- as.data.frame(train_final)
levels(train$balcon_terr) <- c("Sin Balcón o Terraza", "Con Balcón o Terraza")
summary <- train %>% select(price,city,bedrooms,new_surface,property_type,min_dist_bus,min_dist_market,min_dist_colegios, min_dist_oficinas,min_dist_policias,balcon_terr)
summary %>% tbl_summary(by=city,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                               all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = list(city ~ "Ciudad"
                        )
                        
)

test<- as.data.frame(test)
levels(test$balcon_terr) <- c("Sin Balcón o Terraza", "Con Balcón o Terraza")
summary <- test %>% select(city,bedrooms,new_surface,property_type,min_dist_bus,min_dist_market,min_dist_colegios, min_dist_oficinas,min_dist_policias,balcon_terr)
summary %>% tbl_summary(by=city,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                               all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = list(city ~ "Ciudad")
                        
)


