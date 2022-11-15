################################################
#                 Taller 2                     #
#               Modelos/Predicción             #
# Colaboradores                                #
# Daniel Raquira - 201914059                   #
# Santiago Becerra - 201911587                 #
################################################

rm(list=ls())
require(pacman) 

## Llama/instala-llama las librer?as listadas
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
##Librerias requeridas
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

train <- train %>%
  mutate_at(.vars = c(
    "property_type","operation_type","l3"),
    .funs = factor)

test <- test %>%
  mutate_at(.vars = c(
    "property_type","operation_type","l3"),
    .funs = factor)

## Para evitar problemas de correlación y ver como se comportan las variables

cor(train$bedrooms, train$new_surface)
cor(train$bedrooms, train$min_dist_bus)
cor(train$bedrooms, train$min_dist_market)
cor(train$bedrooms, train$min_dist_policias)
cor(train$bedrooms, train$min_dist_oficinas)
cor(train$bedrooms, train$min_dist_colegios)
cor(train$new_surface, train$min_dist_bus)
cor(train$new_surface, train$min_dist_market)
cor(train$new_surface, train$min_dist_policias)
cor(train$new_surface, train$min_dist_oficinas)
cor(train$new_surface, train$min_dist_colegios)
cor(train$min_dist_bus, train$min_dist_market)
cor(train$min_dist_bus, train$min_dist_policias)
cor(train$min_dist_bus, train$min_dist_oficinas)
cor(train$min_dist_bus, train$min_dist_colegios)
cor(train$min_dist_market, train$min_dist_policias)
cor(train$min_dist_market, train$min_dist_oficinas)
cor(train$min_dist_market, train$min_dist_colegios)
cor(train$min_dist_policias, train$min_dist_oficinas)
cor(train$min_dist_policias, train$min_dist_colegios)
cor(train$min_dist_oficinas, train$min_dist_colegios)

#### Arrancamos con un Modelo Tradicional ####

mod1 <- lm(price ~ bedrooms + new_surface + min_dist_bus + min_dist_market + min_dist_policias + min_dist_colegios + min_dist_oficinas + property_type + balcon_terr + l3, data = train)
summary(mod1)

model1 <- train(price ~ bedrooms + new_surface + min_dist_bus + min_dist_market + min_dist_policias + min_dist_colegios + min_dist_oficinas + property_type + balcon_terr + l3 ,
                data = train,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model1 
549026780^2
##MSE (anotar valor que nos de)

##Coeficientes modelo 1 tradicional
coeficientes_m1 <- mod1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

##Gráfica importancia coeficientes model 1 OLS
coeficientes_m1 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo tradicional") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

#### Predicciones de test ####
predicciones_test_ols <- predict(model1, newdata = test)
predicciones_ols<-as.data.frame(predicciones_test_ols)


########## Luego, Modelo 2 Ridge Personas #########

x_train <- model.matrix( ~ bedrooms + new_surface + min_dist_bus + min_dist_market + min_dist_policias + min_dist_colegios + min_dist_oficinas + property_type + balcon_terr + l3, data = train)[, -1]
y_train <- train$price

ridge_m2 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = T
)

regularizacion <- ridge_m2$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = ridge_m2$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes de regularización Ridge") +
  theme_bw() +
  theme(legend.position = "none")

cv_error_ridge <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = T
)

plot(cv_error_ridge)
paste("Mejor valor lambda:", cv_error_ridge$lambda.min)
paste("Mejor valor lambda  + 1 desviación estandar:", cv_error_ridge$lambda.1se)
cv_error_ridge

ridgem2_lambda <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error_ridge$lambda.min,
  standardize = TRUE
)

ridgem2_lambda


coeficientes_m2 <- coef(ridgem2_lambda) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

##Coeficientes modelo ridge
coeficientes_m2

##Gráfica importancia coeficientes ridge
coeficientes_m2 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coefs modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

##Predicciones en test
x.test <- model.matrix( ~ bedrooms + new_surface + min_dist_bus + min_dist_market + min_dist_policias + min_dist_colegios + min_dist_oficinas + property_type + balcon_terr + l3, test)[, -1]
predict_ridge <- predict(ridgem2_lambda, newx = x.test)
predict_ridge

#MSE del ridge: (valor que nos de )

##############el Modelo 3 es Lasso #########################

lasso_m3 <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- lasso_m3$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = lasso_m3$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coefs regularización lasso") +
  theme_bw() +
  theme(legend.position = "none")


cv_error_lasso <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

cv_error_lasso
plot(cv_error_lasso)
paste("Mejor valor de lambda:", cv_error_lasso$lambda.min)
paste("Mejor valor de lambda + 1 desviación estandar:", cv_error_lasso$lambda.1se)


lassom3_lambda <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_lasso$lambda.min,
  standardize = TRUE
)

lassom3_lambda

coeficientes_m3 <- coef(lassom3_lambda) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

#Coeficientes modelo lasso

coeficientes_m3

#Gráfica importancia coeficientes lasso
coeficientes_m3 %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coefs modelo lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# Predicciones en train
predict_lasso <- predict(model_lasso_min, newx = x.test)
predict_lasso
# MSE de entrenamiento: (valor mse)

######### Modelo 4 es de Elastic Net #############
EN_m4 <- train(price ~ bedrooms + new_surface + min_dist_bus + min_dist_market + min_dist_policias + min_dist_colegios + min_dist_oficinas + property_type + balcon_terr + l3, data = train, method = "glmnet",
            trControl = trainControl("cv", number = 10), preProcess = c("center", "scale"))

EN_m4 ## alpha = 0.55 and lambda = 900321. (colocafr los nuestros)

## RMSE= 548747488 (colocar los nuestros)
548747488^2
##MSE= (colocar mse)

# Predicción EN
predict_m4 <- predict(el, test)
predict_m4


########## Modelo 5 - Superlearners ###################
require(randomForest)
require("tidyverse")
require("ranger")
require("SuperLearner")
install.packages("VGAM", dependencies = FALSE)
require("VGAM")
# set the seed for reproducibility
set.seed(201914059)
SuperL_x <- data.frame(train$bedrooms, train$l3, train$new_surface, train$min_dist_bus, train$min_dist_market,train$min_dist_policias, train$min_dist_colegios, train$min_dist_oficinas, train$property_type, train$balcon_terr )
SuperL_x <- rename(SuperL_xXS, bedrooms =train.bedrooms)
SuperL_x <- rename(SuperL_x, l3 =train.l3)
SuperL_x <- rename(SuperL_x, new_surface =train.new_surface)
SuperL_x <- rename(SuperL_x, min_dist_bus =train.min_dist_bus)
SuperL_x <- rename(SuperL_x, min_dist_market =train.min_dist_market)
SuperL_x <- rename(SuperL_x, min_dist_policias =train.min_dist_policias)
SuperL_x <- rename(SuperL_x, min_dist_colegios =train.min_dist_colegios)
SuperL_x <- rename(SuperL_x, min_dist_oficinas =train.min_dist_oficinas)
SuperL_x <- rename(SuperL_x, property_type =train.property_type)
SuperL_x <- rename(SuperL_x, balcon_terr =train.balcon_terr)

SuperL_y <- train$price

str(SuperL_x)
folds = 5
index <- split(1:1000, 1:folds)
##Aquí hacemos un SuperLearnes por medio de OLS, RF y Glmnet
Y_fit <- SuperLearner(Y = SuperL_y, X = SuperL_x,
                     method = "method.NNLS", SL.library = c("SL.mean","SL.lm", "SL.ranger", "SL.glmnet"),
                     cvControl = list(V = folds))

Y_fit ## Nos dice que el mejor mode es ranger_All MSE: 1.612468e+17 --ver que nos de lo mismo

# Para toda x, predecir el outcome
SuperL_yy <- predict(Y_fit, newdata = data.frame(SuperL_x),onlySL = T)$pred
# Create a dataframe para toda x y las respuestas del SuperLearning
SL_responses <- data.frame(SuperL_x, SuperL_yy)


#RMSE de todos los modelos realizados en el train, esto nos dice que el de menor MSE
#es el Superlearner
#OLS MSE:3.014304e+17
#Ridge MSE 3.023e+17
#Lasso 3.014e+17
#Elastic Net MSE=3.011238e+17
#Superlearner MSE:1.612468e+17
#### Verificar nuestros datos
SuperL_x
SuperL_x_test <- data.frame(test$bedrooms, test$l3, test$new_surface, test$min_dist_bus, test$min_dist_market,test$min_dist_policias,test$min_dist_colegios,test$min_dist_oficinas, test$property_type, test$balcon_terr )
SuperL_x_test<-rename(SuperL_x_test, bedrooms =test.bedrooms)
SuperL_x_test<-rename(SuperL_x_test, l3 =test.l3)
SuperL_x_test<-rename(SuperL_x_test, new_surface =test.new_surface)
SuperL_x_test<-rename(SuperL_x_test, min_dist_bus =test.min_dist_bus)
SuperL_x_test<-rename(SuperL_x_test, min_dist_market =test.min_dist_market)
SuperL_x_test<-rename(SuperL_x_test, min_dist_policias =test.min_dist_policias)
SuperL_x_test<-rename(SuperL_x_test, min_dist_colegios =test.min_dist_colegios)
SuperL_x_test<-rename(SuperL_x_test, min_dist_oficinas =test.min_dist_oficinas)
SuperL_x_test<-rename(SuperL_x_test, property_type =test.property_type)
SuperL_x_test<-rename(SuperL_x_test, balcon_terr =test.balcon_terr)

str(SuperL_x_test)

#Estadisticas descriptivas del precio predicho
summary(SL_responses$predict_test)


################## Bigotes para views ########################
bwplot(predict_test ~ l3 , data = SL_responses)

#Submission file
submission<-data.frame(test$property_id,SL_responses$predict_test)
write.csv(submission,"dirección para nuevo archivo, tenemos uno en store", row.names = FALSE)

