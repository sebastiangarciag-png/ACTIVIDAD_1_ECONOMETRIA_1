####################################
######                        ######
#     UNIVERSIDAD DEL QUINDIO      #
#      PROGRAMA DE ECONOMIA        #
#         ECONOMETRIA 1            #
######                        ######
####################################

# BY: SEBASTIAN GARCIA GARCIA 
# sebastian.garciag@uqvirtual.edu.co
# +57 3019728640

Fuente de los datos: [GEIH 2025 - DANE](https://microdatos.dane.gov.co/index.php/catalog/853)

getwd()
options("scipen" = 100 , "digits" = 4)
rm(list = ls())

# paquetes o librerias ___
library("skimr")
library("readxl")
library("stringi")
library("haven")
library("tidyverse")
library("plyr")
library("rstatix")
library("descr")
library("splitstackshape")
library("e1071")
library("dplyr")

## cargamos la base de datos ##
DATA_INGRESOS = read.csv("DATA_INGRESOS.csv")

## descriptivo
head(DATA_INGRESOS)
str(DATA_INGRESOS , list.len = 492)
skimr::skim(DATA_INGRESOS)

# Ver los nombres de las columnas
names(DATA_INGRESOS)

# Ver la estructura (tipos de datos)
str(DATA_INGRESOS)

# Ver las primeras filas en la consola
head(DATA_INGRESOS)

## Analisis descriptivo: Indicadores de posición y centro -----
DATA_INGRESOS |> dplyr::group_by(1) |> mutate( INGLABO = replace_na(INGLABO , replace = 0) ) |> dplyr::summarise( 
  median_INGLABO =  median(INGLABO) , 
  mean_INGLABO = mean(INGLABO) ,
  rango_medio_INGLABO = ((max(INGLABO) - min(INGLABO))/2 ) , 
  min_INGLABO = min(INGLABO) ,
  Q1_INGLABO = quantile(INGLABO , c(0.25) , na.rm = TRUE ) ,
  Q3_INGLABO = quantile(INGLABO , c(0.75) , na.rm = TRUE ),
  rango_q_INGLABO = IQR(INGLABO) ,
  max_INGLABO = max(INGLABO))


### Indicadores de dispersión
DATA_INGRESOS |> dplyr::group_by(1) |> mutate( INGLABO = replace_na(INGLABO , replace = 0) ) |> dplyr::summarise(
  rango_INGLABO = (max(INGLABO) - min(INGLABO)),
  sd_INGLABO = sd(INGLABO) ,
  varianza_INGLABO = var(INGLABO) ,
  c_variacion_INGLABO = (sd(INGLABO) / mean(INGLABO) *100) ,
  c_curtosis_INGLABO = kurtosis(INGLABO) ,
  c_asimetria_INGLABO = skewness(INGLABO))


################################################################################
########               UNIVERSIDAD DEL QUINDIO                          ########
########                PROGRAMA DE ECONOMIA                            ########
########                     ECONOMETRIA 1                              ########
########                      ACTIVIDAD 1                               ########
################################################################################

# BY: SEBASTIAN GARCIA GARCIA 
# sebastian.garciag@uqvirtual.edu.co
# +57 3019728640

#FILTRAMOS LA BASE DE DATOS ANTERIORMENTE CARGADA
DATA_INGRESOS |> 
  filter(!is.na(P6426)) |> 

## Analisis descriptivo: Indicadores de posición y centro (P6426) -----
DATA_INGRESOS |> 
  filter(!is.na(P6426)) |> 
  summarise( 
    median_P6426 = median(P6426), 
    mean_P6426 = mean(P6426),
    rango_medio_P6426 = ((max(P6426) - min(P6426))/2), 
    min_P6426 = min(P6426),
    Q1_P6426 = quantile(P6426, c(0.25), na.rm = TRUE),
    Q3_P6426 = quantile(P6426, c(0.75), na.rm = TRUE),
    rango_q_P6426 = IQR(P6426),
    max_P6426 = max(P6426)
  )

### Indicadores de dispersión (P6426)
DATA_INGRESOS |> 
  filter(!is.na(P6426)) |> 
  summarise(
    rango_P6426 = (max(P6426) - min(P6426)),
    sd_P6426 = sd(P6426),
    varianza_P6426 = var(P6426),
    c_variacion_P6426 = (sd(P6426) / mean(P6426) * 100),
    c_curtosis_P6426 = kurtosis(P6426),
    c_asimetria_P6426 = skewness(P6426)
  )

### Deciles (P6426)
DATA_INGRESOS |> 
  filter(!is.na(P6426)) |> 
  summarise(
    deciles = list(quantile(P6426, probs = seq(0.1, 0.9, 0.1)))
  ) |> 
  pull(deciles)


## Número de personas por cada mes de antigüedad (P6426) -----
tabla_meses <- DATA_INGRESOS |> 
  dplyr::filter(!is.na(P6426)) |> 
  dplyr::count(P6426) |> 
  dplyr::arrange(P6426)

tabla_meses


