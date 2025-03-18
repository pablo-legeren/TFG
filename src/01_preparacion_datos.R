# 01_preparacion_datos.R
# Script para la preparación y carga inicial de datos

# Cargar librerías necesarias
library(dplyr)
library(engsoccerdata)
library(readxl)
library(vcd)
library(tidyverse)
library(ggplot2)

# Preparar los datasets para unirlos
preparar_datos <- function() {
  # Cargar datos históricos de España
  predata <- spain[,-9]
  rownames_to_column(predata, var = 'index')
  
  # Cargar datos de las últimas dos temporadas
  prenueva_data <- read_xlsx("data/ultimas2temporadas.xlsx", sheet = 'Hoja4')
  prenueva_data$Date <- as.Date(prenueva_data$Date)
  prenueva_data$HT <- replace(prenueva_data$HT, is.na(prenueva_data$HT), NA)
  prenueva_data$group <- replace(prenueva_data$group, is.na(prenueva_data$group), NA)
  prenueva_data$notes <- replace(prenueva_data$notes, is.na(prenueva_data$notes), NA)
  
  # Unir los dataframes
  data1 <- predata
  nueva_data <- prenueva_data
  data2 <- rbind(data1, nueva_data)
  data <- data2[,-c(5,9,10,11)]
  
  return(data)
}

# Extraer datos para análisis histórico
extraer_datos_historicos <- function(data, tmp = 2022) {
  datos <- data[(data$Season > tmp-9) & (data$Season < 2023),]
  
  # Reorganizar datos para análisis
  temp <- rbind(
    datos %>% select(Season, Equipo=home, opp=visitor, GF=hgoal, GC=vgoal),
    datos %>% select(Season, Equipo=visitor, opp=home, GF=vgoal, GC=hgoal)
  )
  
  # Convertir a numérico y calcular diferencia de goles
  temp$GF <- as.numeric(temp$GF)
  temp$GC <- as.numeric(temp$GC)
  temp <- temp %>% mutate(dG = GF - GC)
  
  return(list(datos = datos, temp = temp))
}

# Extraer datos como local y visitante
extraer_datos_local_visitante <- function(datos) {
  # Datos como local
  tempL <- rbind(datos %>% select(Season, Equipo=home, opp=visitor, GF=hgoal, GC=vgoal))
  tempL <- tempL %>% mutate(dG = GF - GC)
  
  # Datos como visitante
  tempV <- rbind(datos %>% select(Season, Equipo=visitor, opp=home, GF=vgoal, GC=hgoal))
  tempV <- tempV %>% mutate(dG = GF - GC)
  
  return(list(tempL = tempL, tempV = tempV))
}

# Extraer datos actuales para predicción
extraer_datos_prediccion <- function(data, dia_estudio = as.Date("2024/01/22")) {
  temp_actual <- data[(data$Season == 2023 & data$Date < dia_estudio),]
  temp_pred <- data[(data$Season == 2023 & data$Date > dia_estudio),]
  
  return(list(temp_actual = temp_actual, temp_pred = temp_pred))
}

# Función principal para cargar y preparar todos los datos
cargar_todos_datos <- function() {
  # Cargar datos base
  data <- preparar_datos()
  
  # Preparar datos históricos
  datos_hist <- extraer_datos_historicos(data)
  datos <- datos_hist$datos
  temp <- datos_hist$temp
  
  # Separar datos por local y visitante
  datos_local_visitante <- extraer_datos_local_visitante(datos)
  tempL <- datos_local_visitante$tempL
  tempV <- datos_local_visitante$tempV
  
  # Datos para predicción
  datos_pred <- extraer_datos_prediccion(data)
  temp_actual <- datos_pred$temp_actual
  temp_pred <- datos_pred$temp_pred
  
  return(list(
    data = data,
    datos = datos,
    temp = temp,
    tempL = tempL,
    tempV = tempV,
    temp_actual = temp_actual,
    temp_pred = temp_pred
  ))
}