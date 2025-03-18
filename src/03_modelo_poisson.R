# 03_modelo_poisson.R
# Script para el desarrollo del modelo de Poisson

# Calcular parámetros necesarios para las lambdas
calcular_parametros_lambda <- function(LOCAL1, VISITANTE1, temp1, GMC, GRC, GMF, GRF) {
  dimT <- dim(temp1)
  equip <- rep(0, dimT[1])
  
  for (t in 1:dimT[1]) {
    equip[t] <- temp1$Equipo[t]
  }
  
  equipos <- sort(equip, decreasing = FALSE)
  
  # Calcular capacidades ofensivas y defensivas
  CAC <- rep(0, dimT[1])  # Capacidad Atacante en Casa
  CDC <- rep(0, dimT[1])  # Capacidad Defensiva en Casa
  CAF <- rep(0, dimT[1])  # Capacidad Atacante Fuera
  CDF <- rep(0, dimT[1])  # Capacidad Defensiva Fuera
  
  for (k in 1:dimT[1]) {
    CAC[k] <- LOCAL1$GMC[k] / GMC 
    CDC[k] <- LOCAL1$GRC[k] / GRC
    CAF[k] <- VISITANTE1$GMF[k] / GMF
    CDF[k] <- VISITANTE1$GRF[k] / GRF
  }
  
  PRElandas <- data.frame(equipos = temp1$Equipo, CAC, CDC, CAF, CDF)
  
  return(list(
    equipos = equipos,
    PRElandas = PRElandas
  ))
}

# Filtrar equipos para la temporada de estudio
filtrar_equipos_estudio <- function(temp, tmp, PRElandas) {
  equipos_estudio <- sort(unique(temp[temp$Season == tmp,]$Equipo), decreasing = FALSE)
  PRElandasF <- PRElandas[PRElandas$equipos %in% equipos_estudio,]
  rownames(PRElandasF) <- 1:20
  
  return(list(
    equipos_estudio = equipos_estudio,
    PRElandasF = PRElandasF
  ))
}

# Funciones para calcular las lambdas de cada partido
landaL <- function(l, v, PRElandasF, GMC) {
  landaL <- PRElandasF[PRElandasF$equipos == l, 2] * 
            PRElandasF[PRElandasF$equipos == v, 5] * GMC
  return(landaL)
}

landaV <- function(v, l, PRElandasF, GMF) {
  landaV <- PRElandasF[PRElandasF$equipos == v, 4] * 
            PRElandasF[PRElandasF$equipos == l, 3] * GMF
  return(landaV)
}

# Calcular matrices de lambdas para todos los enfrentamientos
calcular_matrices_lambda <- function(equipos_estudio, PRElandasF, GMC, GMF) {
  n_equipos <- length(equipos_estudio)
  landasL <- matrix(1, nrow = n_equipos, ncol = n_equipos)
  landasV <- matrix(1, nrow = n_equipos, ncol = n_equipos)
  
  colnames(landasL) <- equipos_estudio
  rownames(landasL) <- equipos_estudio
  colnames(landasV) <- equipos_estudio
  rownames(landasV) <- equipos_estudio
  
  # Calcular lambda para cada combinación equipo local-visitante
  for (j in 1:n_equipos) {
    for (i in 1:n_equipos) {
      if (i == j) {
        landasL[i,j] <- 0
        landasV[i,j] <- 0
      } else {
        landasL[i,j] <- landaL(equipos_estudio[i], equipos_estudio[j], PRElandasF, GMC)
        landasV[i,j] <- landaV(equipos_estudio[j], equipos_estudio[i], PRElandasF, GMF)
      }                                          
    }
  }
  
  return(list(
    landasL = landasL,
    landasV = landasV
  ))
}

# Función para simular un partido
match <- function(r1, r2) {
  gL <- rpois(1, r1)
  gV <- rpois(1, r2)
  result <- c(gL, gV)
  return(result)
}

# Calcular parámetros para temporada actual (para predicción)
calcular_parametros_actuales <- function(temp_actual, GMC, GRC, GMF, GRF, PRElandas) {
  actual <- rbind(
    temp_actual %>% select(Season, Equipo=home, opp=visitor, GF=hgoal, GC=vgoal),
    temp_actual %>% select(Season, Equipo=visitor, opp=home, GF=vgoal, GC=hgoal)
  )
  
  actual$GF <- as.numeric(actual$GF)
  actual$GC <- as.numeric(actual$GC)
  actual <- actual %>% mutate(dG = GF - GC)
  
  # Datos de local
  actualL <- rbind(
    temp_actual %>% select(Season, Equipo=home, opp=visitor, GF=hgoal, GC=vgoal)
  )
  actualL <- actualL %>% mutate(dG = GF - GC)
  
  actualLOCAL1 <- actualL %>%
    group_by(Equipo) %>% 
    summarize(
      PJ = n(), 
      GF = sum(GF),
      GC = sum(GC),
      DG = sum(dG), 
      PG = sum(dG > 0),
      PE = sum(dG == 0), 
      PP = sum(dG < 0)
    ) %>% 
    mutate(Puntos = (PG*3) + PE) %>%
    mutate(GMC = GF/PJ) %>% 
    mutate(GRC = GC/PJ)
  
  # Datos de visitante
  actualV <- rbind(
    temp_actual %>% select(Season, Equipo=visitor, opp=home, GF=vgoal, GC=hgoal)
  )
  actualV <- actualV %>% mutate(dG = GF - GC)
  
  actualVISITANTE1 <- actualV %>% 
    group_by(Equipo) %>% 
    summarize(
      PJ = n(), 
      GF = sum(GF),
      GC = sum(GC),
      DG = sum(dG), 
      PG = sum(dG > 0),
      PE = sum(dG == 0), 
      PP = sum(dG < 0)
    ) %>% 
    mutate(Puntos = (PG*3) + PE) %>%
    mutate(GMF = GF/PJ) %>% 
    mutate(GRF = GC/PJ)
  
  # Estadísticas generales
  actual1 <- actual %>% 
    group_by(Equipo) %>% 
    summarize(
      PJ = n(), 
      GF = sum(GF),
      GC = sum(GC),
      DG = sum(dG), 
      PG = sum(dG > 0),
      PE = sum(dG == 0), 
      PP = sum(dG < 0)
    ) %>% 
    mutate(Puntos = (PG*3) + PE) %>%
    mutate(GM = GF/PJ) %>% 
    mutate(GR = GC/PJ)
  
  # Obtener equipos actuales
  dimT_actual <- dim(actual1)
  equip_actual <- rep(0, dimT_actual[1])
  
  for (t in 1:dimT_actual[1]) {
    equip_actual[t] <- actual1$Equipo[t]
  }
  
  equipos_actual <- sort(equip_actual, decreasing = FALSE)
  
  # Calcular capacidades actuales
  CAC_actual <- actualLOCAL1[,10] / GMC
  CDC_actual <- actualLOCAL1[,11] / GRC
  CAF_actual <- actualVISITANTE1[,10] / GMF
  CDF_actual <- actualVISITANTE1[,11] / GRF
  
  PRElandas_actual1 <- data.frame(
    equipos = equipos_actual,
    CAC = CAC_actual,
    CDC = CDC_actual,
    CAF = CAF_actual,
    CDF = CDF_actual
  )
  colnames(PRElandas_actual1) <- colnames(PRElandas)
  
  # Obtener parámetros de la temporada anterior para los equipos actuales
  PRElandas_anterior <- PRElandas[PRElandas$equipos %in% equipos_actual,]
  rownames(PRElandas_anterior) <- 1:20
  
  # Ponderación entre temporada actual y anterior
  peso_actual <- 0.4
  peso_anterior <- 1 - peso_actual
  calculos_landas_pond <- peso_actual * PRElandas_actual1[,-1] + 
                          peso_anterior * PRElandas_anterior[,-1]
  
  PRElandas_actual <- cbind(equipos_actual, calculos_landas_pond)
  
  return(list(
    actual = actual,
    actualLOCAL1 = actualLOCAL1,
    actualVISITANTE1 = actualVISITANTE1,
    actual1 = actual1,
    equipos_actual = equipos_actual,
    PRElandas_actual = PRElandas_actual
  ))
}