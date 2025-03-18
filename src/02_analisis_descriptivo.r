# 02_analisis_descriptivo.R
# Script para análisis descriptivo y verificación de distribución Poisson

# Calcular estadísticas por equipo
calcular_estadisticas <- function(temp, tempL, tempV) {
  # Estadísticas generales
  temp1 <- temp %>% 
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
  
  # Estadísticas como local
  LOCAL1 <- tempL %>%
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
  
  # Estadísticas como visitante
  VISITANTE1 <- tempV %>% 
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
  
  # Promedios globales
  GMC <- colMeans(LOCAL1[-1])[9]  # Media de goles marcados como local
  GRC <- colMeans(LOCAL1[-1])[10] # Media de goles recibidos como local
  GMF <- colMeans(VISITANTE1[-1])[9] # Media de goles marcados como visitante
  GRF <- colMeans(VISITANTE1[-1])[10] # Media de goles recibidos como visitante
  
  return(list(
    temp1 = temp1,
    LOCAL1 = LOCAL1,
    VISITANTE1 = VISITANTE1,
    medias = c(GMC = GMC, GRC = GRC, GMF = GMF, GRF = GRF)
  ))
}

# Análisis de distribución Poisson para todos los partidos
analizar_poisson_total <- function(temp) {
  lambdaT <- mean(temp$GF)
  fptemp <- rpois(length(temp$GF), lambda = lambdaT)
  
  # Preparar gráficos
  par(mfrow = c(1,2))
  
  # Distribución teórica
  barplot(table(fptemp), col = "blue", ylab = "Frecuencia", 
          xlab = "Goles durante la temporada", main = "Distribución teórica", ylim = c(0,2700))
  grid(nx = NA, ny = NULL, lty = 1, col = "gray", lwd = 1)
  
  # Distribución real
  barplot(table(temp$GF), col = "red", ylab = "Frecuencia",
          xlab = "Goles durante la temporada", main = "Distribución real", ylim = c(0,2700))
  grid(nx = NA, ny = NULL, lty = 1, col = "gray", lwd = 1)
  
  # Test Chi-cuadrado
  a <- rep(0, max(temp$GF))
  for (i in (1:max(temp$GF+1))) {
    a[i] <- sum(temp$GF == i-1)
    i <- i+1
  }
  
  frec_esperada <- dpois(0:max(temp$GF), lambdaT)
  frecuencia_esperada <- frec_esperada * length(temp$GF)
  
  w <- rbind(frecuencia_esperada, a)
  resultado_test <- chisq.test(w, simulate.p.value = TRUE)
  
  return(list(
    lambda = lambdaT,
    frecuencia_esperada = frecuencia_esperada,
    frecuencia_observada = a,
    test_chisq = resultado_test
  ))
}

# Análisis de distribución Poisson para partidos de local
analizar_poisson_local <- function(tempL) {
  lambdaL <- mean(tempL$GF)
  fptempL <- rpois(length(tempL$GF), lambda = lambdaL)
  
  # Preparar gráficos
  par(mfrow = c(1,2))
  
  # Distribución teórica
  barplot(table(fptempL), col = "blue", ylab = "Frecuencia", 
          xlab = "Goles en casa", main = "Distribución teórica", ylim = c(0,1250))
  grid(nx = NA, ny = NULL, lty = 1, col = "gray", lwd = 1)
  
  # Distribución real
  barplot(table(tempL$GF), col = "red", ylab = "Frecuencia",
          xlab = "Goles en casa", main = "Distribución real", ylim = c(0,1250))
  grid(nx = NA, ny = NULL, lty = 1, col = "gray", lwd = 1)
  
  return(lambdaL)
}

# Análisis de distribución Poisson para partidos de visitante
analizar_poisson_visitante <- function(tempV) {
  lambdaV <- mean(tempV$GF)
  fptempV <- rpois(length(tempV$GF), lambda = lambdaV)
  
  # Preparar gráficos
  par(mfrow = c(1,2))
  
  # Distribución teórica
  barplot(table(fptempV), col = "blue", ylab = "Frecuencia", 
          xlab = "Goles fuera de casa", main = "Distribución teórica", ylim = c(0,1250))
  grid(nx = NA, ny = NULL, lty = 1, col = "gray", lwd = 1)
  
  # Distribución real
  barplot(table(tempV$GF), col = "red", ylab = "Frecuencia",
          xlab = "Goles fuera de casa", main = "Distribución real", ylim = c(0,1250))
  grid(nx = NA, ny = NULL, lty = 1, col = "gray", lwd = 1)
  
  return(lambdaV)
}