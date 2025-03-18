# 05_prediccion.R
# Script para predicción de partidos futuros y simulación del resto de la temporada

# Cargar bibliotecas necesarias
library(dplyr)
library(ggplot2)

# Función para calcular lambdas para equipos actuales
calcular_lambdas_actuales <- function(equipos_actuales, datos_historicos, datos_actuales, 
                                      peso_temporada_actual = 0.6) {
  # Calcular estadísticas para temporadas históricas
  LOCAL_hist <- datos_historicos %>%
    filter(Equipo %in% equipos_actuales) %>%
    group_by(Equipo) %>%
    summarize(
      PJ = n(),
      GF = sum(GF),
      GC = sum(GC),
      GMC = GF / PJ,
      GRC = GC / PJ
    )
  
  VISITANTE_hist <- datos_historicos %>%
    filter(opp %in% equipos_actuales) %>%
    group_by(opp) %>%
    summarize(
      PJ = n(),
      GF = sum(GF),
      GC = sum(GC),
      GMF = GF / PJ,
      GRF = GC / PJ
    )
  colnames(VISITANTE_hist)[1] <- "Equipo"
  
  # Calcular estadísticas para temporada actual
  LOCAL_actual <- datos_actuales %>%
    filter(Equipo %in% equipos_actuales) %>%
    group_by(Equipo) %>%
    summarize(
      PJ = n(),
      GF = sum(GF),
      GC = sum(GC),
      GMC = GF / PJ,
      GRC = GC / PJ
    )
  
  VISITANTE_actual <- datos_actuales %>%
    filter(opp %in% equipos_actuales) %>%
    group_by(opp) %>%
    summarize(
      PJ = n(),
      GF = sum(GF),
      GC = sum(GC),
      GMF = GF / PJ,
      GRF = GC / PJ
    )
  colnames(VISITANTE_actual)[1] <- "Equipo"
  
  # Calcular medias generales
  GMC_hist <- mean(LOCAL_hist$GMC)
  GRC_hist <- mean(LOCAL_hist$GRC)
  GMF_hist <- mean(VISITANTE_hist$GMF)
  GRF_hist <- mean(VISITANTE_hist$GRF)
  
  GMC_actual <- mean(LOCAL_actual$GMC)
  GRC_actual <- mean(LOCAL_actual$GRC)
  GMF_actual <- mean(VISITANTE_actual$GMF)
  GRF_actual <- mean(VISITANTE_actual$GRF)
  
  # Calcular capacidades ponderadas
  CAC_hist <- LOCAL_hist$GMC / GMC_hist
  CDC_hist <- LOCAL_hist$GRC / GRC_hist
  CAF_hist <- VISITANTE_hist$GMF / GMF_hist
  CDF_hist <- VISITANTE_hist$GRF / GRF_hist
  
  CAC_actual <- LOCAL_actual$GMC / GMC_actual
  CDC_actual <- LOCAL_actual$GRC / GRC_actual
  CAF_actual <- VISITANTE_actual$GMF / GMF_actual
  CDF_actual <- VISITANTE_actual$GRF / GRF_actual
  
  # Crear dataframe con capacidades ponderadas
  PRElandas_hist <- data.frame(
    equipos = LOCAL_hist$Equipo,
    CAC = CAC_hist,
    CDC = CDC_hist,
    CAF = CAF_hist,
    CDF = CDF_hist
  )
  
  PRElandas_actual <- data.frame(
    equipos = LOCAL_actual$Equipo,
    CAC = CAC_actual,
    CDC = CDC_actual,
    CAF = CAF_actual,
    CDF = CDF_actual
  )
  
  # Aplicar ponderación
  peso_hist <- 1 - peso_temporada_actual
  
  # Asegurar que ambos dataframes tienen los mismos equipos en el mismo orden
  PRElandas_hist <- PRElandas_hist[order(PRElandas_hist$equipos), ]
  PRElandas_actual <- PRElandas_actual[order(PRElandas_actual$equipos), ]
  
  # Calcular capacidades ponderadas
  PRElandas_ponderado <- data.frame(
    equipos = PRElandas_hist$equipos,
    CAC = PRElandas_hist$CAC * peso_hist + PRElandas_actual$CAC * peso_temporada_actual,
    CDC = PRElandas_hist$CDC * peso_hist + PRElandas_actual$CDC * peso_temporada_actual,
    CAF = PRElandas_hist$CAF * peso_hist + PRElandas_actual$CAF * peso_temporada_actual,
    CDF = PRElandas_hist$CDF * peso_hist + PRElandas_actual$CDF * peso_temporada_actual
  )
  
  # Devolver medias y capacidades
  return(list(
    PRElandas = PRElandas_ponderado,
    GMC = GMC_actual * peso_temporada_actual + GMC_hist * peso_hist,
    GRC = GRC_actual * peso_temporada_actual + GRC_hist * peso_hist,
    GMF = GMF_actual * peso_temporada_actual + GMF_hist * peso_hist,
    GRF = GRF_actual * peso_temporada_actual + GRF_hist * peso_hist
  ))
}

# Función para crear matrices de lambdas
crear_matrices_lambdas <- function(equipos, PRElandas, GMC, GMF) {
  n_equipos <- length(equipos)
  landasL <- matrix(0, nrow = n_equipos, ncol = n_equipos)
  landasV <- matrix(0, nrow = n_equipos, ncol = n_equipos)
  
  colnames(landasL) <- equipos
  rownames(landasL) <- equipos
  colnames(landasV) <- equipos
  rownames(landasV) <- equipos
  
  for (i in 1:n_equipos) {
    for (j in 1:n_equipos) {
      if (i != j) {
        # Lambda para el equipo local (i)
        landasL[i, j] <- PRElandas$CAC[PRElandas$equipos == equipos[i]] * 
                          PRElandas$CDF[PRElandas$equipos == equipos[j]] * GMC
        
        # Lambda para el equipo visitante (j)
        landasV[j, i] <- PRElandas$CAF[PRElandas$equipos == equipos[j]] * 
                          PRElandas$CDC[PRElandas$equipos == equipos[i]] * GMF
      }
    }
  }
  
  return(list(landasL = landasL, landasV = landasV))
}

# Función para simular partidos individuales
simular_partido <- function(equipo_local, equipo_visitante, landasL, landasV) {
  # Obtener lambdas correspondientes
  lambda_local <- landasL[equipo_local, equipo_visitante]
  lambda_visitante <- landasV[equipo_visitante, equipo_local]
  
  # Simular goles
  goles_local <- rpois(1, lambda_local)
  goles_visitante <- rpois(1, lambda_visitante)
  
  return(c(goles_local, goles_visitante))
}

# Función para predecir resultados de próximos partidos
predecir_partidos <- function(partidos_futuros, landasL, landasV, n_simulaciones = 1000) {
  n_partidos <- nrow(partidos_futuros)
  resultados <- list()
  
  for (i in 1:n_partidos) {
    local <- partidos_futuros$home[i]
    visitante <- partidos_futuros$visitor[i]
    
    # Simular múltiples veces el mismo partido
    goles_local <- numeric(n_simulaciones)
    goles_visitante <- numeric(n_simulaciones)
    
    for (j in 1:n_simulaciones) {
      sim <- simular_partido(local, visitante, landasL, landasV)
      goles_local[j] <- sim[1]
      goles_visitante[j] <- sim[2]
    }
    
    # Calcular probabilidades
    p_local <- mean(goles_local > goles_visitante)
    p_empate <- mean(goles_local == goles_visitante)
    p_visitante <- mean(goles_local < goles_visitante)
    
    # Resultado más probable
    moda_local <- as.numeric(names(sort(table(goles_local), decreasing = TRUE)[1]))
    moda_visitante <- as.numeric(names(sort(table(goles_visitante), decreasing = TRUE)[1]))
    
    # Guardar resultados
    resultados[[i]] <- list(
      local = local,
      visitante = visitante,
      p_local = p_local,
      p_empate = p_empate,
      p_visitante = p_visitante,
      media_goles_local = mean(goles_local),
      media_goles_visitante = mean(goles_visitante),
      moda_goles_local = moda_local,
      moda_goles_visitante = moda_visitante,
      resultado_probable = paste(moda_local, "-", moda_visitante)
    )
  }
  
  # Convertir a dataframe
  resultados_df <- do.call(rbind, lapply(resultados, function(x) {
    data.frame(
      Local = x$local,
      Visitante = x$visitante,
      P_Victoria_Local = round(x$p_local * 100, 1),
      P_Empate = round(x$p_empate * 100, 1),
      P_Victoria_Visitante = round(x$p_visitante * 100, 1),
      Resultado_Probable = x$resultado_probable,
      Media_Goles_Local = round(x$media_goles_local, 2),
      Media_Goles_Visitante = round(x$media_goles_visitante, 2)
    )
  }))
  
  return(resultados_df)
}

# Función para simular el resto de la temporada
simular_resto_temporada <- function(partidos_jugados, partidos_futuros, landasL, landasV, 
                                   equipos, n_simulaciones = 1000) {
  # Preparar matriz para almacenar resultados
  tabla_puntos_final <- matrix(0, nrow = length(equipos), ncol = n_simulaciones)
  rownames(tabla_puntos_final) <- equipos
  
  # Calcular puntos actuales
  puntos_actuales <- calcular_puntos_actuales(partidos_jugados, equipos)
  
  # Simular los partidos restantes n_simulaciones veces
  for (sim in 1:n_simulaciones) {
    # Copiar puntos actuales
    puntos_sim <- puntos_actuales
    
    # Simular cada partido
    for (i in 1:nrow(partidos_futuros)) {
      local <- partidos_futuros$home[i]
      visitante <- partidos_futuros$visitor[i]
      
      resultado <- simular_partido(local, visitante, landasL, landasV)
      
      # Actualizar puntos según resultado
      if (resultado[1] > resultado[2]) {
        # Victoria local
        puntos_sim[local] <- puntos_sim[local] + 3
      } else if (resultado[1] < resultado[2]) {
        # Victoria visitante
        puntos_sim[visitante] <- puntos_sim[visitante] + 3
      } else {
        # Empate
        puntos_sim[local] <- puntos_sim[local] + 1
        puntos_sim[visitante] <- puntos_sim[visitante] + 1
      }
    }
    
    # Guardar puntos finales
    tabla_puntos_final[, sim] <- puntos_sim
  }
  
  # Calcular posiciones finales para cada simulación
  tabla_posiciones_final <- apply(tabla_puntos_final, 2, function(puntos) {
    order(-puntos)
  })
  rownames(tabla_posiciones_final) <- equipos
  
  # Calcular frecuencias de posiciones
  frecuencias <- apply(tabla_posiciones_final, 1, table)
  tabla_frecuencias <- as.data.frame(bind_rows(frecuencias))
  tabla_frecuencias[is.na(tabla_frecuencias)] <- 0
  
  # Ajustar nombres de columnas
  posibles_posiciones <- sort(unique(as.vector(tabla_posiciones_final)))
  for (pos in posibles_posiciones) {
    if (as.character(pos) %in% colnames(tabla_frecuencias)) {
      colnames(tabla_frecuencias)[colnames(tabla_frecuencias) == as.character(pos)] <- equipos[pos]
    }
  }
  
  # Calcular probabilidades
  probabilidades <- 100 * tabla_frecuencias / n_simulaciones
  
  # Resultados finales medios
  puntos_medios <- rowMeans(tabla_puntos_final)
  resultados_medios <- data.frame(
    Equipo = equipos,
    Puntos_Actuales = puntos_actuales,
    Puntos_Proyectados = puntos_medios,
    Puntos_Por_Conseguir = puntos_medios - puntos_actuales
  )
  resultados_medios <- resultados_medios[order(-resultados_medios$Puntos_Proyectados), ]
  
  return(list(
    tabla_puntos_final = tabla_puntos_final,
    tabla_posiciones_final = tabla_posiciones_final,
    frecuencias = frecuencias,
    probabilidades = probabilidades,
    resultados_medios = resultados_medios
  ))
}

# Función auxiliar para calcular puntos actuales
calcular_puntos_actuales <- function(partidos_jugados, equipos) {
  # Inicializar vector de puntos
  puntos <- numeric(length(equipos))
  names(puntos) <- equipos
  
  # Procesar partidos como local
  partidos_local <- partidos_jugados %>%
    filter(home %in% equipos) %>%
    select(home, visitor, hgoal, vgoal)
  
  for (i in 1:nrow(partidos_local)) {
    local <- partidos_local$home[i]
    visitante <- partidos_local$visitor[i]
    goles_local <- partidos_local$hgoal[i]
    goles_visitante <- partidos_local$vgoal[i]
    
    if (goles_local > goles_visitante) {
      puntos[local] <- puntos[local] + 3
    } else if (goles_local < goles_visitante) {
      puntos[visitante] <- puntos[visitante] + 3
    } else {
      puntos[local] <- puntos[local] + 1
      puntos[visitante] <- puntos[visitante] + 1
    }
  }
  
  return(puntos)
}

# Función para visualizar predicciones de partidos
visualizar_predicciones <- function(predicciones) {
  require(ggplot2)
  
  # Convertir a formato largo para visualización
  predicciones_long <- predicciones %>%
    select(Local, Visitante, P_Victoria_Local, P_Empate, P_Victoria_Visitante) %>%
    tidyr::pivot_longer(
      cols = c(P_Victoria_Local, P_Empate, P_Victoria_Visitante),
      names_to = "Resultado",
      values_to = "Probabilidad"
    )
  
  # Renombrar niveles para mejor visualización
  predicciones_long$Resultado <- gsub("P_Victoria_Local", "Victoria Local", predicciones_long$Resultado)
  predicciones_long$Resultado <- gsub("P_Empate", "Empate", predicciones_long$Resultado)
  predicciones_long$Resultado <- gsub("P_Victoria_Visitante", "Victoria Visitante", predicciones_long$Resultado)
  
  # Crear etiquetas para los partidos
  predicciones_long$Partido <- paste(predicciones_long$Local, "vs", predicciones_long$Visitante)
  
  # Crear gráfico
  ggplot(predicciones_long, aes(x = Partido, y = Probabilidad, fill = Resultado)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Predicciones para próximos partidos",
         x = "Partido",
         y = "Probabilidad (%)") +
    scale_fill_manual(values = c("Victoria Local" = "blue", "Empate" = "gray", 
                                "Victoria Visitante" = "red"))
}

# Función para exportar predicciones a CSV
exportar_predicciones <- function(predicciones, directorio = "../output/tables/") {
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Exportar predicciones
  write.csv(predicciones, 
            file = paste0(directorio, "predicciones_partidos.csv"),
            row.names = FALSE)
  
  return(paste0(directorio, "predicciones_partidos.csv"))
}