# 04_simulacion.R
# Script para simulacion de temporadas completas

#Funcion para simular una temporada entera
simular_temporada <- function(equipos_estudio, landasL, landasV) {
  n_equipos <- length(equipos_estudio)
  resultado2 <- array(0, dim = c(n_equipos, n_equipos, 2))
  rownames(resultado2) <- equipos_estudio
  colnames(resultado2) <- equipos_estudio
  
  # Simular todos los partidos de la temporada
  for (i in 1:n_equipos) {
    for (j in 1:n_equipos) {
      if (i == j) {
        resultado2[i, j, 1] <- NA
        resultado2[i, j, 2] <- NA
      } else {
        parti <- match(landasL[i, j], landasV[j, i])
        resultado2[i, j, 1] <- parti[1] # A FAVOR. El local es la fila, columna visitante.
        resultado2[i, j, 2] <- parti[2] # EN CONTRA. El local es la fila, columna visitante.
      }
    }
  }
  
  # Calcular estadísticas
  diag(resultado2[,,1]) <- NA
  diag(resultado2[,,2]) <- NA
  dif <- resultado2[,,1] - resultado2[,,2]
  
  colnames(dif) <- equipos_estudio
  rownames(dif) <- equipos_estudio
  dift <- t(dif)
  
  # Goles a favor: suma de goles como local + goles como visitante
  gol_F <- rowSums(resultado2[,,1], na.rm = TRUE) + 
           rowSums(t(resultado2[,,2]), na.rm = TRUE)
  
  # Goles en contra: suma de goles en contra como local + goles en contra como visitante
  gol_C <- rowSums(resultado2[,,2], na.rm = TRUE) + 
           rowSums(t(resultado2[,,1]), na.rm = TRUE)
  
  # Partidos ganados, empatados y perdidos
  part_gana2 <- rowSums((dif > 0) + (dift < 0), na.rm = TRUE)
  part_empata2 <- rowSums((dif == 0) + (dift == 0), na.rm = TRUE)
  part_perdi2 <- 38 - part_gana2 - part_empata2
  
  # Puntos totales
  puntos <- rowSums((dif > 0) * 3 + (dif == 0) * 1 + (dift < 0) * 3 + (dift == 0) * 1,
                   na.rm = TRUE)
  
  # Crear dataframe con resultados
  temporada <- data.frame(
    puntos = puntos,
    GF = gol_F,
    GC = gol_C,
    PG = part_gana2,
    PE = part_empata2,
    PP = part_perdi2
  )
  
  return(temporada)
}

#' Realiza múltiples simulaciones de temporadas
#' 
#' @param equipos_estudio Vector con nombres de equipos
#' @param landasL Matriz de parámetros lambda para equipos locales
#' @param landasV Matriz de parámetros lambda para equipos visitantes
#' @param replicaciones Número de temporadas a simular
#' @return Lista con resultados de las simulaciones
realizar_simulaciones <- function(equipos_estudio, landasL, landasV, replicaciones = 1000) {
  tabla_puntos <- matrix(0, 20, replicaciones)
  tabla_GF <- matrix(0, 20, replicaciones)
  tabla_GC <- matrix(0, 20, replicaciones)
  tabla_PG <- matrix(0, 20, replicaciones)
  tabla_PE <- matrix(0, 20, replicaciones)
  tabla_PP <- matrix(0, 20, replicaciones)
  tabla_posiciones <- matrix(0, 20, replicaciones)
  
  rownames(tabla_puntos) <- equipos_estudio
  rownames(tabla_GF) <- equipos_estudio
  rownames(tabla_GC) <- equipos_estudio
  rownames(tabla_PG) <- equipos_estudio
  rownames(tabla_PE) <- equipos_estudio
  rownames(tabla_PP) <- equipos_estudio
  rownames(tabla_posiciones) <- paste("Posición", 1:20)
  
  # Ejecutar las simulaciones
  for (k in 1:replicaciones) {
    # Simular temporada completa
    temporada <- simular_temporada(equipos_estudio, landasL, landasV)
    
    # Almacenar resultados
    tabla_puntos[, k] <- temporada$puntos
    tabla_GF[, k] <- temporada$GF
    tabla_GC[, k] <- temporada$GC
    tabla_PG[, k] <- temporada$PG
    tabla_PE[, k] <- temporada$PE
    tabla_PP[, k] <- temporada$PP
    tabla_posiciones[, k] <- order(-temporada$puntos)
  }
  
  # Calcular frecuencias de posiciones
  frecuencias <- apply(tabla_posiciones, 1, table)
  
  # Crear tabla de frecuencias
  tabla_frecuencias <- as.data.frame(bind_rows(frecuencias))
  tabla_frecuencias[is.na(tabla_frecuencias)] <- 0
  nom_col <- as.integer(colnames(tabla_frecuencias))
  for (e in 1:length(nom_col)) {
    if (!is.na(nom_col[e])) {
      a <- as.integer(nom_col[e])
      if (a <= length(equipos_estudio)) {
        nom_columna <- equipos_estudio[a]
        colnames(tabla_frecuencias)[e] <- nom_columna
      }
    }
  }
  posiciones <- paste("Posición", c(1:20))
  rownames(tabla_frecuencias) <- posiciones
  
  # Calcular probabilidades
  probabilidades <- 100 * (tabla_frecuencias / replicaciones)
  
  # Resultados promedio
  resultados_df <- data.frame(
    Equipo = equipos_estudio,
    Puntos = rowMeans(tabla_puntos), 
    G = rowMeans(tabla_PG),
    E = rowMeans(tabla_PE),
    P = rowMeans(tabla_PP),
    GF = rowMeans(tabla_GF),
    GC = rowMeans(tabla_GC),
    DG = rowMeans(tabla_GF) - rowMeans(tabla_GC)
  )
  resultados_df <- resultados_df[order(-resultados_df$Puntos), ]
  
  return(list(
    tabla_puntos = tabla_puntos,
    tabla_GF = tabla_GF,
    tabla_GC = tabla_GC,
    tabla_PG = tabla_PG,
    tabla_PE = tabla_PE,
    tabla_PP = tabla_PP,
    tabla_posiciones = tabla_posiciones,
    frecuencias = frecuencias,
    tabla_frecuencias = tabla_frecuencias,
    probabilidades = probabilidades,
    resultados_df = resultados_df
  ))
}

#' Crea visualizaciones de los resultados de simulación
#' 
#' @param resultados_simulacion Lista con resultados de simulaciones
#' @param top_equipos Número de equipos principales a mostrar
#' @return Lista con gráficos generados
visualizar_resultados <- function(resultados_simulacion, top_equipos = 5) {
  require(ggplot2)
  require(reshape2)
  
  # Extraer datos necesarios
  probabilidades <- resultados_simulacion$probabilidades
  tabla_puntos <- resultados_simulacion$tabla_puntos
  resultados_df <- resultados_simulacion$resultados_df
  
  # Lista para almacenar gráficos
  graficos <- list()
  
  # 1. Gráfico de barras para probabilidades de campeón
  prob_campeon <- data.frame(
    Equipo = colnames(probabilidades),
    Probabilidad = as.numeric(probabilidades["Posición 1", ])
  )
  prob_campeon <- prob_campeon[order(-prob_campeon$Probabilidad), ][1:top_equipos, ]
  
  graficos$campeon <- ggplot(prob_campeon, aes(x = reorder(Equipo, Probabilidad), y = Probabilidad)) +
    geom_bar(stat = "identity", fill = "blue") +
    coord_flip() +
    labs(title = "Probabilidad de ser campeón", x = "", y = "Probabilidad (%)") +
    theme_minimal()
  
  # 2. Gráfico de probabilidades de descenso
  prob_descenso <- data.frame(
    Equipo = colnames(probabilidades),
    Probabilidad = colSums(probabilidades[c("Posición 18", "Posición 19", "Posición 20"), ])
  )
  prob_descenso <- prob_descenso[order(-prob_descenso$Probabilidad), ][1:top_equipos, ]
  
  graficos$descenso <- ggplot(prob_descenso, aes(x = reorder(Equipo, Probabilidad), y = Probabilidad)) +
    geom_bar(stat = "identity", fill = "red") +
    coord_flip() +
    labs(title = "Probabilidad de descenso", x = "", y = "Probabilidad (%)") +
    theme_minimal()
  
  # 3. Distribución de puntos para los equipos principales
  equipos_top <- resultados_df$Equipo[1:top_equipos]
  puntos_top <- melt(tabla_puntos[equipos_top, ])
  colnames(puntos_top) <- c("Equipo", "Simulacion", "Puntos")
  
  graficos$distribucion <- ggplot(puntos_top, aes(x = Puntos, fill = Equipo)) +
    geom_density(alpha = 0.5) +
    labs(title = "Distribución de puntos para equipos top", x = "Puntos", y = "Densidad") +
    theme_minimal()
  
  return(graficos)
}

#' Guarda los gráficos generados en archivos
#' 
#' @param graficos Lista con gráficos generados
#' @param directorio Directorio donde guardar los archivos
#' @return TRUE si se guardaron correctamente
guardar_visualizaciones <- function(graficos, directorio = "../output/figures/") {
  require(ggplot2)
  
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Guardar cada gráfico
  ggsave(paste0(directorio, "probabilidad_campeon.png"), graficos$campeon, width = 8, height = 6)
  ggsave(paste0(directorio, "probabilidad_descenso.png"), graficos$descenso, width = 8, height = 6)
  ggsave(paste0(directorio, "distribucion_puntos.png"), graficos$distribucion, width = 10, height = 6)
  
  return(TRUE)
}