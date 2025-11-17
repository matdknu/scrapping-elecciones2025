library(RSelenium)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(fs)

# 0. Crear carpeta datos si no existe
dir_create("datos")

# 1. Levantar navegador (usa geckodriver, ignora phantomjs)
rD <- rsDriver(
  browser = "firefox",
  port    = 4540L,
  verbose = FALSE,
  check   = FALSE
)

remDr <- rD$client

# 2. Función que toma una "foto" de los resultados nacionales
scrap_servel_snapshot <- function(remDr) {
  
  remDr$navigate("https://elecciones.servel.cl")
  Sys.sleep(5)  # deja que cargue la app
  
  html <- remDr$getPageSource()[[1]]
  doc  <- read_html(html)
  
  tabs <- html_elements(doc, "table")
  
  # headers de cada tabla,
  # (esto ya lo probaste, pero lo dejamos dentro para robustez)
  headers_list <- map(seq_along(tabs), \(i) {
    html_elements(tabs[[i]], "thead th") |> html_text2()
  })
  
  # índice de la tabla que tiene la columna "Candidatos"
  idx_cand <- which(vapply(headers_list, \(h) "Candidatos" %in% h, logical(1)))
  
  if (!length(idx_cand)) {
    stop("No encontré tabla con encabezado 'Candidatos'.")
  }
  
  tabla_cand <- html_table(tabs[[idx_cand[1]]], fill = TRUE) |> 
    as_tibble()
  
  # separar candidatos (filas 1–8) y resumen (9–12)
  candidatos <- tabla_cand |> 
    slice(1:8) |>
    rename(
      candidato  = Candidatos,
      votos_raw  = Votos,
      porcentaje = Porcentaje
    )
  
  resumen <- tabla_cand |>
    slice(9:12) |>
    rename(
      categoria  = Candidatos,
      valor      = Votos,
      porcentaje = Porcentaje
    )
  
  list(
    candidatos = candidatos,
    resumen    = resumen
  )
}



guardar_snapshot <- function(remDr, dir_out = "datos") {
  snap <- scrap_servel_snapshot(remDr)
  
  # timestamp YYYYMMDD_HHMMSS
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  file_cand   <- file.path(dir_out, paste0("candidatos_primera", ts, ".csv"))
  file_res    <- file.path(dir_out, paste0("resumen_primera", ts, ".csv"))
  
  write_csv(snap$candidatos, file_cand)
  write_csv(snap$resumen,    file_res)
  
  message("Guardado:\n  - ", file_cand, "\n  - ", file_res)
  
  invisible(snap)
}

# Una foto ahora:
guardar_snapshot(remDr)


