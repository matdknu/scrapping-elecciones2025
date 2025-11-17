# ============================================================
# 01_mapa_ganador_presidencial_2025.R
# ============================================================

# ---- 0. Paquetes ----
library(tidyverse)   # dplyr, stringr, readr, etc.
library(janitor)     # clean_names
library(chilemapas)  # mapas de Chile
library(sf)          # objetos espaciales
library(sjmisc)

path_datos <- "datos"
path_image <- "image"

# ---- 1. Leer datos Servel ----
df_raw <- read_csv("datos/presidencial_2025_nac_region_distrito_comuna_20251117_004240.csv")


# ---- 2. Función de estandarización (minúsculas, sin tildes) ----
normalizar_comuna <- function(x) {
  x |>
    str_squish() |>                                  # quita espacios extra
    iconv(from = "", to = "ASCII//TRANSLIT") |>      # quita tildes, ñ -> n, etc.
    str_to_lower()                                   # todo en minúsculas
}

# ---- 3. Quedarse solo con nivel comuna y estandarizar nombres ----
elecciones_comuna <- df_raw |>
  clean_names() |>
  filter(nivel == "comuna") |>
  transmute(
    comuna_original = comuna,
    comuna_std      = normalizar_comuna(comuna),
    candidato,
    porcentaje
  ) |>
  drop_na(porcentaje)

elecciones_comuna |> frq(comuna_std)

# ---- 4. Candidato ganador por comuna ----
ganador_comuna <- elecciones_comuna |>
  group_by(comuna_std) |>
  slice_max(order_by = porcentaje, n = 1, with_ties = FALSE) |>
  ungroup()

# ---- 5. Mapa de comunas (chilemapas) + estandarización equivalente ----
mapa_comunas <- chilemapas::mapa_comunas |>
  left_join(
    chilemapas::codigos_territoriales |>
      select(codigo_comuna, nombre_comuna),
    by = "codigo_comuna"
  ) |>
  rename(comuna_mapa = nombre_comuna) |>
  mutate(
    comuna_std = normalizar_comuna(comuna_mapa)
  )

# (Opcional) check rápido:
mapa_comunas |> select(comuna_mapa, comuna_std) |> print(n = 20)
elecciones_comuna |> select(comuna_original, comuna_std) |> print(n = 20)

unique(mapa_comunas$comuna_std)
unique(elecciones_comuna$comuna_std)

# ---- 6. Unir ganador por comuna al mapa ----
mapa_ganador <- mapa_comunas |>
  left_join(
    ganador_comuna |>
      select(
        comuna_std,
        candidato_ganador  = candidato,
        porcentaje_ganador = porcentaje,
        comuna_servel      = comuna_original
      ),
    by = "comuna_std"
  ) |> 
  filter(comuna_std != "isla de pascua") |> 
  filter(comuna_std != "juan fernandez")

# ---- 7. Diagnóstico de comunas sin dato (opcional) ----
comunas_sin_dato <- mapa_ganador |>
  filter(is.na(candidato_ganador)) |>
  select(codigo_comuna, comuna_mapa, comuna_std)

comunas_sin_dato |> print(n = 50)


# ---- 8. Ejemplo de mapa (opcional) ----
# library(ggplot2)
maps <- mapa_ganador |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, 
              fill = candidato_ganador),
          col = "white") 


ggsave(
  file.path(path_image, paste0("maps", ts, ".png")),
  plot = maps,
  width = 7, height = 5, dpi = 300
)

