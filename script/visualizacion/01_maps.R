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
#df_raw <- read_csv("datos/presidencial_2025_nac_region_distrito_comuna_20251117_004240.csv")
df_raw <- read_csv("datos/presidencial_2025_nac_region_distrito_comuna_20251117_103755.csv")


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
library(dplyr)
library(ggplot2)
library(sf)
library(forcats)

mapa_ganador_plot <- mapa_ganador |>
  mutate(
    candidato_lab = case_when(
      candidato_ganador == "EVELYN MATTHEI FORNET" ~ "Evelyn Matthei Fornet",
      candidato_ganador == "FRANCO PARISI FERNANDEZ" ~ "Franco Parisi Fernandez",
      candidato_ganador == "JEANNETTE JARA ROMAN" ~ "Jeannette Jara Roman",
      candidato_ganador == "JOHANNES KAISER BARENTS-VON HOHENHAGEN" ~
        "Johannes Kaiser Barents-von Hohenhagen",
      candidato_ganador == "JOSE ANTONIO KAST RIST" ~ "Jose Antonio Kast Rist",
      TRUE ~ NA_character_
    ),
    candidato_lab = fct_relevel(
      candidato_lab,
      "Evelyn Matthei Fornet",
      "Franco Parisi Fernandez",
      "Jeannette Jara Roman",
      "Johannes Kaiser Barents-von Hohenhagen",
      "Jose Antonio Kast Rist"
    )
  )

maps <- mapa_ganador_plot |>
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = candidato_lab),
          colour = "white", linewidth = 0.15) +
  scale_fill_manual(
    name = "Candidato ganador",
    values = c(
      "Jose Antonio Kast Rist" = "#377eb8",      # azul
      "Jeannette Jara Roman" = "#d73027",        # rojo
      "Franco Parisi Fernandez" = "#1b9e77",     # verde
      "Evelyn Matthei Fornet" = "#ffd92f",       # amarillo
      "Johannes Kaiser Barents-von Hohenhagen" = "#4daf4a"  # verde distinto
    ),
    na.value = "grey70"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Candidato presidencial ganador por comuna 2025",
    fill  = "Candidato ganador",
    caption = "Fuente: Servel. Elaboración propia."
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(size = 9),
    plot.title   = element_text(face = "bold", hjust = 0.5),
    plot.margin  = margin(5, 5, 5, 5)
  )

maps


ggsave(
  file.path(path_image, paste0("maps_chile", ts, ".png")),
  plot = maps,
  width = 7, height = 5, dpi = 300
)



mapa_ganador |> frq(codigo_region)
mapa_metro <- mapa_ganador |> filter(codigo_region == 13)

map_rm <-mapa_metro |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, 
              fill = candidato_ganador),
          col = "white") 


map_rm

ggsave(
  file.path(path_image, paste0("maps_rm", ts, ".png")),
  plot = map_rm, 
  width = 7, height = 5, dpi = 300
)



library(dplyr)
library(ggplot2)
library(sf)
library(purrr)
library(glue)

# Vector de códigos de región
regiones <- sort(unique(mapa_ganador$codigo_region))

mapa_ganador |> select(comuna_mapa)

plot_region <- function(reg) {
  mapa_ganador |>
    filter(codigo_region == reg) |>
    ggplot() +
    geom_sf(aes(geometry = geometry,
                fill = candidato_ganador),
            colour = "white", linewidth = 0.2) +
    labs(
      title = glue("Candidato ganador - Región {reg}"),
      fill  = "Candidato ganador"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

# Lista con todos los mapas (uno por región)
plots_por_region <- map(regiones, plot_region)
names(plots_por_region) <- regiones

# Ver, por ejemplo, la Región Metropolitana (13)
plots_por_region[["13"]]



plots_por_region[["01"]]
plots_por_region[["02"]]
plots_por_region[["03"]]
plots_por_region[["04"]]
plots_por_region[["05"]]
plots_por_region[["06"]]
plots_por_region[["07"]]
plots_por_region[["08"]]
plots_por_region[["09"]]
plots_por_region[["10"]]
plots_por_region[["11"]]
plots_por_region[["12"]]
plots_por_region[["13"]]
plots_por_region[["14"]]
plots_por_region[["15"]]
plots_por_region[["16"]]


# (Opcional) Guardar cada mapa en un archivo PNG
walk(regiones, \(reg) {
  ggsave(
    filename = glue("output/map_region_{reg}.png"),
    plot     = plot_region(reg),
    width = 6, height = 6, dpi = 300
  )
})


# Maps 2021 -----

proc_primera21 <- readRDS("datos/proc_data/proc_primera21.rds")

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


mapa_comunas





