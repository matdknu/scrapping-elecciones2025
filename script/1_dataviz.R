# 02_dataviz.R

library(readr)
library(dplyr)
library(ggplot2)
library(treemapify)
library(stringr)

# 1) Leer SIEMPRE el archivo de candidatos más actualizado ---------------------

path_datos <- "datos"
path_image <- "image"

files <- list.files(
  path_datos,
  pattern = "^candidatos_.*\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No encontré archivos 'candidatos_primera*.csv' en la carpeta 'datos/'.")
}

# Como el nombre tiene timestamp YYYYMMDD_HHMMSS, el último ordenado es el más nuevo
file_latest <- sort(files, decreasing = TRUE)[1]
message("Usando archivo: ", file_latest)

df_raw <- read_csv(file_latest, show_col_types = FALSE)

df_raw

# 2) Asegurar nombres y tipos numéricos ----------------------------------------

# Intentar estandarizar nombres de columnas
library(readr)
library(dplyr)
library(stringr)

loc <- locale(decimal_mark = ",", grouping_mark = ".")

df <- df_raw |>
  mutate(
    # sacar el número inicial "1 ", "2 ", etc.
    candidato  = str_remove(candidato, "^\\d+\\s+"),
    votos      = parse_number(votos_raw, locale = loc),
    porcentaje = parse_number(porcentaje, locale = loc)
  ) |>
  select(candidato, votos, porcentaje)

df


# Si ya viene limpio de tu script anterior, esto casi no hace nada.
# Pero por si acaso viene con puntos de miles y comas decimales:
loc <- locale(decimal_mark = ",", grouping_mark = ".")

df <- df |>
  mutate(
    votos = if (!is.numeric(votos)) readr::parse_number(votos, locale = loc) else votos,
    porcentaje = if (!is.numeric(porcentaje)) readr::parse_number(porcentaje, locale = loc) else porcentaje
  )

# Si por error se colaron filas de totales, nos quedamos solo con las candidaturas
# (asumiendo que los totales no empiezan con número)
df_cand <- df |>
  filter(str_detect(candidato, "^\\d+\\s") | !str_detect(candidato, "Válidamente|Nulos|Blanco|Total")) |>
  mutate(
    # Si tienes el número delante, lo sacamos y dejamos solo el nombre
    candidato = str_remove(candidato, "^\\d+\\s+")
  )

# 3) Gráfico de barras ---------------------------------------------------------

p_bar <- ggplot(df_cand, aes(x = reorder(candidato, votos), y = votos)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  geom_text(
    aes(label = scales::comma(votos)),
    hjust = -0.1,
    size = 3
  ) +
  labs(
    x = NULL,
    y = "Votos",
    title = "Resultados preliminares – votos por candidatura",
    subtitle = basename(file_latest)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_minimal(base_size = 12)

print(p_bar)

# 4) Treemap -------------------------------------------------------------------

p_tree <- ggplot(
  df_cand,
  aes(
    area  = votos,
    fill  = candidato,
    label = paste0(candidato, "\n", sprintf('%.1f%%', porcentaje))
  )
) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place  = "centre",
    grow   = TRUE,
    reflow = TRUE,
    colour = "white",
    min.size = 4
  ) +
  # Si quieres escala en grises:
  # scale_fill_grey(start = 0.2, end = 0.8, guide = "none") +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  labs(
    title = "Distribución de votos por candidatura (treemap)",
    subtitle = basename(file_latest)
  ) +
  theme_minimal(base_size = 12)

print(p_tree)

# 5) (Opcional) Guardar gráficos en la misma carpeta --------------------------

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

ggsave(
  file.path(path_image, paste0("barras_", ts, ".png")),
  plot = p_bar,
  width = 7, height = 5, dpi = 300
)

ggsave(
  file.path(path_image, paste0("treemap_", ts, ".png")),
  plot = p_tree,
  width = 7, height = 5, dpi = 300
)

