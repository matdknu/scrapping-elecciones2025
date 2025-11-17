library(readr)
library(dplyr)
library(ggplot2)
library(treemapify)
library(stringr)
library(sjmisc)

# 1) Leer SIEMPRE el archivo de candidatos más actualizado ---------------------

path_datos <- "datos"
path_image <- "image"

files <- list.files(
  path_datos,
  pattern = "^candidatos_division.*\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No encontré archivos 'candidatos_primera*.csv' en la carpeta 'datos/'.")
}

# Como el nombre tiene timestamp YYYYMMDD_HHMMSS, el último ordenado es el más nuevo
file_latest <- sort(files, decreasing = TRUE)[1]
message("Usando archivo: ", file_latest)

df2025 <- read_csv(file_latest, show_col_types = FALSE)

df2021 <- readRDS("datos/proc_data/proc_primera21.rds") |> select(-orden) |> 
  mutate(porcentaje = porcentaje * 100) |> 
  filter(region != "Total general")


df2025 |> frq(region)
df2021 |> frq(region)


df2021_std <- df2021 |>
  mutate(
    region_id = as.integer(str_extract(region, "^[0-9]+")),
    region_nombre_std = case_when(
      region_id == 1  ~ "DE TARAPACA",
      region_id == 2  ~ "DE ANTOFAGASTA",
      region_id == 3  ~ "DE ATACAMA",
      region_id == 4  ~ "DE COQUIMBO",
      region_id == 5  ~ "DE VALPARAISO",
      region_id == 6  ~ "DEL LIBERTADOR GENERAL BERNARDO O'HIGGINS",
      region_id == 7  ~ "DEL MAULE",
      region_id == 8  ~ "DEL BIOBIO",
      region_id == 9  ~ "DE LA ARAUCANIA",
      region_id == 10 ~ "DE LOS LAGOS",
      region_id == 11 ~ "DE AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO",
      region_id == 12 ~ "DE MAGALLANES Y DE LA ANTARTICA CHILENA",
      region_id == 13 ~ "METROPOLITANA DE SANTIAGO",
      region_id == 14 ~ "DE LOS RIOS",
      region_id == 15 ~ "DE ARICA Y PARINACOTA",
      region_id == 16 ~ "DE ÑUBLE",
      TRUE ~ NA_character_
    )
  ) |>
  # si quieres dejar fuera "17. Extranjero" para la comparación:
  filter(region_id <= 16)


df2025_std <- df2025 |>
  mutate(
    region_nombre_std = stringr::str_squish(region),
    region_id = case_when(
      region_nombre_std == "DE TARAPACA"                                   ~ 1L,
      region_nombre_std == "DE ANTOFAGASTA"                                ~ 2L,
      region_nombre_std == "DE ATACAMA"                                    ~ 3L,
      region_nombre_std == "DE COQUIMBO"                                   ~ 4L,
      region_nombre_std == "DE VALPARAISO"                                 ~ 5L,
      region_nombre_std == "DEL LIBERTADOR GENERAL BERNARDO O'HIGGINS"     ~ 6L,
      region_nombre_std == "DEL MAULE"                                     ~ 7L,
      region_nombre_std == "DEL BIOBIO"                                    ~ 8L,
      region_nombre_std == "DE LA ARAUCANIA"                               ~ 9L,
      region_nombre_std == "DE LOS LAGOS"                                  ~ 10L,
      region_nombre_std == "DE AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO"  ~ 11L,
      region_nombre_std == "DE MAGALLANES Y DE LA ANTARTICA CHILENA"       ~ 12L,
      region_nombre_std == "METROPOLITANA DE SANTIAGO"                     ~ 13L,
      region_nombre_std == "DE LOS RIOS"                                   ~ 14L,
      region_nombre_std == "DE ARICA Y PARINACOTA"                         ~ 15L,
      region_nombre_std == "DE ÑUBLE"                                      ~ 16L,
      TRUE ~ NA_integer_
    )
  )


df2021_std |> frq(region_id)
df2025_std |> frq(region_id)

# Ver tabla de correspondencia rápida
df2021_std |> distinct(region_id, region_nombre_std) |> arrange(region_id)
df2025_std |> distinct(region_id, region_nombre_std) |> arrange(region_id)


df2021_std |> select(candidato, votos, porcentaje, region_nombre_std)
df2025_std |> select(candidato, votos, porcentaje, region_nombre_std)

df2021 |> frq(candidato)
df2025 |> frq(candidato)


# Mapeo manual de quienes existen en ambas elecciones
map_candidatos <- tibble(
  candidato_std = c(
    "Eduardo Artés Brichetti",
    "Franco Parisi Fernández",
    "José Antonio Kast Rist",
    "Marco Enríquez-Ominami Gumucio"
  ),
  cand2021 = c(
    "Eduardo Artés Brichetti",
    "Franco Parisi Fernández",
    "Jose Antonio Kast Rist",              # así viene en df2021
    "Marco Enríquez-Ominami Gumucio"
  ),
  cand2025 = c(
    "EDUARDO ANTONIO ARTES BRICHETTI",
    "FRANCO PARISI FERNANDEZ",
    "JOSE ANTONIO KAST RIST",
    "MARCO ANTONIO ENRIQUEZ-OMINAMI GUMUCIO"
  )
)

df2021  <- df2021_std |>
  # unir con mapeo usando el nombre tal como viene en 2021
  left_join(
    map_candidatos |>
      select(candidato_std, candidato = cand2021),
    by = "candidato"
  ) |>
  mutate(
    # si está en la tabla, usamos candidato_std; si no, dejamos el original
    candidato_std = coalesce(candidato_std, candidato)
  )

df2025  <- df2025_std |>
  mutate(
    candidato_upper = str_squish(str_to_upper(candidato))
  ) |>
  left_join(
    map_candidatos |>
      select(candidato_std, candidato_upper = cand2025),
    by = "candidato_upper"
  ) |>
  mutate(
    # Para quienes NO están en el mapeo, pasamos de mayúsculas a Title Case
    candidato_std = coalesce(
      candidato_std,
      str_to_title(str_to_lower(candidato))  # Evelyn Matthei, Jara, Kaiser, etc.
    )
  ) |>
  select(-candidato_upper)  # opcional, para limpiar


df2021 |>  select(candidato_std, region_nombre_std, ,porcentaje) |> mutate(anio = 2021)
df2025 |>  select(candidato_std, region_nombre_std, porcentaje)  |> mutate(anio = 2025)






library(dplyr)
library(tidyr)
library(gt)

df2021 |>
  select(candidato_std, region_nombre_std, porcentaje)

df2025 |>
  select(candidato_std, region_nombre_std, porcentaje)

# Gabriel Boric Font --> Jeannette Jara Roman (2021 - 2025) [Boric y Jara]
# Sebastián Sichel Ramírez  ---> Evelyn Matthei Fornet.    [Sichel - Matthei]
# Franco Parisi Fernández ---> Franco Parisi Fernández
# José Antonio Kast Rist --- > José Antonio Kast Rist
# No tiene 2021 --> Johannes Kaiser Barents-Von Hohenhagen



library(dplyr)
library(tidyr)
library(gt)

# 1) Dataframes base por año (ya los tenías)
df2021_comp <- df2021 |>
  select(candidato_std, region_nombre_std, porcentaje) |>
  mutate(anio = 2021)

df2025_comp <- df2025 |>
  select(candidato_std, region_nombre_std, porcentaje) |>
  mutate(anio = 2025)

# 2) Unir y definir el "bloque" de comparación  -----------------------------

comparativa_long <- bind_rows(df2021_comp, df2025_comp) |>
  mutate(
    bloque = case_when(
      # Gabriel Boric Font --> Jeannette Jara Roman  [Boric y Jara]
      (anio == 2021 & candidato_std == "Gabriel Boric Font") |
        (anio == 2025 & candidato_std == "Jeannette Jara Roman") ~ "Boric y Jara",
      
      # Sebastián Sichel Ramírez  --> Evelyn Matthei Fornet  [Sichel - Matthei]
      (anio == 2021 & candidato_std == "Sebastián Sichel Ramírez") |
        (anio == 2025 & candidato_std == "Evelyn Matthei Fornet") ~ "Sichel y Matthei",
      
      # Franco Parisi Fernández --> Franco Parisi Fernández
      candidato_std == "Franco Parisi Fernández" ~ "Franco Parisi Fernández",
      
      # José Antonio Kast Rist --> José Antonio Kast Rist
      candidato_std == "José Antonio Kast Rist"  ~ "José Antonio Kast Rist",
      
      # Solo 2025: Johannes Kaiser
      anio == 2025 & candidato_std == "Johannes Kaiser Barents-Von Hohenhagen"
      ~ "Johannes Kaiser (solo 2025)",
      
      # El resto se mantiene igual (Artés, MEO, etc.)
      TRUE ~ candidato_std
    )
  )

# 3) Pasar a formato ancho: columnas 2021 y 2025 por bloque y región --------

comparativa_wide <- comparativa_long |>
  mutate(anio = as.character(anio)) |>
  pivot_wider(
    names_from  = anio,
    values_from = porcentaje,
    names_prefix = "pct_"
  ) |>
  arrange(region_nombre_std, bloque)


library(dplyr)
library(tidyr)
library(gt)
library(scales)

# 1) Partimos del comparativa_long CON bloque ya creado ----------------------
# (reutiliza lo que ya tenías)

comparativa_long <- bind_rows(df2021_comp, df2025_comp) |>
  mutate(
    bloque = case_when(
      # Gabriel Boric Font --> Jeannette Jara Roman  [Boric y Jara]
      (anio == 2021 & candidato_std == "Gabriel Boric Font") |
        (anio == 2025 & candidato_std == "Jeannette Jara Roman") ~ "Boric y Jara",
      
      # Sebastián Sichel Ramírez  --> Evelyn Matthei Fornet  [Sichel - Matthei]
      (anio == 2021 & candidato_std == "Sebastián Sichel Ramírez") |
        (anio == 2025 & candidato_std == "Evelyn Matthei Fornet") ~ "Sichel y Matthei",
      
      # Franco Parisi Fernández --> Franco Parisi Fernández
      candidato_std == "Franco Parisi Fernández" ~ "Franco Parisi Fernández",
      
      # José Antonio Kast Rist --> José Antonio Kast Rist
      candidato_std == "José Antonio Kast Rist"  ~ "José Antonio Kast Rist",
      
      # Solo 2025: Johannes Kaiser
      anio == 2025 & candidato_std == "Johannes Kaiser Barents-Von Hohenhagen"
      ~ "Johannes Kaiser (solo 2025)",
      
      # El resto se mantiene igual (Artés, MEO, etc.)
      TRUE ~ candidato_std
    )
  )

# 2) Pasar a wide y calcular diferencia porcentual --------------------------

comparativa_wide <- comparativa_long |>
  mutate(anio = as.character(anio)) |>
  pivot_wider(
    names_from  = anio,
    values_from = porcentaje,
    names_prefix = "pct_"
  ) |>
  mutate(
    diff_pct = pct_2025 - pct_2021   # puede quedar NA donde falte uno de los años
  ) |>
  arrange(region_nombre_std, bloque)

# Rango para la escala de colores (rojo < 0, azul > 0)
diff_range <- range(comparativa_wide$diff_pct, na.rm = TRUE)


library(dplyr)
library(tidyr)
library(gt)
library(scales)

# 1) comparativa_long ya lo tienes con `bloque` definido --------------------

# 2) Agrupar por región, bloque y año (una fila por combinación) -----------

comparativa_resum <- comparativa_long |>
  group_by(region_nombre_std, bloque, anio) |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Pasar a wide y calcular diferencia -------------------------------------

comparativa_wide <- comparativa_resum |>
  mutate(anio = as.character(anio)) |>
  pivot_wider(
    names_from  = anio,
    values_from = porcentaje,
    names_prefix = "pct_"
  ) |>
  mutate(
    diff_pct = pct_2025 - pct_2021   # quedará NA solo cuando de verdad falta un año (p.ej. Kaiser)
  ) |>
  arrange(region_nombre_std, bloque)

diff_range <- range(comparativa_wide$diff_pct, na.rm = TRUE)

# 4) gt table ---------------------------------------------------------------

comparativa_wide |> frq(region_nombre_std)





# 1) comparativa_long ya tiene 'bloque' (Boric y Jara, Sichel y Matthei, etc.)

# 2) Resumir para que haya UNA fila por región-bloque-año -------------------
#    (Boric y Jara queda 1 fila 2021 y 1 fila 2025 por región)

comparativa_resum <- comparativa_long |>
  group_by(region_nombre_std, bloque, anio) |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Orden oficial de regiones (1–16) ---------------------------------------

region_order <- tibble::tibble(
  region_nombre_std = c(
    "DE TARAPACA",
    "DE ANTOFAGASTA",
    "DE ATACAMA",
    "DE COQUIMBO",
    "DE VALPARAISO",
    "DEL LIBERTADOR GENERAL BERNARDO O'HIGGINS",
    "DEL MAULE",
    "DEL BIOBIO",
    "DE LA ARAUCANIA",
    "DE LOS LAGOS",
    "DE AYSEN DEL GENERAL CARLOS IBAÑEZ DEL CAMPO",
    "DE MAGALLANES Y DE LA ANTARTICA CHILENA",
    "METROPOLITANA DE SANTIAGO",
    "DE LOS RIOS",
    "DE ARICA Y PARINACOTA",
    "DE ÑUBLE"
  ),
  region_id = 1:16
)

# 4) Wide + diferencia + orden por región del país --------------------------

comparativa_wide <- comparativa_resum |>
  mutate(anio = as.character(anio)) |>
  tidyr::pivot_wider(
    names_from  = anio,
    values_from = porcentaje,
    names_prefix = "pct_"
  ) |>
  mutate(
    diff_pct = pct_2025 - pct_2021   # Δ 2025–2021, NA solo si existe en un solo año
  ) |>
  left_join(region_order, by = "region_nombre_std") |>
  arrange(region_id, bloque)  # 👈 orden de regiones “del país”

diff_range <- range(comparativa_wide$diff_pct, na.rm = TRUE)

# 5) gt table ---------------------------------------------------------------

library(dplyr)
library(gt)
library(scales)

# Partimos de comparativa_wide con region_id, pct_2021, pct_2025, diff_pct, bloque ----

comp_filtrada <- comparativa_wide |>
  filter(bloque != "Eduardo Artés Brichetti",
         bloque != "Harold Mayne-Nicholls Secul")

# Ranking por región y año + símbolos --------------------------------------

comp_marcada <- comp_filtrada |>
  group_by(region_nombre_std) |>
  mutate(
    rank_2021 = dense_rank(desc(pct_2021)),
    rank_2025 = dense_rank(desc(pct_2025)),
    mark_2021 = case_when(
      !is.na(pct_2021) & rank_2021 == 1 ~ "*",
      !is.na(pct_2021) & rank_2021 == 2 ~ "†",
      TRUE ~ ""
    ),
    mark_2025 = case_when(
      !is.na(pct_2025) & rank_2025 == 1 ~ "*",
      !is.na(pct_2025) & rank_2025 == 2 ~ "†",
      TRUE ~ ""
    ),
    # convertimos a texto con 2 decimales + símbolo
    pct_2021_lab = if_else(
      is.na(pct_2021), NA_character_,
      sprintf("%.2f%s", pct_2021, mark_2021)
    ),
    pct_2025_lab = if_else(
      is.na(pct_2025), NA_character_,
      sprintf("%.2f%s", pct_2025, mark_2025)
    )
  ) |>
  ungroup() |>
  select(-region_id, -rank_2021, -rank_2025, -mark_2021, -mark_2025)

diff_range <- range(comp_marcada$diff_pct, na.rm = TRUE)

# gt table ------------------------------------------------------------------

tabla_gt <- comp_marcada |>
  gt(
    rowname_col   = "bloque",
    groupname_col = "region_nombre_std"
  ) |>
  # solo diff_pct es numérico ahora
  fmt_number(
    columns  = diff_pct,
    decimals = 2
  ) |>
  cols_label(
    #pct_2021_lab = "2021",
    #pct_2025_lab = "2025",
    diff_pct     = "Δ 2025–2021 (p.p.)"
  ) |>
  tab_spanner(
    label   = "Porcentaje de voto",
    columns = c(pct_2021, pct_2025)
  ) |>
  tab_stubhead(label = "Candidato / Bloque") |>
  tab_header(
    title    = "Comparativa presidencial 2021 vs 2025",
    subtitle = "Por bloque de candidato y región"
  ) |>
  data_color(
    columns = diff_pct,
    colors = scales::col_numeric(
      palette = c("#b2182b", "#f7f7f7", "#2166ac"),
      domain  = diff_range
    )
  ) |>
  tab_footnote(
    footnote = "*: primer lugar; †: segundo lugar por año y región",
    locations = cells_column_labels(columns = c(pct_2021, pct_2025))
  )

tabla_gt


write_rds(comp_display, "datos/proc_data/comp_display.rds")
write_rds(tabla_gt, "datos/proc_data/tabla_gt.rds")

library(dplyr)
library(gt)
library(scales)

# Partimos de comparativa_wide con region_id, pct_2021, pct_2025, diff_pct, bloque ----

comp_filtrada <- comparativa_wide |>
  filter(bloque != "Eduardo Artés Brichetti",
         bloque != "Harold Mayne-Nicholls Secul")

# Ranking por región y año + símbolos --------------------------------------
comp_marcada <- comp_filtrada |>
  group_by(region_nombre_std) |>
  mutate(
    rank_2021 = dense_rank(desc(pct_2021)),
    rank_2025 = dense_rank(desc(pct_2025)),
    mark_2021 = case_when(
      !is.na(pct_2021) & rank_2021 == 1 ~ "*",
      !is.na(pct_2021) & rank_2021 == 2 ~ "†",
      TRUE ~ ""
    ),
    mark_2025 = case_when(
      !is.na(pct_2025) & rank_2025 == 1 ~ "*",
      !is.na(pct_2025) & rank_2025 == 2 ~ "†",
      TRUE ~ ""
    ),
    # número + símbolo en la MISMA celda
    pct_2021_lab = if_else(
      is.na(pct_2021), NA_character_,
      sprintf("%.2f%s", pct_2021, mark_2021)
    ),
    pct_2025_lab = if_else(
      is.na(pct_2025), NA_character_,
      sprintf("%.2f%s", pct_2025, mark_2025)
    )
  ) |>
  ungroup()

# Nos quedamos solo con lo que queremos mostrar en la tabla -----------------
comp_display <- comp_marcada |>
  select(region_nombre_std, bloque, pct_2021_lab, pct_2025_lab, diff_pct)

diff_range <- range(comp_display$diff_pct, na.rm = TRUE)

# gt table ------------------------------------------------------------------
tabla_gt <- comp_display |>
  gt(
    rowname_col   = "bloque",
    groupname_col = "region_nombre_std"
  ) |>
  fmt_number(
    columns  = diff_pct,
    decimals = 2
  ) |>
  cols_label(
    pct_2021_lab = "2021",
    pct_2025_lab = "2025",
    diff_pct     = "Δ 2025–2021 (p.p.)"
  ) |>
  tab_spanner(
    label   = "Porcentaje de voto",
    columns = c(pct_2021_lab, pct_2025_lab)
  ) |>
  tab_stubhead(label = "Candidato / Bloque") |>
  tab_header(
    title    = "Comparativa presidencial 2021 vs 2025",
    subtitle = "Por bloque de candidato y región"
  ) |>
  data_color(
    columns = diff_pct,
    colors = scales::col_numeric(
      palette = c("#b2182b", "#f7f7f7", "#2166ac"),
      domain  = diff_range
    )
  ) |>
  tab_footnote(
    footnote = "*: primer lugar; †: segundo lugar por año y región",
    locations = cells_column_labels(columns = c(pct_2021_lab, pct_2025_lab))
  )

tabla_gt


library(gt)

# Guardar como HTML
gtsave(
  data = tabla_gt,
  filename = "output/tabla_comparativa_2021_2025.html"
)

# (opcional) Guardar como PNG
gtsave(
  data = tabla_gt,
  filename = "output/tabla_comparativa_2021_2025.png"
)

# (opcional) Guardar como RTF para Word
gtsave(
  data = tabla_gt,
  filename = "output/tabla_comparativa_2021_2025.rtf"
)



library(dplyr)
library(gt)
library(purrr)
library(stringr)
library(scales)

# Rango para los colores de la diferencia
diff_range <- range(comp_display$diff_pct, na.rm = TRUE)

# 1) Función que arma la tabla gt para UNA región --------------------------
make_tab_region <- function(df_region, nombre_region) {
  df_region |>
    select(-region_nombre_std) |>
    gt(rowname_col = "bloque") |>
    fmt_number(
      columns  = diff_pct,
      decimals = 2
    ) |>
    cols_label(
      pct_2021_lab = "2021",
      pct_2025_lab = "2025",
      diff_pct     = "Δ 2025–2021 (p.p.)"
    ) |>
    tab_spanner(
      label   = "Porcentaje de voto",
      columns = c(pct_2021_lab, pct_2025_lab)
    ) |>
    tab_stubhead(label = "Candidato / Bloque") |>
    tab_header(
      title    = "Comparativa presidencial 2021 vs 2025",
      subtitle = nombre_region
    ) |>
    data_color(
      columns = diff_pct,
      colors = scales::col_numeric(
        palette = c("#b2182b", "#f7f7f7", "#2166ac"),
        domain  = diff_range
      )
    ) |>
    tab_footnote(
      footnote = "*: primer lugar; †: segundo lugar por año y región",
      locations = cells_column_labels(columns = c(pct_2021_lab, pct_2025_lab))
    )
}

# 2) Lista de tablas gt, una por región ------------------------------------
tablas_por_region <- comp_display |>
  split(comp_display$region_nombre_std) |>
  imap(~ make_tab_region(.x, .y))

# Ejemplo: ver Tarapacá
tablas_por_region[["DE TARAPACA"]]

dir.create("output/tablas_regiones", showWarnings = FALSE)

imap(tablas_por_region, ~ {
  file_slug <- .y |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace("_$", "")
  
  gtsave(
    data     = .x,
    filename = file.path("output/tablas_regiones",
                         paste0("tabla_", file_slug, ".html"))
  )
})


