library(RSelenium)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(fs)

dir_create("datos")
loc <- locale(decimal_mark = ",", grouping_mark = ".")

# --- 1. Función para leer tabla de candidatos de la página actual ----
parse_tabla_candidatos <- function(html, extra_cols = list()) {
  doc  <- read_html(html)
  tabs <- html_elements(doc, "table")
  
  # buscamos la tabla que tenga header "Candidatos"
  headers_list <- map(seq_along(tabs), \(i) {
    html_elements(tabs[[i]], "thead th") |> html_text2()
  })
  idx_cand <- which(vapply(headers_list, \(h) "Candidatos" %in% h, logical(1)))
  if (!length(idx_cand)) stop("No encontré tabla con 'Candidatos'.")
  
  tabla_cand <- html_table(
    tabs[[idx_cand[1]]],
    fill    = TRUE,
    convert = FALSE
  ) |> as_tibble()
  
  candidatos <- tabla_cand |>
    filter(str_detect(Candidatos, "^\\d+\\s")) |>
    mutate(
      candidato  = str_remove(Candidatos, "^\\d+\\s+"),
      votos      = readr::parse_number(as.character(Votos),      locale = loc),
      porcentaje = readr::parse_number(as.character(Porcentaje), locale = loc)
    ) |>
    select(candidato, votos, porcentaje)
  
  if (length(extra_cols)) {
    for (nm in names(extra_cols)) {
      candidatos[[nm]] <- extra_cols[[nm]]
    }
  }
  candidatos
}

# --- 2. Helpers para encontrar selects por etiqueta de texto ----
find_select_by_label <- function(remDr, label_text) {
  xp <- sprintf(
    "//div[contains(@class,'text-filtros') and normalize-space()='%s']/following-sibling::select[1]",
    label_text
  )
  remDr$findElement(using = "xpath", value = xp)
}

# --- 3. Levantar navegador y navegar a la página ----
SELENIUM_PORT <- 4667L
rD <- rsDriver(
  browser = "firefox",
  port    = SELENIUM_PORT,
  verbose = FALSE,
  check   = FALSE
)
remDr <- rD$client
remDr$setTimeout(type = "implicit", milliseconds = 5000)

remDr$navigate("https://elecciones.servel.cl")
Sys.sleep(5)

# Aseguramos "Presidente"
btn_presidente <- remDr$findElement(
  using = "xpath",
  value = "//button[span[contains(normalize-space(.), 'Presidente')]]"
)
btn_presidente$clickElement()
Sys.sleep(2)

# --- 4. Nacional (opcional) ----
html_nac <- remDr$getPageSource()[[1]]
res_nacional <- parse_tabla_candidatos(
  html_nac,
  extra_cols = list(
    nivel    = "nacional",
    region   = NA_character_,
    distrito = NA_character_,
    comuna   = NA_character_
  )
)

# --- 5. Ir a "División Electoral Chile" ----
btn_div_electoral <- remDr$findElement(
  using = "css selector",
  value = "button[data-id='División Electoral Chile']"
)
btn_div_electoral$clickElement()
Sys.sleep(3)

# --- 6. Obtener listado de REGIONES ----
sel_region <- find_select_by_label(remDr, "Región")
reg_html   <- sel_region$getElementAttribute("innerHTML")[[1]]
doc_reg    <- read_html(paste0("<select>", reg_html, "</select>"))
opt_reg    <- html_elements(doc_reg, "option")
reg_nombres <- html_text2(opt_reg)
regiones_validas <- reg_nombres[reg_nombres != "" & reg_nombres != "Seleccionar"]

cat("Regiones detectadas:\n")
print(regiones_validas)

# --- 7. Loop Región → Distrito → Comuna ----
res_reg_dist_com <- map_dfr(regiones_validas, function(reg_nom) {
  
  message("=== Región: ", reg_nom, " ===")
  
  # 7a) seleccionar región
  sel_region <- find_select_by_label(remDr, "Región")
  sel_region$clickElement()
  Sys.sleep(0.3)
  
  opt_reg_dom <- sel_region$findChildElements("tag name", "option")
  textos_reg  <- vapply(opt_reg_dom,
                        function(o) str_squish(o$getElementText()[[1]]),
                        character(1))
  idx_reg <- which(textos_reg == str_squish(reg_nom))
  if (!length(idx_reg)) {
    warning("No encontré región '", reg_nom, "'. La salto.")
    return(tibble())
  }
  opt_reg_dom[[idx_reg[1]]]$clickElement()
  Sys.sleep(3)
  
  # 7b) tabla a nivel región
  html_reg <- remDr$getPageSource()[[1]]
  res_region <- parse_tabla_candidatos(
    html_reg,
    extra_cols = list(
      nivel    = "region",
      region   = reg_nom,
      distrito = NA_character_,
      comuna   = NA_character_
    )
  )
  
  # 7c) DISTRICTOS para esa región
  sel_dist <- find_select_by_label(remDr, "Distrito")
  dist_html <- sel_dist$getElementAttribute("innerHTML")[[1]]
  doc_dist  <- read_html(paste0("<select>", dist_html, "</select>"))
  opt_dist  <- html_elements(doc_dist, "option")
  dist_nombres <- html_text2(opt_dist)
  distritos_validos <- dist_nombres[dist_nombres != "" & dist_nombres != "Seleccionar"]
  
  message("  Distritos en región: ", length(distritos_validos))
  
  # 7d) Loop por distrito y luego por comuna
  res_distritos <- map_dfr(distritos_validos, function(dist_nom) {
    
    message("  - Distrito: ", dist_nom)
    
    # seleccionar distrito
    sel_dist <- find_select_by_label(remDr, "Distrito")
    sel_dist$clickElement()
    Sys.sleep(0.3)
    
    opt_dist_dom <- sel_dist$findChildElements("tag name", "option")
    textos_dist  <- vapply(opt_dist_dom,
                           function(o) str_squish(o$getElementText()[[1]]),
                           character(1))
    idx_dist <- which(textos_dist == str_squish(dist_nom))
    if (!length(idx_dist)) {
      warning("No encontré distrito '", dist_nom,
              "' en región '", reg_nom, "'. Lo salto.")
      return(tibble())
    }
    opt_dist_dom[[idx_dist[1]]]$clickElement()
    Sys.sleep(2)
    
    # COMUNAS del distrito
    sel_comuna <- find_select_by_label(remDr, "Comuna")
    com_html   <- sel_comuna$getElementAttribute("innerHTML")[[1]]
    doc_com    <- read_html(paste0("<select>", com_html, "</select>"))
    opt_com    <- html_elements(doc_com, "option")
    com_nombres <- html_text2(opt_com)
    comunas_validas <- com_nombres[com_nombres != "" & com_nombres != "Seleccionar"]
    
    message("    Comunas en distrito: ", length(comunas_validas))
    
    # loop comunas
    res_comunas <- map_dfr(comunas_validas, function(com_nom) {
      message("      · Comuna: ", com_nom)
      
      sel_comuna <- find_select_by_label(remDr, "Comuna")
      sel_comuna$clickElement()
      Sys.sleep(0.2)
      
      opt_com_dom <- sel_comuna$findChildElements("tag name", "option")
      textos_com  <- vapply(opt_com_dom,
                            function(o) str_squish(o$getElementText()[[1]]),
                            character(1))
      idx_com <- which(textos_com == str_squish(com_nom))
      if (!length(idx_com)) {
        warning("No encontré comuna '", com_nom,
                "' en región '", reg_nom,
                "', distrito '", dist_nom, "'. La salto.")
        return(tibble())
      }
      opt_com_dom[[idx_com[1]]]$clickElement()
      Sys.sleep(2)
      
      html_com <- remDr$getPageSource()[[1]]
      parse_tabla_candidatos(
        html_com,
        extra_cols = list(
          nivel    = "comuna",
          region   = reg_nom,
          distrito = dist_nom,
          comuna   = com_nom
        )
      )
    })
    
    res_comunas
  })
  
  bind_rows(res_region, res_distritos)
})

# --- 8. Unir nacional + región + comuna y guardar ----
res_todo <- bind_rows(res_nacional, res_reg_dist_com)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
file_out <- file.path("datos", paste0("presidencial_2025_nac_region_distrito_comuna_", ts, ".csv"))
write_csv(res_todo, file_out)
message("Archivo guardado: ", file_out)



  