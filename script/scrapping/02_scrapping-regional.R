library(RSelenium)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(fs)

# ---- 0. Configuración ----
dir_create("datos")

loc <- locale(decimal_mark = ",", grouping_mark = ".")  # formato Servel

parse_tabla_candidatos <- function(html, extra_cols = list()) {
  doc  <- read_html(html)
  tabs <- html_elements(doc, "table")
  
  # buscar tabla con header "Candidatos"
  headers_list <- map(seq_along(tabs), \(i) {
    html_elements(tabs[[i]], "thead th") |> html_text2()
  })
  idx_cand <- which(vapply(headers_list, \(h) "Candidatos" %in% h, logical(1)))
  if (!length(idx_cand)) stop("No encontré tabla con 'Candidatos'.")
  
  tabla_cand <- html_table(
    tabs[[idx_cand[1]]],
    fill    = TRUE,
    convert = FALSE   # <-- MUY IMPORTANTE: todo queda como texto
  ) |> as_tibble()
  
  candidatos <- tabla_cand |>
    # filas que empiezan con número son las candidaturas
    filter(str_detect(Candidatos, "^\\d+\\s")) |>
    mutate(
      candidato  = str_remove(Candidatos, "^\\d+\\s+"),
      votos      = readr::parse_number(as.character(Votos), locale = loc),
      porcentaje = readr::parse_number(as.character(Porcentaje), locale = loc)
    ) |>
    select(candidato, votos, porcentaje)
  
  # añadir columnas extra (ej. region, circ, comuna...)
  if (length(extra_cols)) {
    for (nm in names(extra_cols)) {
      candidatos[[nm]] <- extra_cols[[nm]]
    }
  }
  
  candidatos
}


# ---- 1. Levantar navegador ----
rD <- rsDriver(browser = "firefox",
               port = 4367L, verbose = FALSE, check = FALSE)
remDr <- rD$client

# ---- 2. Navegar y seleccionar "División Electoral Chile" ----
remDr$navigate("https://elecciones.servel.cl")
Sys.sleep(5)

# botón izquierda "División Electoral Chile"
btn_div_electoral <- remDr$findElement(
  using = "css selector",
  "button[data-id='División Electoral Chile']"
)
btn_div_electoral$clickElement()
Sys.sleep(3)

# ---- 3. Obtener combo de Regiones ----
# Usamos el <div> que dice "Región" y tomamos el <select> que le sigue
sel_region <- remDr$findElement(
  using = "xpath",
  "//div[contains(@class,'text-filtros') and normalize-space()='Región']/following-sibling::select[1]"
)

# Sacamos TODO el innerHTML del <select> (todas las <option>)
sel_region_html <- sel_region$getElementAttribute("innerHTML")[[1]]

# Lo envolvemos en un <select> para que rvest lo pueda leer
library(rvest)
doc_sel <- read_html(paste0("<select>", sel_region_html, "</select>"))

options_nodes <- html_elements(doc_sel, "option")
reg_nombres <- html_text2(options_nodes)

# índices válidos (saltamos "Seleccionar")
idx_valid <- which(reg_nombres != "" & reg_nombres != "Seleccionar")

cat("Regiones detectadas:\n")
print(reg_nombres[idx_valid])

# ---- 4. Iterar por todas las Regiones y scrapear tabla ----
resultados_regiones <- purrr::map_dfr(idx_valid, function(dom_idx) {
  
  reg_nom <- reg_nombres[dom_idx]
  message("Scrapeando región: ", reg_nom)
  
  # Siempre volvemos a seleccionar el combo de REGIÓN con el mismo xpath
  sel_region <- remDr$findElement(
    using = "xpath",
    "//div[contains(@class,'text-filtros') and normalize-space()='Región']/following-sibling::select[1]"
  )
  opts_region <- sel_region$findChildElements(using = "tag name", "option")
  
  # Abrimos el combo (opcional, pero ayuda a que el click se registre bien)
  sel_region$clickElement()
  Sys.sleep(0.5)
  
  # Click en la opción dom_idx (2..17)
  opts_region[[dom_idx]]$clickElement()
  
  Sys.sleep(4)  # esperar a que se actualice la tabla
  
  html <- remDr$getPageSource()[[1]]
  
  parse_tabla_candidatos(html, extra_cols = list(region = reg_nom))
})

resultados_regiones


# ---- 5. Guardar resultado con timestamp ----
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
file_out <- file.path("datos", paste0("candidatos_division_electoral_region_", ts, ".csv"))
write_csv(resultados_regiones, file_out)

message("Archivo guardado: ", file_out)

# ---- 6. Cerrar Selenium cuando ya no se use ----
# remDr$close()
# rD$server$stop()
