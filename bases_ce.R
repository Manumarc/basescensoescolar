#====================================================================#
# Descargar las bases del Censo escolar para cada año que se indique #
#====================================================================#

descargar_censo <- function(anio,
                            out_base = "01 Bases",
                            verbose = TRUE) {
  
  # URL de arranque
  root_url = "https://escale.minedu.gob.pe/uee/-/document_library_display/GMv7/view/10481"
  
  # Arreglar URLs
  fix_url <- function(x) {
    
    if (is.na(x) || x == "") return(NA_character_)
    if (str_starts(x, "http")) x else paste0("https://escale.minedu.gob.pe", x)
    
  }
  
  safe_read <- function(u) {
    
    tryCatch(read_html(u), error = function(e) NULL)
    
  }
  
  vmsg <- function(...) if (verbose) message(...)
  
  # 1) Localizar el link de la carpeta del año en la página raíz
  vmsg("Leyendo índice de años...")
  root_pg <- safe_read(root_url)
  if (is.null(root_pg)) stop("No se pudo leer la página raíz: ", root_url)
  
  # Buscar <a> cuyo texto sea exactamente el año
  nodes_a <- root_pg %>% html_nodes("a")
  texts <- nodes_a %>% html_text(trim = TRUE)
  hrefs <- nodes_a %>% html_attr("href")
  
  # Seleccionar primer href cuyo texto coincide con el año
  idx <- which(texts == as.character(anio))
  if (length(idx) == 0) stop("No se encontró carpeta del año ", anio, " en la página raíz.")
  anio_href <- hrefs[idx[1]]
  anio_link <- fix_url(anio_href)
  vmsg("Carpeta del año encontrada: ", anio_link)
  
  # 2) Detectar cuántas páginas tiene
  anio_pg1 <- safe_read(anio_link)
  if (is.null(anio_pg1)) stop("No se pudo leer la página del año: ", anio_link)
  
  pager_hrefs <- anio_pg1 %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    discard(is.na)
  
  pages_detected <- pager_hrefs %>%
    str_extract("cur2=\\d+") %>%
    discard(is.na) %>%
    str_extract("\\d+") %>%
    as.integer() %>%
    unique()
  
  total_pages <- if (length(pages_detected) == 0) {
    
    1
    
  } else {
    
    max(pages_detected) + 1
    
  }
  
  vmsg("Páginas detectadas para ", anio, ": ", total_pages)
  
  # Construir URLs de todas las páginas del año
  page_urls <- if (total_pages == 1) {
    
    anio_link
    
  } else {
    
    c(anio_link,
      vapply(
        2:total_pages, function(p) {
          
          paste0(anio_link, "?_110_INSTANCE_GMv7_cur1=1&cur2=", p)
          
        }, 
        character(1)
      )
    )
    
  }
  
  # 3) Extraer enlaces hacia subpáginas (los items listados)
  extract_subpages <- function(page_url) {
    
    pg <- safe_read(page_url)
    if (is.null(pg)) return(character(0))
    h <- pg %>% html_nodes("a") %>% html_attr("href")
    h <- discard(h, is.na)
    # patrón robusto: enlaces con 'document_library_display' y que acaban en /<n> (id)
    subs <- h[str_detect(h, "document_library_display") & str_detect(h, "/view/.+?/\\d+$|/view/.+")]
    # completar
    subs <- unique(subs)
    subs <- vapply(subs, fix_url, character(1))
    subs[!is.na(subs)]
    
  }
  
  all_subpages <- page_urls %>%
    map(extract_subpages) %>%
    unlist() %>%
    unique()
  vmsg("Subpáginas encontradas: ", length(all_subpages))
  if (length(all_subpages) == 0) vmsg("No se encontraron subpáginas; revisar la estructura HTML")
  
  # 4) Para cada subpágina obtener el enlace real del .zip
  get_real_zip <- function(sub_url) {
    
    pg <- safe_read(sub_url)
    if (is.null(pg)) return(NA_character_)
    
    # A) Data-download-url (atributo en botones)
    btn <- pg %>% html_nodes("[data-download-url]") 
    if (length(btn) > 0) {
      
      dd <- html_attr(btn, "data-download-url")
      dd <- discard(dd, is.na)
      if (length(dd) > 0) return(fix_url(dd[1]))
      
    }
    
    # B) buscar hrefs que terminen en .zip
    hrefs <- pg %>% html_nodes("a") %>% html_attr("href")
    hrefs <- discard(hrefs, is.na)
    zips <- hrefs[str_detect(hrefs, "\\.zip($|\\?)")]
    if (length(zips) > 0) return(fix_url(zips[1]))
    
    # C) Buscar en bloques de texto (pre, div, p) enlaces "documents/..."
    textos <- pg %>% html_nodes("pre, p, div") %>% html_text(trim = TRUE)
    candidatos <- textos[str_detect(textos, "https?://escale\\.minedu\\.gob\\.pe/documents/")]
    if (length(candidatos) > 0) {
      
      found <- str_extract(candidatos[1], "https?://escale\\.minedu\\.gob\\.pe/documents/[A-Za-z0-9/\\-_.]+\\.zip")
      if (!is.na(found)) return(found)
      
    }
    
    # D) Buscar cualquier 'documents/' en hrefs
    doc_links <- hrefs[str_detect(hrefs, "/documents/")]
    if (length(doc_links) > 0) return(fix_url(doc_links[1]))
    
    return(NA_character_)
    
  }
  
  vmsg("Extrayendo enlaces reales de ZIP (esto puede demorar)...")
  zip_links_raw <- all_subpages %>% map_chr(get_real_zip)
  zip_links <- unique(discard(zip_links_raw, is.na))
  vmsg("ZIPs reales detectados: ", length(zip_links))
  
  # 5) Preparar carpeta de salida y descargar (evita volver a descargar si ya existe)
  out_dir <- file.path(out_base, as.character(anio))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  downloaded <- list()
  skipped <- list()
  
  for (lk in zip_links) {
    
    fname <- basename(lk)
    
    destfile <- file.path(out_dir, fname)
    
    if (file.exists(destfile) && file.info(destfile)$size > 0) {
      
      vmsg("  (saltando existente) ", fname)
      skipped <- c(skipped, destfile)
      next
      
    }
    
    vmsg("  Descargando: ", fname)
    
    tryCatch({
      
      download.file(lk, destfile, mode = "wb", quiet = TRUE)
      downloaded <- c(downloaded, destfile)
      
    }, error = function(e) {
      
      vmsg("    ERROR descarga: ", fname, " -> ", e$message)
      
    })
  }
  
  vmsg("\nResumen:")
  vmsg("  ZIPs detectados: ", length(zip_links))
  vmsg("  Descargados: ", length(downloaded))
  vmsg("  Saltados (existentes): ", length(skipped))
  
  return(
    list(
      year = anio, # Verificación del año descargado
      detected = zip_links, # Verificación de enlaces de descarga de .zip
      downloaded = downloaded,# Verificación de archivos descargados
      skipped = skipped # Verificación de archivos omitidos (porque ya estaban descargados)
    )
  )
}
