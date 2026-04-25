#====================================================================#
# Descargar las bases del Censo escolar para cada año que se indique #
#====================================================================#

descargar_censo <- function(anio,
                            out_base = "01 Bases",
                            verbose = TRUE) {
  
  # URL de arranque del archivos del censo escolar por años 
  root_url = "https://escale.minedu.gob.pe/uee/-/document_library_display/GMv7/view/10481"
  
  # Arreglar URLs
  fix_url <- function(x) {
    
    if (is.na(x) || x == "") return(NA_character_)
    if (str_starts(x, "http")) x else paste0("https://escale.minedu.gob.pe", x)
    
  }
  
  safe_read <- function(u) {

    tryCatch({
      # Usar httr para manejar sesiones y user-agent
      resp <- httr::GET(u, httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
      httr::stop_for_status(resp)
      read_html(resp)
    }, error = function(e) {
      vmsg("  Advertencia al leer: ", u, " -> ", e$message)
      NULL
    })

  }
  
  vmsg <- function(...) if (verbose) message(...)

  # Condicional para agregar varios años, iterar sobre cada uno y devolver lista de resultados
  
  if (length(anio) > 1) {

    resultados <- lapply(anio, function(a) {
      vmsg("\n========== Año: ", a, " ==========")
      descargar_censo(anio = a, out_base = out_base, verbose = verbose)
    })
    names(resultados) <- as.character(anio)
    return(resultados)

  }

  # Localizar el link de la carpeta del año en la página raíz # 
  #-----------------------------------------------------------#
  
  vmsg("Leyendo índice de años...")
  root_pg <- safe_read(root_url)
  if (is.null(root_pg)) stop("No se pudo leer la página raíz: ", root_url)
  
  # Buscar <a> con texto que sea exactamente el año
  nodes_a <- root_pg %>% 
    html_nodes("a")
  
  texts <- nodes_a %>% 
    html_text(trim = TRUE)
  
  hrefs <- nodes_a %>% 
    html_attr("href")
  
  # Seleccionar primer "href" con texto que coincide con el año
  idx <- which(texts == as.character(anio))
  if (length(idx) == 0) stop("No se encontró carpeta del año ", anio, " en la página raíz.")
  anio_href <- hrefs[idx[1]]
  anio_link <- fix_url(anio_href)
  vmsg("Carpeta del año encontrada: ", anio_link)
  
  # Detectar cuántas páginas tiene # 
  #--------------------------------#
  
  anio_pg1 <- safe_read(anio_link)
  if (is.null(anio_pg1)) stop("No se pudo leer la página del año: ", anio_link)
  
  pager_hrefs_raw <- anio_pg1 %>%
    html_nodes("a") %>%
    html_attr("href")
  pager_hrefs <- pager_hrefs_raw[!is.na(pager_hrefs_raw)]

  cur_matches <- str_extract(pager_hrefs, "cur2=\\d+")
  cur_matches <- cur_matches[!is.na(cur_matches)]
  pages_detected <- unique(as.integer(str_extract(cur_matches, "\\d+")))
  
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
  
  # Extraer enlaces hacia subpáginas (los items listados) #
  #-------------------------------------------------------#
  
  extract_subpages <- function(page_url) {
    
    pg <- safe_read(page_url)
    if (is.null(pg)) return(character(0))
    h <- pg %>% html_nodes("a") %>% html_attr("href")
    h <- h[!is.na(h)]
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
  
  # Para cada subpágina obtener el enlace real del .zip #
  #-----------------------------------------------------#
  
  get_real_zip <- function(sub_url) {
    
    pg <- safe_read(sub_url)
    if (is.null(pg)) return(NA_character_)
    
    # Data-download-url (atributo en botones)
    btn <- pg %>% html_nodes("[data-download-url]") 
    if (length(btn) > 0) {
      
      dd <- html_attr(btn, "data-download-url")
      dd <- dd[!is.na(dd)]
      if (length(dd) > 0) return(fix_url(dd[1]))
      
    }
    
    # Buscar hrefs que terminen en .zip
    hrefs <- pg %>% html_nodes("a") %>% html_attr("href")
    
    hrefs <- hrefs[!is.na(hrefs)]
    
    zips <- hrefs[str_detect(hrefs, "\\.zip($|\\?)")]
    if (length(zips) > 0) return(fix_url(zips[1]))
    
    # Buscar en bloques de texto (pre, div, p) enlaces "documents/..."
    textos <- pg %>% 
      html_nodes("pre, p, div") %>% 
      html_text(trim = TRUE)
    
    candidatos <- textos[str_detect(textos, "https?://escale\\.minedu\\.gob\\.pe/documents/")]
    if (length(candidatos) > 0) {
      
      found <- str_extract(candidatos[1], "https?://escale\\.minedu\\.gob\\.pe/documents/[A-Za-z0-9/\\-_.]+\\.zip")
      if (!is.na(found)) return(found)
      
    }
    
    # Buscar cualquier 'documents/' en hrefs
    doc_links <- hrefs[str_detect(hrefs, "/documents/")]
    if (length(doc_links) > 0) return(fix_url(doc_links[1]))
    
    return(NA_character_)
    
  }
  
  vmsg("Extrayendo enlaces reales de ZIP (esto puede demorar)...")
  
  zip_links_raw <- all_subpages %>% 
    map_chr(get_real_zip)
  
  zip_links <- unique(zip_links_raw[!is.na(zip_links_raw)])
  
  vmsg("ZIPs reales detectados: ", length(zip_links))
  
  # Preparar carpeta de salida y descargar #
  #----------------------------------------#
  
  out_dir <- file.path(out_base, as.character(anio))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  downloaded <- list()
  skipped <- list()
  extracted <- list()

  for (lk in zip_links) {

    fname <- basename(lk)
    destfile <- file.path(out_dir, fname)

    if (file.exists(destfile) && file.info(destfile)$size > 0) {

      vmsg("  (saltando existente) ", fname)
      skipped <- c(skipped, destfile)

    } else {

      vmsg("  Descargando: ", fname)

      tryCatch({

        download.file(lk, destfile, mode = "wb", quiet = TRUE)
        downloaded <- c(downloaded, destfile)

      }, error = function(e) {

        vmsg("    ERROR descarga: ", fname, " -> ", e$message)

      })

    }

    # Si es ZIP: descomprimir y renombrar. Si es PDF solo renombrar con año #
    #-----------------------------------------------------------------------#
    
    if (file.exists(destfile) && file.info(destfile)$size > 0) {

      ext_archivo <- tolower(tools::file_ext(fname))

      if (ext_archivo == "zip") {

        vmsg("  Descomprimiendo: ", fname)

        tryCatch({

          # Carpeta temporal para extraer
          tmp_extract <- file.path(out_dir, paste0("_tmp_", tools::file_path_sans_ext(fname)))
          dir.create(tmp_extract, recursive = TRUE, showWarnings = FALSE)

          unzip(destfile, exdir = tmp_extract)

          # Listar todos los archivos extraídos (recursivo por si hay subcarpetas)
          archivos_extraidos <- list.files(tmp_extract, recursive = TRUE, full.names = TRUE)

          for (arch in archivos_extraidos) {

            nombre_original <- basename(arch)
            extension <- tools::file_ext(nombre_original)
            nombre_sin_ext <- tools::file_path_sans_ext(nombre_original)

            # Agregar el año al nombre solo si no lo tiene ya
            if (!grepl(as.character(anio), nombre_sin_ext)) {
              nuevo_nombre <- paste0(nombre_sin_ext, "_", anio, ".", extension)
            } else {
              nuevo_nombre <- nombre_original
            }

            destino_final <- file.path(out_dir, nuevo_nombre)

            file.copy(arch, destino_final, overwrite = TRUE)
            extracted <- c(extracted, destino_final)
            vmsg("    -> ", nuevo_nombre)

          }

          # Eliminar carpeta temporal
          unlink(tmp_extract, recursive = TRUE)

          # Eliminar el ZIP original (ya fue descomprimido)
          file.remove(destfile)

      }, error = function(e) {

        vmsg("    ERROR al descomprimir: ", fname, " -> ", e$message)

      })

      } else {

        # No es ZIP (PDF, etc.): solo renombrar con el año y conservar
        nombre_sin_ext <- tools::file_path_sans_ext(fname)

        if (!grepl(as.character(anio), nombre_sin_ext)) {
          nuevo_nombre <- paste0(nombre_sin_ext, "_", anio, ".", ext_archivo)
          nuevo_destfile <- file.path(out_dir, nuevo_nombre)
          file.rename(destfile, nuevo_destfile)
          vmsg("  Conservado (no ZIP): ", nuevo_nombre)
          extracted <- c(extracted, nuevo_destfile)
        } else {
          vmsg("  Conservado (no ZIP): ", fname)
          extracted <- c(extracted, destfile)
        }

      }

    }

  }

  vmsg("\nResumen:")
  vmsg("  ZIPs detectados: ", length(zip_links))
  vmsg("  Descargados: ", length(downloaded))
  vmsg("  Saltados (ya existían): ", length(skipped))
  vmsg("  Archivos extraídos: ", length(extracted))

  return(
    list(
      year = anio,
      detected = zip_links,
      downloaded = downloaded,
      skipped = skipped,
      extracted = extracted
    )
  )
}
