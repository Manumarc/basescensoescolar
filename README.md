# Paquetes requeridos

- library(rvest)
- library(stringr) 
- library(purrr) 
- library(dplyr) 

# Bases de datos del Censo escolar

Funciones para descargar las bases del censo escolar según año indicado

## Llamar a las funciones 

Para llamar a la función desarrollada se debe usar el siguiente código:

```
devtools::source_url("https://https://raw.githubusercontent.com/Manumarc/basescensoescolar/refs/heads/main/bases_ce.R")

```
## Descargar bases

La función "descargar_censo" permite descargar las bases del Censo escolar desde el año 2004 hasta el último año recogido. Los argumentos son la definición del año (anio), la carpeta donde se hara la descarga (out_base) que por default se ha nombrado "01 Bases" (si no la encuentra, la función crea la carpeta) y la opción de visibilizar los mensajes en pantalla mientras trabaja la función (verbose).

```
# descargar_censo <- function(anio,
#                           out_base = "01 Bases",
#                           verbose = TRUE)

descargar_censo(2024, "01 Bases")

```
