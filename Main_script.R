# -------------------------------------------
# Proyecto: código main (RD fuzzy espejo Stata)
# Fecha de última actualización: 01/08/2025
# Autores: Juan Sebastián Lemos, Jhon Medina Claros
# -------------------------------------------

# ===============================================================
# 1) Librerías iniciales para exportar tablas en LaTeX
# ===============================================================
suppressPackageStartupMessages({
  library(optparse)
  library(readr)
  library(xtable)
})

# ---------------------------------------------------------------
# 1.1) Función para guardar la tabla en formato LaTeX
# ---------------------------------------------------------------
write_table_tex <- function(data, file_path, caption) {
  kable(data, format = "latex", booktabs = TRUE, caption = caption, 
        digits = 3, align = 'c', escape = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
    save_kable(file = file_path)
}


# ===============================================================
# 2) Librerías principales para manejo de datos y econometría
# ===============================================================
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(stringr); library(tidyr); library(tibble)
  library(rdrobust); library(modelsummary); library(broom)
  library(sandwich); library(lmtest); library(car); library(glue); library(readr)
})

# Opciones globales
options(dplyr.summarise.inform = FALSE)
options(stringsAsFactors = FALSE)
options(scipen = 999)


# ===============================================================
# 3) Definición de rutas
# ===============================================================
path <- "C:/Users/user/OneDrive - Universidad de los Andes/Archivos de Tomas Rodriguez Barraquer - 4. Datos"

# Bases en Stata
BasesS <- file.path(path, "2. Bases_stata")

# Resultados de regresiones RD
ResultadosRD <- file.path(path, "4. Resultados/2. Regresiones/1. Raw/4.RD")

# Crear directorio para guardar regresiones si no existe
dir.create(file.path(ResultadosRD, "regresiones"), recursive = TRUE, showWarnings = FALSE)


# ===============================================================
# 4) Funciones auxiliares (helpers)
# ===============================================================
require_vars <- function(df, vars){
  faltan <- setdiff(vars, names(df))
  if(length(faltan)) stop(sprintf("Faltan variables: %s", paste(faltan, collapse=", ")), call.=FALSE)
}

to_chr <- function(v){ 
  if(inherits(v,"haven_labelled")) v <- haven::zap_labels(v) 
  as.character(v) 
}

to_num <- function(v){ 
  if(inherits(v,"haven_labelled")) v <- haven::zap_labels(v) 
  suppressWarnings(as.numeric(as.character(v))) 
}


# ===============================================================
# 5) Importar y filtrar base principal (traducción de Stata a R)
# ===============================================================
ruta_base <- file.path(BasesS, "base_regresiones.dta")
stopifnot(file.exists(ruta_base))
df0 <- read_dta(ruta_base)

# Filtrar PERIODO >= 201510
PERIODO_num <- to_num(df0$PERIODO)
keep1 <- is.na(PERIODO_num) | PERIODO_num >= 201510
df1 <- df0[keep1, , drop = FALSE]

# Eliminar últimos == "19"
ps  <- to_chr(df1$periodo_s)
ult <- ifelse(!is.na(ps), stringr::str_sub(ps, 5), NA_character_)
keep2 <- is.na(ult) | ult != "19"
df2 <- df1[keep2, , drop = FALSE]

# Convertir ESTRATO a numérico
df2$ESTRATO <- to_num(to_chr(df2$ESTRATO))

# Reemplazar PMA = NA si PMA == 0
PMA_num <- to_num(df2$PMA)
PMA_num[PMA_num == 0] <- NA_real_
df2$PMA <- PMA_num

# Mantener solo PERIODO == CD_primeravez
PER  <- to_num(df2$PERIODO)
CDpv <- to_num(df2$CD_primeravez)
keep3 <- (!is.na(PER) & !is.na(CDpv) & PER == CDpv)
df3 <- df2[keep3, , drop = FALSE]

# Variable género (F=1, M=0)
GEN <- to_chr(df3$GENERO)
df3$genero <- NA_real_
df3$genero[GEN == "FEMENINO"]  <- 1
df3$genero[GEN == "MASCULINO"] <- 0

# Variable calendario (A=1, B=0)
if(!("calendar" %in% names(df3)) && "calendario" %in% names(df3)){
  cal <- to_chr(df3$calendario)
  df3$calendar <- NA_real_
  df3$calendar[cal == "A"] <- 1
  df3$calendar[cal == "B"] <- 0
  df3$calendar[cal == ""]  <- NA_real_
}

# Corrección censo/cupo
CEN <- to_num(df3$CENSUS_ENRL)
TOT <- to_num(df3$total_seccion)
CUP <- to_num(df3$CUPO)
retiros    <- CEN - TOT
menorcenso <- (retiros < 0)
menorcupo  <- ((CUP - TOT) < 0)

drop_idx <- (menorcenso & menorcupo)
keep_after <- !drop_idx
df4  <- df3[keep_after, , drop = FALSE]
CENs <- CEN[keep_after]
CUPs <- CUP[keep_after]
rep_idx <- menorcenso[keep_after] & !menorcupo[keep_after]
CENs[rep_idx] <- CUPs[rep_idx]
df4$CENSUS_ENRL <- CENs

# Variables de programa/carrera
df4$carrera_corregida <- ""
econ_num <- to_num(df4$econ)
df4$carrera_corregida[!is.na(econ_num) & econ_num == 1] <- "ECON"

if("CODIGO_PROGRAMA_1" %in% names(df4)){
  prog <- to_chr(df4$CODIGO_PROGRAMA_1)
  df4$carrera_corregida[!is.na(econ_num) & econ_num == 0] <- prog[!is.na(econ_num) & econ_num == 0]
}

lev <- sort(unique(df4$carrera_corregida[df4$carrera_corregida != ""]))
df4$cod_carrera <- match(df4$carrera_corregida, lev)

K <- length(lev)
if (K > 0){
  for(i in seq_len(K)){
    nm <- paste0("carr_", i)
    df4[[nm]] <- as.integer(df4$cod_carrera == i)
    df4[[nm]][is.na(df4$cod_carrera)] <- 0L
  }
}

# Base final para RD-FUZZY
df <- df4


# ===============================================================
# 6) Cargar base temporal
# ===============================================================
ruta_bt <- file.path(ResultadosRD, "base_temporal.dta")
stopifnot(file.exists(ruta_bt))
bt <- read_dta(ruta_bt)


# ===============================================================
# 7) Librerías adicionales para tablas en LaTeX
# ===============================================================
suppressPackageStartupMessages({
  library(optparse)
  library(knitr)       # Para kable
  library(kableExtra)  # Para dar estilo
})


# ===============================================================
# 8) Funciones para exportar tablas desde scripts
# ===============================================================

# Construcción de rutas
build_paths <- function() {
  list(
    Scripts = "C:/Users/user/Downloads/Stata proyecto/Tablas RD",  
    Output  = "C:/Users/user/Downloads/Stata proyecto/Tablas RD/Tablas latex"
  )
}

# Ejecutar un script de regresión por nombre
run_regression_script <- function(tabla_name, paths) {
  script_path <- file.path(paths$Scripts, paste0("tabla", tabla_name, ".R"))
  
  if (!file.exists(script_path)) {
    stop("No se encuentra el script ", script_path, call. = FALSE)
  }
  
  source(script_path)  
  cat("Tabla ", tabla_name, " ha sido generada y guardada en formato LaTeX.\n")
}

# Ejecutar todas las tablas o una específica
run_and_export <- function(tabla_name, paths) {
  if (tolower(tabla_name) == "all") {
    tabla_names <- c("1", "2", "3", "4", "5", "6", "7", 
                     "8", "9", "10", "11", "12", "13", 
                     "14", "15", "16", "17", "18")
  } else {
    tabla_names <- tabla_name
  }
  
  if (is.character(tabla_names)) {
    tabla_names <- list(tabla_names)
  }
  
  for (tabla in tabla_names) {
    message("Generando tabla ", tabla, "...")
    run_regression_script(tabla, paths)
  }
}


# ===============================================================
# 9) Ejecución principal
# ===============================================================
paths <- build_paths()

# Verificar directorio de salida
if (!dir.exists(paths$Output)) {
  dir.create(paths$Output, recursive = TRUE)
  cat("Directorio creado: ", paths$Output, "\n")
} else {
  cat("Directorio ya existe: ", paths$Output, "\n")
}


# ===============================================================
# 10) Elegir tabla a ejecutar
# ===============================================================

tabla_name <- "6"  
run_and_export(tabla_name, paths)

# Confirmar ruta de salida
cat("Las tablas han sido guardadas como archivos LaTeX en: ", paths$Output, "\n")


