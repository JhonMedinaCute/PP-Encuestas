# -------------------------------------------
# Proyecto: Análisis de encuestas
# Fecha de última actualización: 01/08/2025
# Autores: Juan Sebastián Lemos, Jhon Medina Claros
# -------------------------------------------
# -------------------------------------------
# Proyecto: Análisis de encuestas
# Fecha de última actualización: 01/08/2025
# Autores: Juan Sebastián Lemos, Jhon Medina Claros
# -------------------------------------------


# ===============================================================
# 1) Librerías
# ===============================================================

# 1.1) Librerías para exportar tablas en LaTeX
suppressPackageStartupMessages({
  library(optparse)
  library(readr)
  library(xtable)
  library(knitr)
  library(kableExtra)
})

# 1.2) Librerías principales para manejo de datos y econometría
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(stringr); library(tidyr); library(tibble)
  library(rdrobust); library(modelsummary); library(broom)
  library(sandwich); library(lmtest); library(car); library(glue)
  library(readxl); library(ggplot2); library(skimr); library(GGally)
})

# ===============================================================
# 2) Opciones globales
# ===============================================================
options(
  dplyr.summarise.inform = FALSE,
  stringsAsFactors = FALSE,
  scipen = 999
)


# ===============================================================
# 3) Funciones auxiliares
# ===============================================================

# 3.1) Guardar tabla en formato LaTeX
write_table_tex <- function(data, file_path, caption) {
  kable(data, format = "latex", booktabs = TRUE, caption = caption,
        digits = 3, align = 'c', escape = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%
    save_kable(file = file_path)
}

# 3.2) Verificar variables requeridas
require_vars <- function(df, vars){
  faltan <- setdiff(vars, names(df))
  if(length(faltan)) stop(sprintf("Faltan variables: %s", paste(faltan, collapse=", ")), call.=FALSE)
}

# 3.3) Conversión de variables
to_chr <- function(v){ 
  if(inherits(v,"haven_labelled")) v <- haven::zap_labels(v) 
  as.character(v) 
}

to_num <- function(v){ 
  if(inherits(v,"haven_labelled")) v <- haven::zap_labels(v) 
  suppressWarnings(as.numeric(as.character(v))) 
}


# ===============================================================
# 4) Definición de rutas
# ===============================================================
base_path <- "C:/Users/user/OneDrive - Universidad de los Andes/Archivos de Tomas Rodriguez Barraquer - 4. Datos"

# Subcarpetas
BasesS      <- file.path(base_path, "2. Bases_stata")
ResultadosRD <- file.path(base_path, "4. Resultados/2. Regresiones/1. Raw/4.RD")
RawDataPath  <- file.path(base_path, "1. Bases_raw/centro_evaluacion/")

# Crear directorio de resultados si no existe
dir.create(file.path(ResultadosRD, "regresiones"), recursive = TRUE, showWarnings = FALSE)


# ===============================================================
# 5) Carga de bases
# ===============================================================
dfcruce   <- read_excel(file.path(RawDataPath, "datos_combinados_encuestas.xlsx"))

dfprofes <- read_dta("C:/Users/user/OneDrive - Universidad de los andes/Archivos de Tomas Rodriguez Barraquer - 4. Datos/2. Bases_stata/docentes_cursos_f.dta")
summary(dfprofes)
skim(dfprofes)

print(dfprofes$ID_PERSONA)  # Muestra todos los valores de la columna ID_PERSONA



ruta_base <- file.path(BasesS, "base_regresiones.dta")
stopifnot(file.exists(ruta_base))
df0 <- read_dta(ruta_base)


# ===============================================================
# 6) Limpieza y transformación de datos
# ===============================================================

# 6.1) Filtrar PERIODO >= 201510
PERIODO_num <- to_num(df0$PERIODO)
df1 <- df0[is.na(PERIODO_num) | PERIODO_num >= 201720, , drop = FALSE]

# 6.2) Eliminar últimos == "19"
ps  <- to_chr(df1$periodo_s)
ult <- ifelse(!is.na(ps), str_sub(ps, 5), NA_character_)
df2 <- df1[is.na(ult) | ult != "19", , drop = FALSE]

# 6.3) Convertir ESTRATO a numérico
df2$ESTRATO <- to_num(to_chr(df2$ESTRATO))

# 6.4) Reemplazar PMA = NA si PMA == 0
PMA_num <- to_num(df2$PMA)
PMA_num[PMA_num == 0] <- NA_real_
df2$PMA <- PMA_num

# 6.5) Mantener solo PERIODO == CD_primeravez
PER  <- to_num(df2$PERIODO)
CDpv <- to_num(df2$CD_primeravez)
df3 <- df2[!is.na(PER) & !is.na(CDpv) & PER == CDpv, , drop = FALSE]

# 6.6) Variable género (F=1, M=0)
GEN <- to_chr(df3$GENERO)
df3$genero <- NA_real_
df3$genero[GEN == "FEMENINO"]  <- 1
df3$genero[GEN == "MASCULINO"] <- 0

# 6.7) Variable calendario (A=1, B=0)
if(!("calendar" %in% names(df3)) && "calendario" %in% names(df3)){
  cal <- to_chr(df3$calendario)
  df3$calendar <- NA_real_
  df3$calendar[cal == "A"] <- 1
  df3$calendar[cal == "B"] <- 0
  df3$calendar[cal == ""]  <- NA_real_
}

# 6.8) Corrección censo/cupo
CEN <- to_num(df3$CENSUS_ENRL)
TOT <- to_num(df3$total_seccion)
CUP <- to_num(df3$CUPO)

retiros    <- CEN - TOT
menorcenso <- (retiros < 0)
menorcupo  <- ((CUP - TOT) < 0)

drop_idx <- (menorcenso & menorcupo)
df4  <- df3[!drop_idx, , drop = FALSE]

# Ajustar censo si corresponde
CENs <- CEN[!drop_idx]
CUPs <- CUP[!drop_idx]
rep_idx <- menorcenso[!drop_idx] & !menorcupo[!drop_idx]
CENs[rep_idx] <- CUPs[rep_idx]
df4$CENSUS_ENRL <- CENs

# 6.9) Variables de programa/carrera
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
# 7) Exploración inicial
# ===============================================================

skim(df0)
skim(df2)
skim(dfcruce)
summary(df0)

View(dfcruce)

# Número de CRN únicos
n_distinct(dfcruce$crn)

# Frecuencia de cada CRN
crn_freq <- dfcruce %>%
  count(crn, sort = TRUE)

# Periodos distintos por CRN
crn_periodo <- dfcruce %>%
  group_by(crn) %>%
  summarise(n_periodos = n_distinct(periodo),
            total_registros = n())

# Número de docentes únicos
n_distinct(dfcruce$identificacion_docente)

# Cursos impartidos por docente
docente_cursos <- dfcruce %>%
  count(identificacion_docente, sort = TRUE)

# Promedio de cursos por docente
mean(docente_cursos$n)

# CRN únicos por periodo
crn_por_periodo <- dfcruce %>%
  group_by(periodo) %>%
  summarise(crn_unicos = n_distinct(crn),
            registros = n())

# Cursos de interés
cursos_interes <- dfcruce %>%
  filter(curso_ua %in% c("MATE1201", "MATE1201C", "MATE1203", "MATE1203C", "MATE1204"))

# Frecuencia por curso
cursos_freq <- cursos_interes %>%
  count(curso_ua, sort = TRUE)

# Repetición por periodo
cursos_periodo <- cursos_interes %>%
  group_by(periodo, curso_ua) %>%
  summarise(n_secciones = n_distinct(crn),
            docentes = n_distinct(identificacion_docente),
            .groups = "drop")





library(dplyr)
library(stringr)

dfcruce <- dfcruce %>%
  rename(
    MATERIA = curso_ua,
    CRN = crn,
    PERIODO = periodo
  )

df <- df %>%
  mutate(MATERIA = str_replace_all(MATERIA, "-", ""))

df$PERIODO <- as.character(df$PERIODO)
df$CRN <- as.character(df$CRN)
df$MATERIA <- as.character(df$MATERIA)

dfcruce$PERIODO <- as.character(dfcruce$PERIODO)
dfcruce$CRN <- as.character(dfcruce$CRN)
dfcruce$MATERIA <- as.character(dfcruce$MATERIA)


# ===============================================================
# 8) Cruce df0 (estudiantes) × dfcruce (encuestas por profe/sección)
#     Llave: (periodo, crn) — conserva llaves de ambos lados
# ===============================================================


# Cargar librerías necesarias
suppressPackageStartupMessages({
  library(dplyr)
  library(writexl)
})

# --- 1. Realizar el JOIN entre las dos bases por las tres columnas claves ---
df_unido <- df %>%
  left_join(
    dfcruce,
    by = c("PERIODO" = "PERIODO", 
           "CRN" = "CRN", 
           "MATERIA" = "MATERIA"),
    relationship = "many-to-many"
  )

summary(df_unido)

skim(df_unido)





# --- 2. Verificar cuántos estudiantes no tienen encuesta asociada --------------
estudiantes_sin_encuesta <- df_unido %>%
  filter(is.na(identificacion_docente)) %>%
  nrow()

cat("Número de estudiantes sin encuesta asociada:", estudiantes_sin_encuesta, "\n")



##-------------------------GENERO-----------------------------------

# Crear una nueva columna 'genero' con valores NA inicialmente
df_unido$generoprofesores <- NA

# Asignar género según la identificación docente

df_unido$generoprofesores[df_unido$identificacion_docente == 80873504] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 70045815] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1010161525] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1010182985] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1013585751] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1016096219] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1017123691] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1020726856] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1020737718] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 1024489209] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 1026252241] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1032473417] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1056708737] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 11204883] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 1233691384] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 19091298] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 19293359] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 19439284] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 19450145] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 19485626] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 19494664] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 2993600] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 319330] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 326898] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 35330464] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 384185] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 398132] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 399357] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 400118] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 408768] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 41467039] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 41485060] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 41649848] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 491693] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 513464] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 52042025] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 52455226] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 53070605] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 53905558] <- 0
df_unido$generoprofesores[df_unido$identificacion_docente == 5964026] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 6677998] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 70045815] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79143758] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79148320] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79418982] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79484184] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79830356] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79862039] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79939996] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 79961817] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80047261] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80068707] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80135063] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80192443] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80201487] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80425776] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80504006] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80794221] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80831462] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 80857443] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 852663] <- NA
df_unido$generoprofesores[df_unido$identificacion_docente == 92529303] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 9431154] <- 1
df_unido$generoprofesores[df_unido$identificacion_docente == 98705918] <- 1

# Verificar que la columna 'generoprofesores' fue creada correctamente
head(df_unido[, c("identificacion_docente", "generoprofesores")])




# 📦 Cargar paquetes
library(dplyr)
library(openxlsx)
library(skimr)
library(stringr)

#  Cargar base dfcursos_MATE1201
dfcursos_MATE1201 <- read.xlsx("C:/Users/user/OneDrive - Universidad de los andes/Documentos/Octavo semestre/Stata proyecto/Acceso python/dfcursos_MATE1201.xlsx")

#  Limpiar y renombrar variables
dfcursos_MATE1201 <- dfcursos_MATE1201 %>%
  mutate(
    CODIGO = as.character(CODIGO_ESTUDIANTE)
  ) %>%
  rename(
    CRNPRECALCULO = CRN,
    FACULTADPRECALCULO = DESCRIPCION_FACULTAD_PROGRAMA_1,
    PERIODOPRECALCULO = PERIODO
  ) %>%
  select(CODIGO, CRNPRECALCULO, FACULTADPRECALCULO, PERIODOPRECALCULO)

#  Asegurar que df_unido tenga el formato correcto
df_unido <- df_unido %>%
  mutate(
    CODIGO = as.character(CODIGO_ESTUDIANTE)
  )

#  Limpieza adicional (espacios, ceros, formatos)
df_unido <- df_unido %>%
  mutate(CODIGO = str_trim(CODIGO))

dfcursos_MATE1201 <- dfcursos_MATE1201 %>%
  mutate(CODIGO = str_trim(CODIGO))

# Verificar coincidencias por CODIGO
common_codigos <- inner_join(
  df_unido %>% distinct(CODIGO),
  dfcursos_MATE1201 %>% distinct(CODIGO),
  by = "CODIGO"
)

cat("Número de coincidencias por CODIGO:", nrow(common_codigos), "\n")

#  Unir bases SOLO por CODIGO
df_final <- df_unido %>%
  left_join(dfcursos_MATE1201, by = "CODIGO")

# Verificar resultados
skim(dfcursos_MATE1201)
colnames(df_final)

# Chequear filas con datos de precálculo no vacíos
summary(is.na(df_final$CRNPRECALCULO))
summary(is.na(df_final$FACULTADPRECALCULO))
summary(is.na(df_final$PERIODOPRECALCULO))


library(dplyr)

# Asegurarte de que las variables tengan el mismo tipo
df_final <- df_final %>%
  mutate(
    CRNPRECALCULO = as.character(CRNPRECALCULO),
    PERIODOPRECALCULO = as.character(PERIODOPRECALCULO)
  )

dfcruce <- dfcruce %>%
  mutate(
    CRN = as.character(CRN),
    PERIODO = as.character(PERIODO)
  )

# Hacer el merge por las variables especificadas
df_merge <- df_final %>%
  left_join(dfcruce, by = c("CRNPRECALCULO" = "CRN",
                            "PERIODOPRECALCULO" = "PERIODO"))

# Verificar resultado
dim(df_merge)
skimr::skim(df_merge)

colnames(df_merge)
library(dplyr)

df_merge <- df_merge %>%
  rename(
    IDENTIFICACION_DOCENTEDIIFERENCIAL = identificacion_docente.x,
    TIPOCURSODIFERENCIAL = tipoCurso.x,
    NASIGDIFERENCIAL = nAsig.x,
    NINSCDIFERENCIAL = nInsc.x,
    PORCRTADIFERENCIAL = porcRta.x,
    NDIFERENCIAL = N.x,
    PUNTAJEDIFERENCIAL = puntaje.x,
    SECCION_UA = seccion_ua.x,
    GENEROPROFESORESDIFERENCIAL = generoprofesores,
    CODIGO = CODIGO,
    CRNPRECALCULO = CRNPRECALCULO,
    FACULTADPRECALCULO = FACULTADPRECALCULO,
    PERIODOPRECALCULO = PERIODOPRECALCULO,
    CURSECPRECALCULO = curSec.y,
    IDENTIFICACION_DOCENTEPRECALCULO = identificacion_docente.y,
    TIPOCURSOPRECALCULO = tipoCurso.y,
    NASIGPRECALCULO = nAsig.y,
    NLNSCPRECALCULO = nInsc.y,
    PORCRTAPRECALCULO = porcRta.y,
    NPRECALCULO = N.y,
    PUNTAJEPRECALCULO = puntaje.y,
    MATERIAPRECALCULO = MATERIA.y,
    SECCION_UAPRECALCULO = seccion_ua.y
  )


unique(df_merge$IDENTIFICACION_DOCENTEPRECALCULO)

# Crear una nueva columna 'GENEROPROFESORESPRECALCULO' con valores NA inicialmente
df_merge$GENEROPROFESORESPRECALCULO <- NA


# =====================================
# Asignar género según identificación docente de PRECALCULO
# =====================================

df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1024565577] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1125470712] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1022390216] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1031178186] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1032495886] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1033762543] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1073676893] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1060656552] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 72053439] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1022443452] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79426561] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1115094348] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1023934169] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1193223941] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1140901124] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1193034715] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1233890949] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1010235971] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80212517] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1026273524] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1026303253] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1088300310] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1136884437] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 167157] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1094960683] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1032471713] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80149776] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80096095] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 94449678] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 12745822] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1061704887] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 7429274] <- NA
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1020828212] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1020821596] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1020820750] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1020786351] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1019102153] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1018513564] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1018513313] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1018500267] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1018470338] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1018440268] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1018439375] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1016106125] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1015473637] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1014269883] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1013680655] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1013657697] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1010229473] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1010220799] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1010074602] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1005340636] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1003740434] <- NA
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1000723996] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80873504] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 70045815] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1010161525] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1010182985] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1013585751] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1016096219] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1017123691] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1020726856] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1020737718] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1024489209] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1026252241] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1032473417] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1056708737] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 11204883] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 1233691384] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 19091298] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 19293359] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 19439284] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 19450145] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 19485626] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 19494664] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 2993600] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 319330] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 326898] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 35330464] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 384185] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 398132] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 399357] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 400118] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 408768] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 41467039] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 41485060] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 41649848] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 491693] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 513464] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 52042025] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 52455226] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 53070605] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 53905558] <- 0
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 5964026] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 6677998] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 70045815] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79143758] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79148320] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79418982] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79484184] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79830356] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79862039] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79939996] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 79961817] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80047261] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80068707] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80135063] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80201487] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80504006] <- 1
df_merge$GENEROPROFESORESPRECALCULO[df_merge$IDENTIFICACION_DOCENTEPRECALCULO == 80857443] <- 0




colnames(df_merge)

library(dplyr)

# 1. Renombrar las llaves en df_promedios2
df_promedios2 <- df_promedios2 %>%
  rename(
    PERIODOPRECALCULO = PERIODO,
    IDENTIFICACION_DOCENTEPRECALCULO = identificacion_docente
  )

# 2. Seleccionar solo la variable necesaria + llaves
df_promedios2_sel <- df_promedios2 %>%
  select(PERIODOPRECALCULO, IDENTIFICACION_DOCENTEPRECALCULO, mm_prev_flexible)

# 3. Hacer el merge (left join)
df_final <- left_join(
  df_merge,
  df_promedios2_sel,
  by = c("PERIODOPRECALCULO", "IDENTIFICACION_DOCENTEPRECALCULO")
)

colnames(df_final)



library(dplyr)

library(dplyr)

q1 <- 146.8446
q2 <- 154.3900

df_final <- df_final %>%
  mutate(tercil_mm = case_when(
    is.na(mm_prev_flexible)            ~ NA_character_,
    mm_prev_flexible <= q1             ~ "mbajo",
    mm_prev_flexible >  q1 & mm_prev_flexible <= q2 ~ "mmediano",
    mm_prev_flexible >  q2             ~ "malto"
  ))

mbajo    <- df_final %>% filter(tercil_mm == "mbajo")
mmediano <- df_final %>% filter(tercil_mm == "mmediano")
malto    <- df_final %>% filter(tercil_mm == "malto")

# Chequeo rápido
table(df_final$tercil_mm, useNA = "ifany")


library(dplyr)

# 1) Asegurar que la variable Precalculo exista y normalizarla a numérica (0/1)
stopifnot("Precalculo" %in% names(df_final))

df_final_norm <- df_final %>%
  mutate(
    Precalculo_num = suppressWarnings(as.numeric(Precalculo))
  )

# 2) Contrafactuales = observaciones con Precalculo == 0
df_contrafactuales <- df_final_norm %>%
  filter(!is.na(Precalculo_num) & Precalculo_num == 0) %>%
  select(-Precalculo_num)  # opcional: quitar la columna auxiliar

library(dplyr)


# Pequeño helper para agregar contrafactuales y marcar su origen
agrega_contraf <- function(df_tercil, df_contra) {
  bind_rows(
    df_tercil  %>% mutate(es_contrafactual = 0L),
    df_contra  %>% mutate(es_contrafactual = 1L)
  ) %>%
    # Evitar filas duplicadas (si no tienes una llave única, usa distinct por todas las columnas)
    distinct()
  # Si tienes una llave clara, por ej. LLAVE_TERM_PIDM_CRN, usa:
  # distinct(LLAVE_TERM_PIDM_CRN, .keep_all = TRUE)
}

mbajo_all    <- agrega_contraf(mbajo,    df_contrafactuales)
mmediano_all <- agrega_contraf(mmediano, df_contrafactuales)
malto_all    <- agrega_contraf(malto,    df_contrafactuales)

# Chequeos rápidos
cat("mbajo_all -> total:",    nrow(mbajo_all),    " | contrafactuales:", sum(mbajo_all$es_contrafactual, na.rm = TRUE), "\n")
cat("mmediano_all -> total:", nrow(mmediano_all), " | contrafactuales:", sum(mmediano_all$es_contrafactual, na.rm = TRUE), "\n")
cat("malto_all -> total:",    nrow(malto_all),    " | contrafactuales:", sum(malto_all$es_contrafactual, na.rm = TRUE), "\n")


colnames(malto_all)


# ==================== RD: Probabilidad de reprobar (adaptado a tu caso) ====================
library(dplyr)
library(tibble)
library(rdrobust)
library(haven)

# ---- helpers ----
to_num <- function(v){
  if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
  if (is.factor(v)) v <- as.character(v)
  suppressWarnings(as.numeric(v))
}
drop_zerovar <- function(M){
  if (is.null(M)) return(NULL)
  M <- as.matrix(M)
  keep <- apply(M, 2, function(v) any(!is.na(v)) && stats::var(v, na.rm=TRUE) > 0)
  if (!any(keep)) return(NULL)
  M[, keep, drop=FALSE]
}
mk_dummies <- function(df, prefix){
  cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  if (!length(cols)) return(NULL)
  ord <- suppressWarnings(as.integer(sub(paste0("^", prefix, "_?"), "", cols)))
  ord[is.na(ord)] <- Inf
  cols <- cols[order(ord, cols)]
  M <- as.matrix(data.frame(lapply(df[, cols, drop=FALSE], to_num)))
  M <- drop_zerovar(M)
  if (is.null(M)) return(NULL)
  if (ncol(M) >= 2) M <- M[, -1, drop=FALSE]  # quitar una base
  M
}
get_hr <- function(rd){
  if (!is.null(rd$h))  c(hl = rd$h[1],  hr = rd$h[2]) else
    if (!is.null(rd$bws)) c(hl = rd$bws[1], hr = rd$bws[2]) else
      stop("No encontré h_l/h_r.")
}
get_effN <- function(rd){
  if (!is.null(rd$Nh))  c(Nl = rd$Nh[1],  Nr = rd$Nh[2]) else
    if (!is.null(rd$N_h)) c(Nl = rd$N_h[1], Nr = rd$N_h[2]) else
      c(Nl = NA_real_, Nr = NA_real_)
}
mean_in_window <- function(y, x, c0, hr){
  win <- !is.na(y) & !is.na(x) & x >= (c0 - hr["hl"]) & x <= (c0 + hr["hr"])
  mean(y[win], na.rm = TRUE)
}
run_one <- function(y, x, z = NULL, covs = NULL, c0 = 79){
  try1 <- try(
    rdrobust(y=y, x=x, c=c0, fuzzy=z, covs=covs,
             kernel="triangular", p=1, q=2, bwselect="mserd",
             vce="hc0", masspoints="check", stdvars=TRUE, all=TRUE),
    silent = TRUE
  )
  if (!inherits(try1, "try-error")) return(try1)
  rdrobust(y=y, x=x, c=c0, fuzzy=z, covs=covs,
           kernel="triangular", p=1, q=2, bwselect="mserd",
           vce="hc0", masspoints="adjust", stdvars=TRUE, all=TRUE)
}
fit_model <- function(y, x, z, covs = NULL, c0 = 79){
  if (is.null(covs)) {
    mask <- complete.cases(y, x, z); CC <- NULL
  } else {
    covs <- drop_zerovar(covs)
    if (is.null(covs)) { mask <- complete.cases(y, x, z); CC <- NULL } else {
      mask <- complete.cases(cbind(y, x, z, covs))
      CC <- as.matrix(covs[mask, , drop=FALSE])
    }
  }
  yy <- y[mask]; xx <- x[mask]; zz <- z[mask]
  rd <- run_one(yy, xx, zz, covs = CC, c0 = c0)
  hr <- get_hr(rd); N <- get_effN(rd)
  list(
    rd   = rd, mask = mask,
    hl   = as.numeric(hr["hl"]), hr = as.numeric(hr["hr"]),
    Nl   = as.numeric(N["Nl"]),  Nr = as.numeric(N["Nr"]),
    ybar = mean_in_window(yy, xx, c0, hr)
  )
}
grab_robust <- function(M){
  rd <- M$rd
  list(
    est  = as.numeric(rd$coef["Robust", 1]),
    se   = as.numeric(rd$se["Robust", 1]),
    Nobs = sum(M$mask),
    Nl   = M$Nl, Nr = M$Nr,
    ybar = M$ybar,
    hl   = M$hl, hr = M$hr
  )
}

# ---------------- wrapper principal: pásale tu df (malto_all / mmediano_all / mbajo_all) ----------------
run_rd_prob_reprobar <- function(df, cutoff = 79, pass_cut = 3.0){
  # y: 'reprobo' si existe; si no, construir desde CALIFICACION_FINAL_VALOR
  if ("reprobo" %in% names(df)) {
    y_raw <- df$reprobo
    if (is.character(y_raw) || is.factor(y_raw)) {
      y <- as.integer(tolower(as.character(y_raw)) %in% c("si","sí","yes","1","true"))
    } else {
      y <- to_num(y_raw)
      uz <- na.omit(unique(y))
      if (all(sort(uz) == c(1,2))) y <- y - 1
      y <- ifelse(y > 0.5, 1, 0)
    }
  } else {
    stopifnot("CALIFICACION_FINAL_VALOR" %in% names(df))
    y <- to_num(df$CALIFICACION_FINAL_VALOR)
    y <- as.integer(y < pass_cut)  # 1 = reprobó si nota < pass_cut
  }
  
  # running y fuzzy del MISMO df
  stopifnot(all(c("PMA","Precalculo") %in% names(df)))
  x <- to_num(df$PMA)
  z <- to_num(df$Precalculo)
  uz <- na.omit(unique(z))
  if (all(sort(uz) == c(1,2))) z <- z - 1
  if (!all(na.omit(unique(z)) %in% c(0,1))) z <- ifelse(z > 0.5, 1, 0)
  
  # ---- covariables por especificación ----
  # Cohorte: intenta coho_*; si no existen, usa 'calendar' si está
  cohoM <- mk_dummies(df, "coho_")
  calM  <- if ("calendar" %in% names(df)) as.matrix(df[, "calendar", drop=FALSE]) else NULL
  # Programa
  carrM <- mk_dummies(df, "carr_")
  
  cov_a <- NULL
  # (2) "cohort FE": si hay coho_*, úsalos; si no, pero hay calendar, usa calendar
  cov_b <- if (!is.null(cohoM)) cohoM else calM
  # (3) "cohort FE + program FE"
  if (!is.null(cohoM) && !is.null(carrM)) {
    cov_c <- cbind(cohoM, carrM)
  } else if (is.null(cohoM) && !is.null(calM) && !is.null(carrM)) {
    cov_c <- cbind(calM, carrM)
  } else if (!is.null(cohoM)) {
    cov_c <- cohoM
  } else if (!is.null(calM)) {
    cov_c <- calM
  } else {
    cov_c <- carrM
  }
  
  # extras (como en tu base)
  extra_names <- intersect(c("total_seccion","genero","edad","ESTRATO","PUNTAJE"), names(df))
  extras <- if (length(extra_names)) {
    as.matrix(data.frame(lapply(df[, extra_names, drop=FALSE], to_num)))
  } else NULL
  extras <- drop_zerovar(extras)
  
  cov_d <- {
    tmp <- cov_c
    if (!is.null(extras)) tmp <- if (is.null(tmp)) extras else cbind(tmp, extras)
    drop_zerovar(tmp)
  }
  
  # ---- corre las 4 especificaciones ----
  M_a <- fit_model(y, x, z, covs = cov_a, c0 = cutoff)
  M_b <- fit_model(y, x, z, covs = cov_b, c0 = cutoff)
  M_c <- fit_model(y, x, z, covs = cov_c, c0 = cutoff)
  M_d <- fit_model(y, x, z, covs = cov_d, c0 = cutoff)
  
  a <- grab_robust(M_a); b <- grab_robust(M_b); c <- grab_robust(M_c); d <- grab_robust(M_d)
  
  # ---- tabla tipo Stata ----
  pval  <- function(e,s) 2*pnorm(-abs(e/s))
  stars <- function(p) ifelse(is.na(p),"", ifelse(p<.01,"***", ifelse(p<.05,"**", ifelse(p<.10,"*",""))))
  est_vec <- c(a$est,b$est,c$est,d$est)
  se_vec  <- c(a$se, b$se, c$se, d$se)
  p_vec   <- mapply(pval, est_vec, se_vec)
  N_vals  <- c(a$Nobs, b$Nobs, c$Nobs, d$Nobs)
  lefts   <- c(a$Nl,   b$Nl,   c$Nl,   d$Nl)
  rights  <- c(a$Nr,   b$Nr,   c$Nr,   d$Nr)
  means   <- sprintf("%.3f", rep(a$ybar, 4))          # outcome mean fijado al (1)
  bws     <- sprintf("%.3f", c(a$hl, b$hl, c$hl, d$hl))
  
  course_effect <- tibble(
    Row="Course Effect",
    `(1)`=paste0(sprintf("%.3f", est_vec[1]), stars(p_vec[1])),
    `(2)`=paste0(sprintf("%.3f", est_vec[2]), stars(p_vec[2])),
    `(3)`=paste0(sprintf("%.3f", est_vec[3]), stars(p_vec[3])),
    `(4)`=paste0(sprintf("%.3f", est_vec[4]), stars(p_vec[4]))
  )
  course_se <- tibble(
    Row="",
    `(1)`=paste0("[",sprintf("%.3f", se_vec[1]),"]"),
    `(2)`=paste0("[",sprintf("%.3f", se_vec[2]),"]"),
    `(3)`=paste0("[",sprintf("%.3f", se_vec[3]),"]"),
    `(4)`=paste0("[",sprintf("%.3f", se_vec[4]),"]")
  )
  footer <- dplyr::bind_rows(
    tibble(Row="Observations", `(1)`=format(N_vals[1], big.mark=","), `(2)`=format(N_vals[2], big.mark=","), `(3)`=format(N_vals[3], big.mark=","), `(4)`=format(N_vals[4], big.mark=",")),
    tibble(Row="Obs. left",    `(1)`=as.character(lefts[1]), `(2)`=as.character(lefts[2]), `(3)`=as.character(lefts[3]), `(4)`=as.character(lefts[4])),
    tibble(Row="Obs. right",   `(1)`=as.character(rights[1]), `(2)`=as.character(rights[2]), `(3)`=as.character(rights[3]), `(4)`=as.character(rights[4])),
    tibble(Row="Outcome mean", `(1)`=means[1], `(2)`=means[2], `(3)`=means[3], `(4)`=means[4]),
    tibble(Row="Bandwidth",    `(1)`=bws[1],   `(2)`=bws[2],   `(3)`=bws[3],   `(4)`=bws[4]),
    tibble(Row="Cohort FE",    `(1)`=ifelse(is.null(cohoM) & is.null(calM),"No","Yes"),
           `(2)`="Yes", `(3)`="Yes", `(4)`="Yes"),
    tibble(Row="Program FE",   `(1)`=ifelse(is.null(carrM),"No","Yes"),
           `(2)`="No",  `(3)`="Yes", `(4)`="Yes"),
    tibble(Row="Covariates",   `(1)`="No", `(2)`="No",  `(3)`="No",  `(4)`=ifelse(is.null(extras),"No","Yes"))
  )
  header <- tibble(
    Row=c("VARIABLES","LABELS"),
    `(1)`=c("", "reprobo"),
    `(2)`=c("", "reprobo"),
    `(3)`=c("", "reprobo"),
    `(4)`=c("", "reprobo")
  )
  
  table_out <- dplyr::bind_rows(header, course_effect, course_se, footer)
  
  list(
    table = table_out,
    raw   = tibble(model=c("(1)","(2)","(3)","(4)"),
                   estimate=est_vec, std_error=se_vec, p_value=p_vec,
                   N=N_vals, N_left=lefts, N_right=rights,
                   ybar=a$ybar, h_l=c(a$hl,b$hl,c$hl,d$hl), h_r=c(a$hr,b$hr,c$hr,d$hr))
  )
}

res_malto    <- run_rd_prob_reprobar(malto_all,    cutoff = 79, pass_cut = 3.0)
res_mmediano <- run_rd_prob_reprobar(mmediano_all, cutoff = 79, pass_cut = 3.0)
res_mbajo    <- run_rd_prob_reprobar(mbajo_all,    cutoff = 79, pass_cut = 3.0)

print(res_malto$table,    n = nrow(res_malto$table))
print(res_mmediano$table, n = nrow(res_mmediano$table))
print(res_mbajo$table,    n = nrow(res_mbajo$table))



# ====================== Table 6: Precálculo Effect on Differential Calculus Grade ======================
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(rdrobust)
  library(haven)
})

# ----------------------------- Helpers básicos (tipos) -----------------------------
to_num <- function(v){
  if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
  if (is.factor(v)) v <- as.character(v)
  suppressWarnings(as.numeric(v))
}

# ----------------------------- FE de programa (carr_* con baseline) -----------------------------
get_prog_fe <- function(dat){
  carr_cols <- grep("^carr_\\d+$", names(dat), value = TRUE)
  if (!length(carr_cols)) return(NULL)
  carr_cols <- sort(carr_cols)
  M <- as.matrix(dat[, carr_cols, drop = FALSE])
  if (ncol(M) >= 2) M <- M[, -1, drop = FALSE] else M <- NULL  # quitar baseline
  M
}

# Fallback si no hay carr_*: construir FE desde un código categórico
mk_program_FE_from_code <- function(dat, var = "cod_carrera"){
  if (!var %in% names(dat)) return(NULL)
  M <- model.matrix(~ factor(dat[[var]]) - 1)
  colnames(M) <- paste0("prog_", seq_len(ncol(M)))
  if (ncol(M) >= 2) M <- M[, -1, drop = FALSE]
  M
}

# ----------------------------- Helpers RD (ancho, N, media en ventana) -----------------------------
.get_hr <- function(rd){
  if (!is.null(rd$h))  return(c(hl = rd$h[1], hr = rd$h[2]))
  if (!is.null(rd$bws)) return(c(hl = rd$bws[1], hr = rd$bws[2]))
  stop("No pude encontrar los anchos de banda (h_l, h_r).")
}
.get_effN <- function(rd){
  if (!is.null(rd$Nh))  return(c(Nh_l = rd$Nh[1], Nh_r = rd$Nh[2]))
  if (!is.null(rd$N_h)) return(c(Nh_l = rd$N_h[1], Nh_r = rd$N_h[2]))
  c(Nh_l = NA_real_, Nh_r = NA_real_)
}
.mean_in_window <- function(y, x, hr, c0 = 79){
  win <- !is.na(x) & (x >= (c0 - hr["hl"])) & (x <= (c0 + hr["hr"]))
  mean(y[win], na.rm = TRUE)
}

# ----------------------------- Extractores tolerantes a versiones -----------------------------
.pick_by_label <- function(M, want = c("bias","robust")){
  want <- match.arg(want)
  if (is.null(M)) return(list(i=NA_integer_, j=NA_integer_))
  rn <- rownames(M); cn <- colnames(M)
  pat <- if (want == "bias") "bias|bias[- ]?correct" else "robust"
  if (!is.null(cn)) {
    j <- which(grepl(pat, cn, ignore.case = TRUE))[1]
    if (!is.na(j)) return(list(i = 1L, j = j))
  }
  if (!is.null(rn)) {
    i <- which(grepl(pat, rn, ignore.case = TRUE))[1]
    if (!is.na(i)) return(list(i = i, j = 1L))
  }
  # fallback por posiciones: conv=1, bc=2, robust=3
  list(i = 1L, j = ifelse(want == "bias",
                          ifelse(ncol(M) >= 2, 2L, 1L),
                          ifelse(ncol(M) >= 3, 3L, min(2L, ncol(M)))))
}
.pick_scalar <- function(M, pos){
  if (is.null(M) || is.na(pos$i) || is.na(pos$j) ||
      nrow(M) < pos$i || ncol(M) < pos$j) return(NA_real_)
  as.numeric(M[pos$i, pos$j])
}
.pick_ci_robust <- function(CI){
  if (is.null(CI)) return(c(NA_real_, NA_real_))
  rn <- rownames(CI); cn <- colnames(CI)
  if (!is.null(rn)) {
    i <- which(grepl("robust", rn, ignore.case = TRUE))[1]
    if (!is.na(i) && ncol(CI) >= 2) return(as.numeric(CI[i, 1:2]))
  }
  if (!is.null(cn) && nrow(CI) >= 1) {
    j1 <- which(grepl("cil.*robust|lower.*robust|robust.*low", cn, TRUE))[1]
    j2 <- which(grepl("ciu.*robust|upper.*robust|robust.*upp", cn, TRUE))[1]
    if (!is.na(j1) && !is.na(j2)) return(as.numeric(CI[1, c(j1, j2)]))
  }
  if (length(dim(CI)) == 2 && ncol(CI) >= 6) return(as.numeric(CI[1, 5:6])) # conv(1:2), bc(3:4), robust(5:6)
  if (is.null(dim(CI)) && length(CI) >= 6)  return(as.numeric(CI[c(5,6)]))
  c(NA_real_, NA_real_)
}

# ----------------------------- Núcleo: construir tabla (1)–(4) para un df -----------------------------
build_table6 <- function(df, cutoff = 79){
  stopifnot(all(c("CALIFICACION_FINAL_VALOR","PMA","Precalculo") %in% names(df)))
  y  <- to_num(df$CALIFICACION_FINAL_VALOR)
  x  <- to_num(df$PMA)
  fz <- to_num(df$Precalculo)
  
  # Normalizar fuzzy a 0/1 si viniera como {1,2} u otros
  if (all(na.omit(unique(fz)) %in% c(1,2))) fz <- fz - 1
  if (!all(na.omit(unique(fz)) %in% c(0,1))) fz <- ifelse(fz > 0.5, 1, 0)
  
  # ---------------- Covariables por especificación ----------------
  cov_a <- NULL
  cov_b <- if ("calendar" %in% names(df)) as.matrix(df[, "calendar", drop = FALSE]) else NULL
  progM <- get_prog_fe(df)
  if (is.null(progM)) progM <- mk_program_FE_from_code(df, "cod_carrera")  # fallback
  cov_c <- if (!is.null(progM)) cbind(cov_b, progM) else cov_b
  
  extra_cols <- intersect(c("total_seccion","genero","edad","ESTRATO","PUNTAJE"), names(df))
  base_d     <- if (!is.null(progM)) cbind(cov_b, progM) else cov_b
  cov_d      <- if (length(extra_cols) > 0) cbind(base_d, as.matrix(df[, extra_cols, drop = FALSE])) else base_d
  
  # Observations de Stata = casos completos por especificación
  N_total <- function(covs_mat){
    req <- cbind(y, x, fz)
    if (!is.null(covs_mat)) req <- cbind(req, covs_mat)
    sum(stats::complete.cases(req))
  }
  
  # ---------------- Estimación (coef = BC; EE/p/IC = Robust) ----------------
  fit_rd_bcR <- function(covs_mat, label, calFE, progFE, covsFlag){
    # limpiar constantes/col de var=0
    if (!is.null(covs_mat)) {
      keep <- apply(covs_mat, 2, function(v) stats::var(v, na.rm = TRUE) > 0)
      if (any(!keep)) covs_mat <- covs_mat[, keep, drop = FALSE]
      if (!is.null(covs_mat) && ncol(covs_mat) == 0) covs_mat <- NULL
    }
    
    rd <- rdrobust(
      y = y, x = x, c = cutoff, fuzzy = fz,
      kernel = "triangular", p = 1, q = 2, bwselect = "mserd",
      covs = covs_mat, all = TRUE
      # Si quieres clavar Stata en EE: agrega vce="hc0"
    )
    
    hr  <- .get_hr(rd)
    Nh  <- .get_effN(rd)
    muY <- .mean_in_window(y, x, hr, c0 = cutoff)
    
    pos_est_bc <- .pick_by_label(rd$Estimate, "bias")
    pos_se_rb  <- .pick_by_label(rd$se,       "robust")
    pos_pv_rb  <- .pick_by_label(rd$pv,       "robust")
    
    est <- .pick_scalar(rd$Estimate, pos_est_bc)
    se  <- .pick_scalar(rd$se,       pos_se_rb)
    pv  <- .pick_scalar(rd$pv,       pos_pv_rb)
    ci  <- .pick_ci_robust(rd$ci)
    
    tibble(
      model        = label,
      estimate     = est,
      std_error    = se,
      p_value      = pv,
      ci_lo        = ci[1],
      ci_hi        = ci[2],
      observations = N_total(covs_mat),
      obs_left     = Nh["Nh_l"],
      obs_right    = Nh["Nh_r"],
      outcome_mean = muY,
      bandwidth_hl = hr["hl"],
      bandwidth_hr = hr["hr"],
      calendar_FE  = ifelse(calFE, "Yes", "No"),
      program_FE   = ifelse(progFE, "Yes", "No"),
      covariates   = ifelse(covsFlag, "Yes", "No")
    )
  }
  
  t1 <- fit_rd_bcR(cov_a, "(1)", FALSE, FALSE, FALSE)
  t2 <- fit_rd_bcR(cov_b, "(2)", TRUE,  FALSE, FALSE)
  t3 <- fit_rd_bcR(cov_c, "(3)", TRUE,  TRUE,  FALSE)
  t4 <- fit_rd_bcR(cov_d, "(4)", TRUE,  TRUE,  TRUE)
  
  tabla <- bind_rows(t1, t2, t3, t4)
  
  # Fijar Outcome mean al del modelo (1)
  ybar1 <- tabla$outcome_mean[1]
  tabla$outcome_mean <- rep(ybar1, nrow(tabla))
  
  # Formateo tipo Stata
  stars <- function(p){ ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))) }
  fmt3  <- function(x) sprintf("%.3f", x)
  
  out_table <- tibble(
    Row = c("Course Effect", "", "Observations", "Obs. left", "Obs. right",
            "Outcome mean", "Bandwidth", "Calendar FE", "Program FE", "Covariates"),
    `(1)` = c(paste0(fmt3(tabla$estimate[1]), stars(tabla$p_value[1])),
              paste0("[", fmt3(tabla$std_error[1]), "]"),
              tabla$observations[1], tabla$obs_left[1], tabla$obs_right[1],
              fmt3(tabla$outcome_mean[1]), fmt3(tabla$bandwidth_hl[1]),
              tabla$calendar_FE[1], tabla$program_FE[1], tabla$covariates[1]),
    `(2)` = c(paste0(fmt3(tabla$estimate[2]), stars(tabla$p_value[2])),
              paste0("[", fmt3(tabla$std_error[2]), "]"),
              tabla$observations[2], tabla$obs_left[2], tabla$obs_right[2],
              fmt3(tabla$outcome_mean[2]), fmt3(tabla$bandwidth_hl[2]),
              tabla$calendar_FE[2], tabla$program_FE[2], tabla$covariates[2]),
    `(3)` = c(paste0(fmt3(tabla$estimate[3]), stars(tabla$p_value[3])),
              paste0("[", fmt3(tabla$std_error[3]), "]"),
              tabla$observations[3], tabla$obs_left[3], tabla$obs_right[3],
              fmt3(tabla$outcome_mean[3]), fmt3(tabla$bandwidth_hl[3]),
              tabla$calendar_FE[3], tabla$program_FE[3], tabla$covariates[3]),
    `(4)` = c(paste0(fmt3(tabla$estimate[4]), stars(tabla$p_value[4])),
              paste0("[", fmt3(tabla$std_error[4]), "]"),
              tabla$observations[4], tabla$obs_left[4], tabla$obs_right[4],
              fmt3(tabla$outcome_mean[4]), fmt3(tabla$bandwidth_hl[4]),
              tabla$calendar_FE[4], tabla$program_FE[4], tabla$covariates[4])
  )
  
  list(table = out_table, raw = tabla)
}

# ----------------------------- Runner por terciles -----------------------------
run_table6_terciles <- function(malto_all, mmediano_all, mbajo_all, cutoff = 79){
  Alto    <- build_table6(malto_all,    cutoff)
  Mediano <- build_table6(mmediano_all, cutoff)
  Bajo    <- build_table6(mbajo_all,    cutoff)
  
  # Comparativo rápido: Course Effect + Bandwidth
  pick_rows <- function(tb) dplyr::filter(tb$table, Row %in% c("Course Effect","Bandwidth"))
  compare <- bind_rows(
    pick_rows(Alto)    %>% mutate(Tercil="Alto"),
    pick_rows(Mediano) %>% mutate(Tercil="Mediano"),
    pick_rows(Bajo)    %>% mutate(Tercil="Bajo")
  ) %>% relocate(Tercil, .before = Row)
  
  list(Alto=Alto, Mediano=Mediano, Bajo=Bajo, compare=compare)
}

# ----------------------------- Ejemplo de uso -----------------------------
 res6 <- run_table6_terciles(malto_all, mmediano_all, mbajo_all, cutoff = 79)
print(res6$Alto$table,    n = nrow(res6$Alto$table))
print(res6$Mediano$table, n = nrow(res6$Mediano$table))
print(res6$Bajo$table,    n = nrow(res6$Bajo$table))
print(res6$compare,       n = nrow(res6$compare))













