# Experimentos Colaborativos Default
# Workflow  Training Strategy
# Problema 7 - Grupo B. El objetivo del experimento es probar que los meses de
# la pandemia no son representativos para entrenar un modelo de predicción de
# bajas. Por lo tanto, se entrena un modelo quitando los meses de pandemia, y se
# cotrasta contra una corrida de un modelo que incluye dichos meses de pandemia.
# Este script establece la estrategia de testing sin los meses de pandemia que van
# desde 202003 a 202004. Se entrena en 18 meses, manteniendo siempre fija esta ventana

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")


# Parametros del script
PARAM <- list()
PARAM$experimento <- "TS6410_PROBL07_01"

PARAM$exp_input <- "FE6310_PROBL07"

# me salteo los meses duros de pandemia
# entreno en 18 meses

# Predicción sobre el mes 202107, para obtener el gráfico de la curva de ganancias
PARAM$future <- c(202107)
PARAM$final_train <- c(
  202105, 202104, 202103, 202102, 202101, 202012, 202011, 202010, 202009, 
  202008, 202007, 202006, 202005, 202002, 202001, 201912, 201911, 201910
)

# Entreno en 18 meses, sin considerar los meses de pandemia desde 202003 a 202004
PARAM$train$training <- c(
  202103, 202102, 202101, 202012, 202011, 202010, 202009, 202008, 202007, 
  202006, 202005, 202002, 202001, 201912, 201911, 201910, 201909, 201908
)

# Valido en 1 mes, 202104
PARAM$train$validation <- c(202104)

# Testeo en 1 mes, 202105
PARAM$train$testing <- c(202105)

# Atencion  0.1  de  undersampling de la clase mayoritaria,  los CONTINUA
# 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA
PARAM$train$undersampling <- 0.1

PARAM$train$semilla <- 125541 # cambiar por su propia semilla  !!!

PARAM$home <- "~/buckets/b1/"
# FIN Parametros del script


OUTPUT <- list()

# si training se establece identico a validation,
#  entonces aguas abajo se hara Cross-Validation
# si training = validation = testing   tambien se hara  Cross-Validation
#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo OUTPUT
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd(PARAM$home)

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input <- paste0("./exp/", PARAM$exp_input, "/dataset.csv.gz")
dataset <- fread(dataset_input)


# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

setorder(dataset, foto_mes, numero_de_cliente)

# grabo los datos del futuro
fwrite(dataset[foto_mes %in% PARAM$future, ],
  file = "dataset_future.csv.gz",
  logical01 = TRUE,
  sep = ","
)

# grabo los datos donde voy a entrenar los Final Models
fwrite(dataset[foto_mes %in% PARAM$final_train, ],
  file = "dataset_train_final.csv.gz",
  logical01 = TRUE,
  sep = ","
)



# grabo los datos donde voy a hacer la optimizacion de hiperparametros
set.seed(PARAM$train$semilla, kind = "L'Ecuyer-CMRG")
dataset[
  foto_mes %in% PARAM$train$training,
  azar := runif(nrow(dataset[foto_mes %in% PARAM$train$training]))
]

dataset[, fold_train := 0L]
dataset[
  foto_mes %in% PARAM$train$training &
    (azar <= PARAM$train$undersampling |
      clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  fold_train := 1L
]

dataset[, fold_validate := 0L]
dataset[foto_mes %in% PARAM$train$validation, fold_validate := 1L]

dataset[, fold_test := 0L]
dataset[foto_mes %in% PARAM$train$testing, fold_test := 1L]


fwrite(dataset[fold_train + fold_validate + fold_test >= 1, ],
  file = "dataset_training.csv.gz",
  logical01 = TRUE,
  sep = ","
)

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(is.na(x))
    }
  ),
  "ceros" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(x == 0, na.rm = TRUE)
    }
  )
))

fwrite(tb_campos,
  file = "dataset_training.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset_train$ncol <- ncol(dataset[fold_train > 0, ])
OUTPUT$dataset_train$nrow <- nrow(dataset[fold_train > 0, ])
OUTPUT$dataset_train$periodos <- dataset[fold_train > 0, length(unique(foto_mes))]

OUTPUT$dataset_validate$ncol <- ncol(dataset[fold_validate > 0, ])
OUTPUT$dataset_validate$nrow <- nrow(dataset[fold_validate > 0, ])
OUTPUT$dataset_validate$periodos <- dataset[fold_validate > 0, length(unique(foto_mes))]

OUTPUT$dataset_test$ncol <- ncol(dataset[fold_test > 0, ])
OUTPUT$dataset_test$nrow <- nrow(dataset[fold_test > 0, ])
OUTPUT$dataset_test$periodos <- dataset[fold_test > 0, length(unique(foto_mes))]

OUTPUT$dataset_future$ncol <- ncol(dataset[foto_mes %in% PARAM$future, ])
OUTPUT$dataset_future$nrow <- nrow(dataset[foto_mes %in% PARAM$future, ])
OUTPUT$dataset_future$periodos <- dataset[foto_mes %in% PARAM$future, length(unique(foto_mes))]

OUTPUT$dataset_finaltrain$ncol <- ncol(dataset[foto_mes %in% PARAM$final_train, ])
OUTPUT$dataset_finaltrain$nrow <- nrow(dataset[foto_mes %in% PARAM$final_train, ])
OUTPUT$dataset_finaltrain$periodos <- dataset[foto_mes %in% PARAM$final_train, length(unique(foto_mes))]

OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)
