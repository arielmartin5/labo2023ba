# Ensemble de arboles de decision
# utilizando el naif metodo de Arboles Azarosos
# entreno cada arbol en un subconjunto distinto de atributos del dataset

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")

# parmatros experimento
PARAM <- list()
PARAM$experimento <- 3224

# Establezco la semilla aleatoria, cambiar por SU primer semilla
PARAM$semilla <- 124541

# parameetros rpart - Seteamos los valores para que no pode el árbol
PARAM$rpart_param <- list(
  "cp" = -1,
  "minsplit" = 250,
  "minbucket" = 100,
  "maxdepth" = 14
)

# parametros  arbol
# entreno cada arbol con solo 50% de las variables variables
PARAM$feature_fraction <- 0.5
# voy a generar 500 arboles, a mas arboles mas tiempo de proceso y MEJOR MODELO
#  pero ganancias marginales
PARAM$num_trees_max <- 500

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# Creeamos el campo clase_binaria2 en el dataset
library(dplyr)
dataset <- mutate(dataset, clase_binaria2 = ifelse(clase_ternaria == "BAJA+2", "pos", 
                    ifelse(clase_ternaria == "BAJA+1", "pos", "neg")))

# Eliminamos del dataset la clase ternaria
dataset <- select(dataset, -clase_ternaria)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(paste0("./exp/KA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


# que tamanos de ensemble grabo a disco, pero siempre debo generar los 500
grabar <- c(500)

# agrego tantos canaritos como columnas
#for (i in 1:ncol(dataset)) {
#    dataset[, paste0("canarito", i) := runif(nrow(dataset))]
#}

# defino los dataset de entrenamiento y aplicacion
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]


# aqui se va acumulando la probabilidad del ensemble
dapply[, prob_acumulada := 0]

# Establezco cuales son los campos que puedo usar para la prediccion
# el copy() es por la Lazy Evaluation
campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_binaria2")))


# Genero las salidas
set.seed(PARAM$semilla) # Establezco la semilla aleatoria

for (arbolito in 1:PARAM$num_trees_max) {
  qty_campos_a_utilizar <- as.integer(length(campos_buenos)
  * PARAM$feature_fraction)

  campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

  # paso de un vector a un string con los elementos
  # separados por un signo de "+"
  # este hace falta para la formula
  campos_random <- paste(campos_random, collapse = " + ")

  # armo la formula para rpart
  formulita <- paste0("clase_binaria2 ~ ", campos_random)

  # genero el arbol de decision
  modelo <- rpart(formulita,
    data = dtrain,
    xval = 0,
    control = PARAM$rpart_param
  )

  # hago el pruning de los canaritos
  # haciendo un hackeo a la estructura  modelo_original$frame
  # -666 es un valor arbritrariamente negativo que jamas es generado por rpart
  #modelo$frame[
  #    modelo$frame$var %like% "canarito",
  #    "complexity"
  #] <- -666

  #modelo_pruned <- prune(modelo, -666)

  # aplico el modelo a los datos que no tienen clase
  prediccion <- predict(modelo, dapply, type = "prob")

  dapply[, prob_acumulada := prob_acumulada + prediccion[, "pos"]]

  if (arbolito %in% grabar) {
    # Genero la entrega para Kaggle
    umbral_corte <- (1 / 40) * arbolito
    entrega <- as.data.table(list(
      "numero_de_cliente" = dapply[, numero_de_cliente],
      "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
    )) # genero la salida

    nom_arch <- paste0(
      "KA", PARAM$experimento, "_",
      sprintf("%.3d", arbolito), # para que tenga ceros adelante
      ".csv"
    )
    fwrite(entrega,
      file = nom_arch,
      sep = ","
    )

    cat(arbolito, " ")
  }
}
