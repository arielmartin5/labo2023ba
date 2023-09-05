# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
rm(list = ls())

# cargo las librerias que necesito
require("data.table")
require("nnet")
#require("randomForest")
#require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# Creeamos el campo clase_binaria2 en el dataset
library(dplyr)
dataset <- mutate(dataset, clase_binaria2 = ifelse(clase_ternaria == "BAJA+2", "pos", 
                    ifelse(clase_ternaria == "BAJA+1", "pos", "neg")))

# Eliminamos del dataset la clase ternaria
dataset <- select(dataset, -clase_ternaria)

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- nnet(clase_binaria2~.,data=dtrain,size = 1)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "pos"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_RF001.csv",
        sep = ","
)
