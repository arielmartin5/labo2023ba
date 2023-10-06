#Necesita para correr en Google Cloud
#   8 vCPU
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#Parametros del script
kexp_input  <- "HT6510"   #Aqui debe ir SU  bayesiana elegida
# FIN Parametros del script

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------

mostrar_iteracion <- function( campo )
{
  if( campo %in% colnames( tb_log ) )
  {
    cat( campo, ":", tb_log[  , round( min(get(campo)), 4 ) ],
                            tb_log[ 1, round( get(campo), 4 ) ],
                            tb_log[  , round( max( get(campo) ), 4 ) ], 
                            "\n" )
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

cat( "\n\n\n" )
cat( "Experimentos TS : ", length( list.files( path="~/buckets/b1/exp/" , pattern="TS" ) ) , "\n" )
cat( "Experimentos HT : ", length( list.files( path="~/buckets/b1/exp/" , pattern="HT" ) ) , "\n" )
cat( "Experimentos ZZ : ", length( list.files( path="~/buckets/b1/exp/" , pattern="ZZ" ) ) , "\n" )
cat( "Experimentos HB : ", length( list.files( path="~/buckets/b1/exp/" , pattern="HB" ) ) , "\n" )
cat( "\n\n\n" )

#leo la salida de la optimizacion bayesiana
arch_log  <- paste0( base_dir, "exp/", kexp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", kexp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_training.csv.gz" )
dtrain  <- fread( arch_dataset )


#impresiones
cat( "Experimento: ", kexp_input , "\n" )
cat( "Dataset Training: ", "cols:", ncol(dtrain) , "\n" )
cat( "train: ",    "filas:",  dtrain[ fold_train==1, .N] ,    ",  meses:",  dtrain[ fold_train==1, unique(foto_mes)] , "\n" )
cat( "validate: ", "filas:",  dtrain[ fold_validate==1, .N] , ",  meses:",  dtrain[ fold_validate==1, unique(foto_mes)] , "\n" )
cat( "test: ",     "filas:",  dtrain[ fold_test==1, .N] ,     ",  meses:",  dtrain[ fold_test==1, unique(foto_mes)] , "\n" )
cat("\n\n" )

rm( dtrain )
gc(verbose=FALSE)

#--------------------------------------

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dtrain_final  <- fread( arch_dataset )

cat( "Dataset Final Training: ", "filas:", nrow(dtrain_final), ", cols:", ncol(dtrain_final), "\nmeses:", dtrain_final[ , unique(foto_mes)] , "\n" )
cat( "\n\n" )
#--------------------------------------

cat( "Bayesiana: ",  "iteraciones:", nrow(tb_log),  ",  ganancia:", max( tb_log$ganancia ) ,"\n" )
cat( "\n\n" )

mostrar_iteracion( "learning_rate" )
mostrar_iteracion( "feature_fraction" )
mostrar_iteracion( "min_data_in_leaf" )
mostrar_iteracion( "num_leaves" )
