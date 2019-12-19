#Funcion para carga automatica o instalacion, en su defecto, de los paquetes
#necesarios para que el examen sea reproducible
instalar <- function(paquete) {
  
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

#Lista de paquetes necesarios
paquetes <- c('plotly','ggplot2','readr','stringr','tidyr','dplyr','shiny','mice','hrbrthemes','onehot','gridExtra')
lapply(paquetes, instalar)

#Cargamos los datos
carga_datos_entrena <- function(){
  if(file.exists('train.csv')){
    datos <- read_csv('train.csv',
                      col_types = cols(
                        TripType = col_factor(levels = l),
                        Weekday = col_factor(levels = w)))
    print('Datos existen y se han cargado')
  }
  else{
    warning('El archivo no existe. Intentando descargar...')
    dir_web <- 'https://raw.githubusercontent.com/brunocgf/WalmartRecruiting/master/data/train.csv'
    datos <- read_csv(dir_web,
                      col_types = cols(
                        TripType = col_factor(levels = l),
                        Weekday = col_factor(levels = w)))
    print('Datos descargados y listos para usarse')
  }
  return(datos)
}

carga_datos_prueba <- function(){
  if(file.exists('test.csv')){
    datos <- read_csv('test.csv',
                      col_types = cols(
                        TripType = col_factor(levels = l),
                        Weekday = col_factor(levels = w)))
    print('Datos existen y se han cargado')
  }
  else{
    warning('El archivo no existe. Intentando descargar...')
    dir_web <- 'https://raw.githubusercontent.com/brunocgf/WalmartRecruiting/master/data/test.csv'
    datos <- read_csv(dir_web,
                      col_types = cols(
                        TripType = col_factor(levels = l),
                        Weekday = col_factor(levels = w)))
    print('Datos descargados y listos para usarse')
  }
  return(datos)
}

#Para la columna que tiene NULL textual, asignamos NA
remueve_nulls <- function(datos){
  indices <- which(datos == 'NULL',arr.ind = TRUE)
  datos[indices] <- NA
  return(datos)
}

#Limpiamos valores erroneos
limpia_textos <- function(datos){
  datos[datos == "MENSWEAR"] <- "MENS WEAR"
  return(datos)
}

#Obtemos indices que tienen un determinado porcentaje de nas
indices_con_NAs <- function(data, porcentaje) {
  n <- as.integer(porcentaje  * ncol(data))
  which( apply(data, 1, function(x) sum(is.na(x))) >= n )
}

#funcion para extraer los codigos de las companias
comp_code <- function(dato)
{
  x <- substr(dato, 0, 6)
  if (!is.na(x) & x != "000000")
  {
    return(x)
  }
  else if (is.na(x))
  {
    return("9x9x9x")
  }
}

moda <- function(dato) {
  unico <- unique(dato)
  unico[which.max(tabulate(match(dato, unico)))]
}
