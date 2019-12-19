#Verifcamos que no haya problemas con los renglones del conjunto de datos, despues de importarlos
problems(test_datos)$row
problems(train_datos)$row

#Limpiamos valores de columnas mal escritos
train_datos <- limpia_textos(train_datos)
test_datos <- limpia_textos(test_datos)
sort(unique(train_datos$DepartmentDescription))

#Cambiamos el null textual a NA
train_datos <- remueve_nulls(train_datos)
test_datos <- remueve_nulls(test_datos)

#Ajustamos el tipo de datos
train_datos <- readr::type_convert(train_datos)
test_datos <- readr::type_convert(test_datos)

#Removemos columnas con mas de 40% de nas
train_datos_filtrado <- train_datos[-indices_con_NAs(train_datos, 0.43),]
test_datos_filtrado <- test_datos[-indices_con_NAs(test_datos, 0.60),]
sum(is.na(test_datos_filtrado))
#Imputamos valores faltantes
limpios_train <- knnImputation(train_datos_filtrado)
limpios_test <- knnImputation(test_datos_filtrado)