#Cargamos los datos
limpios_train <- read_csv("./LimpiosKnn_train.csv",
                          col_types = cols(
                            TripType = col_factor(levels = level_l),
                            Weekday = col_factor(levels = level_w),
                            DepartmentDescription = col_factor(levels = level_d)))
limpios_train <- limpios_train[-1]
limpios_test <- read_csv("./LimpiosKnn_test.csv",
                         col_types = cols(
                           Weekday = col_factor(levels = level_w),
                           DepartmentDescription = col_factor(levels = level_d)))

limpios_train["compania"] <- apply(limpios_train[,4], 1, function (x) comp_code(x))
limpios_test["compania"] <- apply(limpios_test[,3], 1, function (x) comp_code(x))
sum(is.na(limpios_test))
problems(limpios_test)$row
#PARA TRAIN

#indices donde hay y no hay devoluciones ()
a <- which(apply(limpios_train[,5],1, function(x) x < 0))     
k <- which(apply(limpios_train[,5],1, function(x) x > 0))     
#donde hay
limpios_train$devolucion[a] <- 1
#donde no hay
limpios_train$devolucion[k] <- 0

#Ahora precisamos las cantidades de ventas y devoluciones
limpios_train["num_ventas"] <- limpios_train$ScanCount
limpios_train["num_devs"] <- limpios_train$ScanCount

#Si el numero de venta es negativo
a <- which(apply(limpios_train[,10], 1, function(x) x < 0))  
limpios_train$num_ventas[a] <- 0

#Si el numero de devolucion es positivo
a <- which(apply(limpios_train[,11],1, function(x) x > 0)) 
limpios_train$num_devs[a] <- 0


getmode(test_datos_filtrado$Upc)


#MIXTO

#Fineline por numero de visita
train_fn_mode <- limpios_train %>%
  select(VisitNumber, FinelineNumber) %>%
  group_by(VisitNumber)

train_fn_mode <- aggregate(train_fn_mode$FinelineNumber, list(train_fn_mode$VisitNumber), moda) 

names(train_fn_mode)[names(train_fn_mode) == "Group.1"] <- "VisitNumber"
names(train_fn_mode)[names(train_fn_mode) == "x"] <- "moda_fineline"

#Cantidad de Upc y fineline number por visita
train_vn_upcfn <- limpios_train %>% 
  select(VisitNumber, Upc, FinelineNumber) %>% 
  group_by(VisitNumber) %>%
  count() 

#Creamos variables dummies para los departementos
train_dep_dum <- limpios_train %>% 
  select(DepartmentDescription) 

library(fastDummies)
train_dep_dum <- fastDummies::dummy_cols(train_dep_dum)
train_dep_dum <- cbind(limpios_train$VisitNumber,train_dep_dum)
train_dep_dum <- train_dep_dum[-2]


# Variante variables dummies
train_des <- limpios_train %>%
  pivot_wider(id_cols = c('TripType','VisitNumber','Weekday'),
              names_from = 'DepartmentDescription',
              values_from = 'ScanCount',
              values_fn = list(ScanCount = sum)) %>% 
  map_dfr(~replace_na(.,0)) %>% 
  mutate(Weekday = as.integer(Weekday)) %>% 
  select(TripType, VisitNumber, Weekday, level_d)

test_des <- test %>%
  pivot_wider(id_cols = c('VisitNumber','Weekday'),
              names_from = 'DepartmentDescription',
              values_from = 'ScanCount',
              values_fn = list(ScanCount = sum)) %>% 
  map_dfr(~replace_na(.,0)) %>% 
  mutate(Weekday = as.integer(Weekday),
         'HEALTH AND BEAUTY AIDS' = 0) %>% 
  select(VisitNumber, Weekday, level_d)

write_feather(train_des, "train.feather")
write_feather(test_des, "test.feather")
