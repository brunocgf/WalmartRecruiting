## Primero cargamos los datos

source('utils.R')
source('00-load.R')

# a <- unique(sort(train$TripType))
# a <- as.factor(a)
# b <- unique(sort(test$TripType))
# b <- as.factor(b)

# Empezamos viendo las características más importantes de los datos de entrenamiento

head(train)
str(train)
summary(train)

# Se puede observar que existen valores faltantes en los datos de entrenamiento.
# Para los datos de prueba tenemos el mismo caso

sum(is.na(train$Upc))
sum(is.na(test$Upc))
sum(is.na(train$FinelineNumber))
sum(is.na(test$FinelineNumber))

# Conteos de na en los atributos se puede ver en la siguiente gráfica
barplot(apply(train, 2 ,function(x) sum(is.na(x))),  main="NAs",ylab="Apariciones",xlab="Atributo",las=2)

#Contamos las aparaciones por departamento en las transacciones, se notan los NULL, y deptos repetidos
train %>%
  group_by(DepartmentDescription) %>%
  ggplot() +
  geom_bar(aes(train$DepartmentDescription), stat = "count") +
  theme(axis.text.x = element_text(angle = 90))

#Ahora que sabemos que hay null, graficamos su aparicion  
barplot(apply(train, 2, function(x) sum(x == "NULL" | is.na(x))), main="NAs y NULLs",ylab="Apariciones",xlab="Atributo",las=2)

#PARA PRUEBA

#Conteos de na en los atributos
barplot(apply(test, 2 ,function(x) sum(is.na(x))),  main="NAs",ylab="Apariciones",xlab="Atributo",las=2)

#Contamos las aparaciones por departamento en las transaccciones, se notan los NULL, y deptos repetidos
test %>%
  group_by(DepartmentDescription) %>%
  ggplot() +
  geom_bar(aes(test$DepartmentDescription), stat = "count") +
  theme(axis.text.x = element_text(angle = 90))

#Ahora que sabemos que hay null, graficamos su aparicion 
barplot(apply(test, 2, function(x) sum(x == "NULL" | is.na(x))), main="NAs y NULLs",ylab="Apariciones",xlab="Atributo",las=2)

#Comparamos nulos entre ambos datasets
par(mfrow = c(1:2))
barplot(apply(train, 2, function(x) sum(x == "NULL" | is.na(x))), main="NAs y NULLs Train",ylab="Apariciones",xlab="Atributo",las=2)
barplot(apply(test, 2, function(x) sum(x == "NULL" | is.na(x))), main="NAs y NULLs Test",ylab="Apariciones",xlab="Atributo",las=2)

# De las estadísticas mostradas es importante hacer notar que el número de cada tipo de visitas es inexacto,
# dado que cada observación de la tabla de datos es por transacción y no por visita.
# El número de visitas registradas son solo
length(unique(train$VisitNumber))
# También es interesante ver el número de ventas por tipo de visita.
# Abajo se muestra el tanto del  número de visitas como el número de transacciones y las ventas como el por tipo de de viaje.

p1 <- train %>%
  group_by(TripType) %>% 
  summarise(n = n_distinct(VisitNumber)) %>% 
  ggplot() +
  geom_bar(aes(x = TripType, y = n), stat = 'identity') +
  labs(title = 'Número de visitas por tipo de viaje') +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

p2 <- train %>%
  group_by(TripType) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_bar(aes(x = TripType, y = n), stat = 'identity') +
  labs(title = 'Transacciones por tipo de viaje') +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

p3 <- train %>%
  group_by(TripType) %>% 
  summarise(n = sum(ScanCount)) %>% 
  ggplot() +
  geom_bar(aes(x = TripType, y = n), stat = 'identity') +
  labs(title = 'Ventas por tipo de viaje') +
  theme_hc() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

grid.arrange(p1,p2,p3,nrow=3)


# A esta información podemos agregar el día de la semana

train %>%
  group_by(TripType, Weekday) %>% 
  summarise(n = n_distinct(VisitNumber)) %>% 
  ggplot() +
  geom_bar(aes(x = TripType, y = n), stat = 'identity') +
  facet_wrap(~Weekday) +
  theme_hc()

# Ahora veamos la relación entre el tipo de viaje y el Departament, con base el número de artículos comprados.

train %>%
  group_by(TripType, DepartmentDescription) %>% 
  summarise(n = sum(ScanCount)) %>% 
  semi_join(train %>% group_by(DepartmentDescription) %>% summarise(n=n()) %>% filter(n>3000),
            by = 'DepartmentDescription') %>% 
  ggplot(aes(x = TripType, y = DepartmentDescription, fill = n)) +
  geom_tile() +
  scale_fill_continuous(low="white", high="steelblue4") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
