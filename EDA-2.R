train <- read.csv(file = "data/train.csv", stringsAsFactors=FALSE)
test <- read.csv(file = "data/test.csv", stringsAsFactors=FALSE)
a <- unique(sort(train$TripType))
a <- as.factor(a)

b <- unique(sort(test$TripType))
b <- as.factor(b)

sum(is.na(train$Upc))
sum(is.na(test$Upc))
sum(is.na(train$FinelineNumber))
sum(is.na(test$FinelineNumber))

#Trip types por visitas, mejorada
train %>%
  group_by(TripType) %>% 
  summarise(n = n_distinct(VisitNumber)) %>% 
  ggplot() +
  geom_bar(aes(a, n), stat="identity") 

#Conteos de na en los atributos
barplot(apply(train, 2 ,function(x) sum(is.na(x))),  main="NAs",ylab="Apariciones",xlab="Atributo",las=2)

#Contamos las aparaciones por departamento en las transaccciones, se notan los NULL, y deptos repetidos
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
