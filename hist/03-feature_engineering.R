
## Para cada una de las variables categóricas se hacen las variables dummies.
## Están dentro de una función para no cargar e una vez los tres conjuntos de datos.
## Para los días de la semana se convierten en enteres siendo viernes igual a 1


feature_eng <- function(df=train, feature = 'dep', set = 'train') {
  
  if(set == 'train'){
    
    if(feature == 'dep'){
      res <- train %>%
        pivot_wider(id_cols = c('TripType','VisitNumber','Weekday'),
                    names_from = 'DepartmentDescription',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0)) %>% 
        mutate(Weekday = as.integer(Weekday))
    }
    if(feature == 'fine'){
      res <- train %>%
        pivot_wider(id_cols = c('TripType','VisitNumber','Weekday'),
                    names_from = 'FinelineNumber',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))%>% 
        mutate(Weekday = as.integer(Weekday))
    }
    if(feature == 'upc'){
      res <- train %>%
        pivot_wider(id_cols = c('TripType','VisitNumber','Weekday'),
                    names_from = 'Upc',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))%>% 
        mutate(Weekday = as.integer(Weekday))
    }
  }
  
  else{
    if(feature == 'dep'){
      res <- train %>%
        pivot_wider(id_cols = c('VisitNumber','Weekday'),
                    names_from = 'DepartmentDescription',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))%>% 
        mutate(Weekday = as.integer(Weekday))
    }
    if(feature == 'fine'){
      res <- train %>%
        pivot_wider(id_cols = c('VisitNumber','Weekday'),
                    names_from = 'FinelineNumber',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))%>% 
        mutate(Weekday = as.integer(Weekday))
    }
    if(feature == 'upc'){
      res <- train %>%
        pivot_wider(id_cols = c('VisitNumber','Weekday'),
                    names_from = 'Upc',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))%>% 
        mutate(Weekday = as.integer(Weekday))
    }
  }
  res
}