
## Para cada una de las variables categóricas se hacen las variables dummies.
## Están dentro de una función para no cargar e una vez los tres conjuntos de datos.


feature_eng <- function(df=train, feature = 'dep', set = 'train') {
  
  if(set == 'train'){
    
    if(feature == 'dep'){
      res <- train %>%
        pivot_wider(id_cols = c('TripType','VisitNumber','Weekday'),
                    names_from = 'DepartmentDescription',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))
    }
    if(feature == 'fine'){
      res <- train %>%
        pivot_wider(id_cols = c('TripType','VisitNumber','Weekday'),
                    names_from = 'FinelineNumber',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))
    }
    if(feature == 'upc'){
      res <- train %>%
        pivot_wider(id_cols = c('TripType','VisitNumber','Weekday'),
                    names_from = 'Upc',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))
    }
  }
  
  else{
    if(feature == 'dep'){
      res <- train %>%
        pivot_wider(id_cols = c('VisitNumber','Weekday'),
                    names_from = 'DepartmentDescription',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))
    }
    if(feature == 'fine'){
      res <- train %>%
        pivot_wider(id_cols = c('VisitNumber','Weekday'),
                    names_from = 'FinelineNumber',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))
    }
    if(feature == 'upc'){
      res <- train %>%
        pivot_wider(id_cols = c('VisitNumber','Weekday'),
                    names_from = 'Upc',
                    values_from = 'ScanCount',
                    values_fn = list(ScanCount = sum)) %>% 
        map_dfr(~replace_na(.,0))
    }
  }
  res
}