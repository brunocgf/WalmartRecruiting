Comprensión del negocio
================
Bruno C. Gonzalez
16/12/2019

# Comprención del Negocio

## Antecedentes

El objetivo de la competencia es categorizar las visitas de compras
basado en los productos que ha adquirido el cliente, por ejemplo ‘visita
para una pequeña cena’, ‘visita para comprar regalos’, etc.

Walmart ya ha categorizado los viajes contenidos en estos datos dentro
de 38 distintos tipos, usando un metodo propio en un extenso conjunto de
datos.

## Determinación del objetivo

El reto es reproducir esta categorización con un conjunto de
características más limitado. Esto puede proveer nuevas y más robustas
maneras de categorizar los datos.

## Determinación del criterio de éxito

La predicción se evalúa usando la pérdida logarítmica multiclase. Por
cada visita se debe ingresar un conjunto de probabilidades por cada
*tipo de viaje*.

## Plan del proyecto

# Compresión de los datos.

## Análisis Exploratorio de datos

Empezamos viendo la estructura de la tabla de entrenamiento.

``` r
head(train)
```

    ## # A tibble: 6 x 7
    ##   TripType VisitNumber Weekday     Upc ScanCount DepartmentDescr~
    ##   <fct>          <int> <fct>     <dbl>     <int> <fct>           
    ## 1 999                5 Friday  6.81e10        -1 FINANCIAL SERVI~
    ## 2 30                 7 Friday  6.05e10         1 SHOES           
    ## 3 30                 7 Friday  7.41e 9         1 PERSONAL CARE   
    ## 4 26                 8 Friday  2.24e 9         2 PAINT AND ACCES~
    ## 5 26                 8 Friday  2.01e 9         2 PAINT AND ACCES~
    ## 6 26                 8 Friday  2.01e 9         2 PAINT AND ACCES~
    ## # ... with 1 more variable: FinelineNumber <int>

*TripType* es la etiqueta sobre la que se deben clasificar cada una de
las visitas, identificadas por la variable *VisitNumber*. *Weekday* es
el día de la semana en el que se realizó la visita. *Upc* es el código
universal del producto, que identifica sobre qué producto se hizo la
transacción. *ScanCount* es el número de artículos vendidos (siendo
devolución cuando este número es negativo). Tanto
*DepartmentDescription* como *FilelineNumber* identifican el
departaminto y subdepartamento al que pertenece el artículo.

Ahora veamos algunas propiedades del conjunto de
    datos.

``` r
str(train)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 647054 obs. of  7 variables:
    ##  $ TripType             : Factor w/ 38 levels "3","4","5","6",..: 38 23 23 19 19 19 19 19 19 19 ...
    ##  $ VisitNumber          : int  5 7 7 8 8 8 8 8 8 8 ...
    ##  $ Weekday              : Factor w/ 7 levels "Friday","Saturday",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Upc                  : num  6.81e+10 6.05e+10 7.41e+09 2.24e+09 2.01e+09 ...
    ##  $ ScanCount            : int  -1 1 1 2 2 2 1 1 1 -1 ...
    ##  $ DepartmentDescription: Factor w/ 69 levels "FINANCIAL SERVICES",..: 1 2 3 4 4 4 4 4 4 4 ...
    ##  $ FinelineNumber       : int  1000 8931 4504 3565 1017 1017 1017 2802 4501 3565 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   TripType = col_factor(levels = c("3", "4", "5", "6", "7", "8", "9", "12", "14", "15", "18", 
    ##   ..     "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", 
    ##   ..     "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", 
    ##   ..     "41", "42", "43", "44", "999"), ordered = FALSE, include_na = FALSE),
    ##   ..   VisitNumber = col_integer(),
    ##   ..   Weekday = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
    ##   ..   Upc = col_double(),
    ##   ..   ScanCount = col_integer(),
    ##   ..   DepartmentDescription = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
    ##   ..   FinelineNumber = col_integer()
    ##   .. )

En la tabla anterior podemos ver que existen 38 diferentes tipos de
visitas, así como 69 departamentos distintos. A continuación veremos
algunas estadísticas de los
    datos.

``` r
summary(train)
```

    ##     TripType       VisitNumber          Weekday            Upc           
    ##  40     :174164   Min.   :     5   Friday   : 96247   Min.   :8.340e+02  
    ##  39     : 95504   1st Qu.: 49268   Saturday :122096   1st Qu.:3.400e+09  
    ##  37     : 38954   Median : 97074   Sunday   :133975   Median :7.050e+09  
    ##  38     : 29565   Mean   : 96168   Monday   : 83130   Mean   :3.061e+10  
    ##  25     : 27609   3rd Qu.:144316   Tuesday  : 72529   3rd Qu.:3.007e+10  
    ##  7      : 23199   Max.   :191347   Wednesday: 71115   Max.   :9.790e+11  
    ##  (Other):258059                    Thursday : 67962   NA's   :4129       
    ##    ScanCount               DepartmentDescription FinelineNumber
    ##  Min.   :-12.000   GROCERY DRY GOODS  : 70402    Min.   :   0  
    ##  1st Qu.:  1.000   DSD GROCERY        : 68332    1st Qu.:1404  
    ##  Median :  1.000   PRODUCE            : 51115    Median :3352  
    ##  Mean   :  1.109   DAIRY              : 43820    Mean   :3727  
    ##  3rd Qu.:  1.000   PERSONAL CARE      : 41969    3rd Qu.:5501  
    ##  Max.   : 71.000   IMPULSE MERCHANDISE: 28712    Max.   :9998  
    ##                    (Other)            :342704    NA's   :4129

De las estadísticas mostradas es importante hacer notar que el número de
cada tipo de visitas es inexacto, dado que cada observación de la tabla
de datos es por transacción y no por visita. El número de visitas
registradas son solo 95674. También es interesante ver el número de
ventas por tipo de visita. Abajo se muestra el tanto del número de
visitas como el número de transacciones y las ventas como el por tipo de
de viaje.

``` r
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
```

![](Comprension_del_negocio_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

A esta información podemos agregar el día de la semana

``` r
train %>%
  group_by(TripType, Weekday) %>% 
  summarise(n = n_distinct(VisitNumber)) %>% 
  ggplot() +
  geom_bar(aes(x = TripType, y = n), stat = 'identity') +
  facet_wrap(~Weekday) +
  theme_hc()
```

![](Comprension_del_negocio_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Hay 97715 distintos tipos de productos en los datos Hay 69 distintos
tipos de productos en los datos

Ahora veamos la relación entre el tipo de viaje y el Departament, con
base el número de artículos comprados.

``` r
train %>%
  group_by(TripType, DepartmentDescription) %>% 
  summarise(n = sum(ScanCount)) %>% 
  semi_join(train %>% group_by(DepartmentDescription) %>% summarise(n=n()) %>% filter(n>1000),
            by = 'DepartmentDescription') %>% 
  ggplot(aes(x = TripType, y = DepartmentDescription, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c()
```

![](Comprension_del_negocio_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
