
# Cargamos los datos de entrenamiento
train <- read_csv('./data/train.csv',
                  col_types = cols(
                    TripType = col_factor(levels = l),
                    VisitNumber = col_integer(),
                    Weekday = col_factor(),
                    ScanCount = col_integer(),
                    DepartmentDescription = col_factor(),
                    FinelineNumber = col_integer()
                  ))