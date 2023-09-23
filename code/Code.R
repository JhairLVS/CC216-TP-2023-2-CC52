# Istalamos las librerias necesarias

install.packages("outliers")
install.packages("ggplot2")
install.packages("dplyr")

# Cargamos las librerias necesarias

library(outliers)
library(ggplot2)
library(dplyr)

# Cargamos los datos

data <- read.csv("C:/Users/asus/Downloads/hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)

# Inspeccionamos los datos

str(data) 
head(data, n = 10) 
summary(data)
colnames(data)
column_types <- sapply(data, class)
print(column_types)

# Indentificación de datos faltantes en general
# Contamos los valores faltantes en cada columna del conjunto de datos 'data'

missing_values <- colSums(is.na(data))

# Mostramos la cantidad de valores faltantes por columna

print(missing_values)

# Eliminamos filas con valores NA y almacenar el resultado en un nuevo conjunto de datos

data_sin_na <- na.omit(data)

# Otra alternativa es:
# Filtrar las filas completas (sin valores faltantes) en el conjunto de datos

data_filtras_filas_completas <- data[complete.cases(data), ]

# Mostramos las primeras filas del conjunto de datos filtrado

head(data_filtras_filas_completas)

# Creamos una lista vacía para almacenar las tablas de frecuencia

frequency_tables <- list()

# Iteramos a través de cada columna del conjunto de datos

for (col_name in colnames(data_sin_na)) {
  frequency_tables[[col_name]] <- table(data_sin_na[[col_name]])
}

# Mostramos las tablas de frecuencia para cada columna

for (col_name in colnames(data_sin_na)) {
  cat("Tabla de frecuencia para la columna:", col_name, "\n")
  print(frequency_tables[[col_name]])
}

# Identificamos los valores atipicos
# Identificamos outliers en las columnas numéricas y enteras
# Seleccionamos todas las columnas numéricas y enteras

numeric_integer_columns <- data_sin_na[sapply(data_sin_na, function(x) is.numeric(x) || is.integer(x))]

# Creamos una lista para almacenar los resultados de outliers

outliers_results <- list()

# Iteramos a través de las columnas numéricas y enteras

for (col_name in colnames(numeric_integer_columns)) {
  outliers <- outlier(numeric_integer_columns[[col_name]])
  outliers_results[[col_name]] <- outliers
  print(paste("Outliers en la columna", col_name))
  print(outliers)
}

#Eliminacion de outliers

data_sin_outliers <- data_sin_na[data_sin_na$required_car_parking_spaces < 8, ]

# Establecemos un límite superior para niños y bebés

limite_superior <- 3

data_sin_outliers$children[data_sin_outliers$children > limite_superior] <- limite_superior
data_sin_outliers$babies[data_sin_outliers$babies > limite_superior] <- limite_superior

table(data_sin_outliers$required_car_parking_spaces)
table(data_sin_outliers$children)
table(data_sin_outliers$babies)

# Graficos

# Reservas por Tipo de Hotel 

reservas_por_hotel <- as.data.frame(table(data_sin_outliers$hotel))

colnames(reservas_por_hotel) <- c("Tipo_de_Hotel", "Cantidad_de_Reservas")

colores <- c("City Hotel" = "skyblue", "Resort Hotel" = "salmon")

ggplot(data = reservas_por_hotel, aes(x = Tipo_de_Hotel, y = Cantidad_de_Reservas, fill = Tipo_de_Hotel)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colores) + 
  labs(title = "Reservas por Tipo de Hotel",
       x = "Tipo de Hotel",
       y = "Cantidad de Reservas")

# Variación de la Demanda por Año

ggplot(data = data_sin_outliers, aes(x = arrival_date_year, fill = hotel)) +
  geom_bar(position = "stack") +
  labs(title = "Variación de la Demanda por Año",
       x = "Año",
       y = "Cantidad de Reservas",
       fill = "Tipo de Hotel") +
  theme_minimal()

# Reservas con Niños o Bebés

# Creamos un nuevo dataframe con los datos necesarios
datos_grafico <- data.frame(
  Categoria = c("Con Niños o Bebés", "Sin Niños ni Bebés"),
  Cantidad = c(sum(data_sin_outliers$children > 0 | data_sin_outliers$babies > 0),
               sum(data_sin_outliers$children == 0 & data_sin_outliers$babies == 0))
)

ggplot(datos_grafico, aes(x = Categoria, y = Cantidad, fill = Categoria)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Reservas con Niños o Bebés",
    y = "Número de Reservas",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Variación de Reservas por Mes

# Creamos un vector con el orden deseado de los meses

orden_meses <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Convertimos la columna arrival_date_month a factor con niveles ordenados

data_sin_outliers$arrival_date_month <- factor(data_sin_outliers$arrival_date_month, levels = orden_meses)

# Luego, ordenamos el conjunto de datos por la columna arrival_date_month

data_sin_outliers <- data_sin_outliers[order(data_sin_outliers$arrival_date_month), ]

# Creamos un resumen de reservas por mes

reservas_por_mes <- data_sin_outliers %>%
  group_by(arrival_date_year, arrival_date_month) %>%
  summarise(Cantidad_de_Reservas = n())

ggplot(data = reservas_por_mes, aes(x = arrival_date_month, y = Cantidad_de_Reservas, group = arrival_date_year, color = arrival_date_year)) +
  geom_line() +
  labs(title = "Variación de Reservas por Mes",
       x = "Mes",
       y = "Cantidad de Reservas",
       color = "Año") +
  theme_minimal()

# Reservas por Mes

# Creamos un resumen de reservas por mes
resumen_reservas <- data_sin_outliers %>%
  group_by(arrival_date_month) %>%
  summarise(Reservas = n())

ggplot(resumen_reservas, aes(x = arrival_date_month, y = Reservas, group = 1)) +
  geom_line() +
  labs(
    title = "Reservas por Mes",
    x = "Mes",
    y = "Número de Reservas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Importancia de Espacios de Estacionamiento

# Creamos un nuevo dataframe con los datos necesarios

datos_grafico <- data.frame(
  Espacios_Estacionamiento = factor(data_sin_outliers$required_car_parking_spaces, levels = c(0, 1)),
  Cantidad_Reservas = rep(1, nrow(data_sin_outliers))
)

ggplot(datos_grafico, aes(x = Espacios_Estacionamiento, fill = Espacios_Estacionamiento)) +
  geom_bar() +
  labs(
    title = "Importancia de Espacios de Estacionamiento",
    x = "Espacios de Estacionamiento",
    y = "Número de Reservas",
    fill = "Espacios de Estacionamiento"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Cancelaciones de Reservas por Mes

# Creamos un dataframe resumen que cuente las cancelaciones por mes

resumen_cancelaciones <- data_sin_outliers %>%
  group_by(arrival_date_month) %>%
  summarise(Cancelaciones = sum(is_canceled))

ggplot(resumen_cancelaciones, aes(x = arrival_date_month, y = Cancelaciones, group = 1)) +
  geom_line() +
  labs(
    title = "Cancelaciones de Reservas por Mes",
    x = "Mes",
    y = "Número de Cancelaciones"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
