#------------------------Paso 1: Generación del archivo CSV con datos aleatorios-------------------------------------------
# Cargar las bibliotecas necesarias
library(tidyverse)

# Definir las categorías y nombres de productos
categorias <- c("Electrónica", "Computadoras", "Accesorios", "Telefonía", "Videojuegos")
productos <- list(
  "Electrónica" = c("Televisor", "Sistema de Sonido", "Cámara"),
  "Computadoras" = c("Laptop", "Desktop", "Tablet"),
  "Accesorios" = c("Mouse", "Teclado", "Auriculares"),
  "Telefonía" = c("Smartphone", "Teléfono Fijo", "Walkie-Talkie"),
  "Videojuegos" = c("Consola", "Juego", "Control")
)

# Generar datos aleatorios
set.seed(123)  # Para reproducibilidad
n <- 1000  # Número de registros

id_venta <- seq(1, n)
id_producto <- sample(1:100, n, replace = TRUE)
categoria <- sample(categorias, n, replace = TRUE)
nombre_producto <- sapply(categoria, function(x) sample(productos[[x]], 1))
precio_unitario <- round(runif(n, 10, 1000), 2)
cantidad_vendida <- sample(1:20, n, replace = TRUE)

# Crear el data frame
ventas <- data.frame(ID_Venta = id_venta, 
                     ID_Producto = id_producto, 
                     Nombre_Producto = nombre_producto, 
                     Categoría = categoria, 
                     Precio_Unitario = precio_unitario, 
                     Cantidad_Vendida = cantidad_vendida)

# Guardar en un archivo CSV
write.csv(ventas, "ventas.csv", row.names = FALSE)

#---------------------------Paso 2: Carga de los datos desde el archivo CSV y almacenamiento en un data frame-----------------------------------

# Cargar los datos desde el archivo CSV
ventas <- read.csv("ventas.csv")

# Ver los primeros registros del data frame
head(ventas)

#---------------------------Paso 3: Limpieza de datos y manejo de valores faltantes------------------------------------
# Verificar si hay valores faltantes
sum(is.na(ventas))

# Convertir tipos de datos si es necesario
# En este caso, todos los tipos parecen adecuados
str(ventas)

#--------------------------Paso 4: Total de ventas por categoría de producto utilizando dplyr--------------------------------
library(dplyr)

# Calcular el total de ventas por categoría
total_ventas_categoria <- ventas %>%
  group_by(Categoría) %>%
  summarise(Total_Ventas = sum(Precio_Unitario * Cantidad_Vendida))

# Ver el resultado
total_ventas_categoria
#-------------------------Paso 5: Precio promedio de los productos en cada categoría utilizando dplyr---------------------------
# Calcular el precio promedio por categoría
precio_promedio_categoria <- ventas %>%
  group_by(Categoría) %>%
  summarise(Precio_Promedio = mean(Precio_Unitario))

# Ver el resultado
precio_promedio_categoria
#-------------------------Paso 6: Identificación de los 5 productos más vendidos y menos vendidos utilizando dplyr--------------------
# Calcular la cantidad total vendida por producto
ventas_por_producto <- ventas %>%
  group_by(Nombre_Producto) %>%
  summarise(Cantidad_Total_Vendida = sum(Cantidad_Vendida)) %>%
  arrange(desc(Cantidad_Total_Vendida))

# 5 productos más vendidos
top_5_mas_vendidos <- head(ventas_por_producto, 5)

# 5 productos menos vendidos
top_5_menos_vendidos <- tail(ventas_por_producto, 5)

# Ver los resultados
top_5_mas_vendidos
top_5_menos_vendidos
#-----------------------Paso 7: Creación de una visualización básica con ggplot2----------------------------
library(ggplot2)

# Crear una visualización de la distribución de precios por categoría
ggplot(ventas, aes(x = Categoría, y = Precio_Unitario)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de Precios por Categoría", x = "Categoría", y = "Precio Unitario")

#-----------------------Paso 8: Guardado de los resultados en un archivo CSV------------------------
# Guardar los resultados en archivos CSV
write.csv(total_ventas_categoria, "total_ventas_categoria.csv", row.names = FALSE)
write.csv(precio_promedio_categoria, "precio_promedio_categoria.csv", row.names = FALSE)
write.csv(top_5_mas_vendidos, "top_5_mas_vendidos.csv", row.names = FALSE)
write.csv(top_5_menos_vendidos, "top_5_menos_vendidos.csv", row.names = FALSE)
