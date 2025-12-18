library(tidyverse)

# Cargar y sustituir NULL por NA
unir_master <- read_csv("C:/Users/veronicob/Downloads/unir_master.csv", na = c("", "NULL", "null", NA))

# 🔹 Mostrar nombres de columnas
cat("Columnas del archivo unir_master:\n")
print(colnames(unir_master))

# 🔹 Mostrar estructura de los datos
cat("\nEstructura del dataset:\n")
str(unir_master)

# 🔹 Mostrar primeras filas (opcional)
cat("\nPrimeras filas del dataset:\n")
print(head(unir_master))

#lo corri en la consola source("C:\\Users\\veronicob\\Desktop\\r-exercise\\explaration.R"),

#file.choose()
