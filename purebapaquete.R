# Cargar librerías necesarias
library(DMwR)
library(mlbench)

# Cargar el dataset original
data("BostonHousing")

# Ver las primeras filas del original
head(BostonHousing)

# Hacer una copia para trabajar (el original queda intacto)
bh_copy <- BostonHousing

# Introducir algunos NA artificialmente en la copia
set.seed(123)
indices_na <- sample(1:nrow(bh_copy), 20)
bh_copy$crim[indices_na] <- NA   # metemos NA en la variable crim

# Verificar cuántos NA hay en la copia
cat("NA en 'crim' antes de imputación:", sum(is.na(bh_copy$crim)), "\n")

# Aplicar imputación central en la copia
bh_copy_imp <- centralImputation(bh_copy)

# Verificar que ya no hay NA en la copia imputada
cat("NA en 'crim' después de imputación:", sum(is.na(bh_copy_imp$crim)), "\n")

# Mostrar primeras filas después de imputación
head(bh_copy_imp)

cat("\n✅ ¡El paquete DMwR funciona bien con BostonHousing en la copia!\n")




