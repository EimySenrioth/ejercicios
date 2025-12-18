# 1. Instala la librería VIM
#install.packages("VIM")
#install.packages("rpart")
#install.packages("mice")

# 2. Carga la librería
library(VIM)

# 3. Carga los datos airquality
data(airquality)

# 4. Haz una copia de la base de datos original en una variable llamada oairquality
oairquality <- airquality

# 5. Utiliza una instrucción o función para contar los datos faltantes en la base de 
# datos airquality
total_na <- sum(is.na(airquality))
print(total_na)

# 6. Crea la matriz con los niveles de faltabilidad
miss_matrix <- aggr(airquality, plot = FALSE)
print(miss_matrix)

# 7. Utiliza una instrucción o función para contar los faltantes por columna de 
#la base de datos airquality
col_na <- colSums(is.na(airquality))
print(col_na)

# 8. Omite las filas con NA’s (datos faltantes) sin eliminarlas y 
#guarda el resultado en una variable llamada temp1
temp1 <- na.omit(airquality)

# 9. Haz imputación por medidas de tendencia central (media y mediana) 
# de la base de datos airquality
air_mean <- airquality
air_mean$Ozone[is.na(air_mean$Ozone)] <- mean(air_mean$Ozone, na.rm = TRUE)
air_mean$Solar.R[is.na(air_mean$Solar.R)] <- mean(air_mean$Solar.R, na.rm = TRUE)
air_median <- airquality
air_median$Ozone[is.na(air_median$Ozone)] <- median(air_median$Ozone, na.rm = TRUE)
air_median$Solar.R[is.na(air_median$Solar.R)] <- median(air_median$Solar.R, na.rm = TRUE)

# 10. Haz el cálculo de precisión
complete_data <- temp1
sim_data <- complete_data
set.seed(123) 
sim_data$Ozone[sample(1:nrow(sim_data), 37)] <- NA
sim_data$Solar.R[sample(1:nrow(sim_data), 7)] <- NA
# Imputar con kNN
sim_knn <- kNN(sim_data, variable = c("Ozone", "Solar.R"), k = 5)
imputed_ozone <- sim_knn$Ozone[is.na(sim_data$Ozone)]
original_ozone <- complete_data$Ozone[match(names(imputed_ozone), rownames(complete_data))]
imputed_solar <- sim_knn$Solar.R[is.na(sim_data$Solar.R)]
original_solar <- complete_data$Solar.R[match(names(imputed_solar), rownames(complete_data))]
rmse_ozone <- sqrt(mean((imputed_ozone - original_ozone)^2, na.rm = TRUE))
rmse_solar <- sqrt(mean((imputed_solar - original_solar)^2, na.rm = TRUE))
print(paste("RMSE Ozone (kNN):", rmse_ozone))
print(paste("RMSE Solar.R (kNN):", rmse_solar))

# 11. Haz imputación por modelo, utiliza KNN en las dos columnas con datos faltantes
air_knn <- kNN(airquality, variable = c("Ozone", "Solar.R"), k = 5)

# 12. Haz imputación por modelo, utiliza r-part en las dos columnas con datos faltantes
library(rpart)
model_ozone <- rpart(Ozone ~ Solar.R + Wind + Temp + Month + Day, data = airquality[!is.na(airquality$Ozone), ])
pred_ozone <- predict(model_ozone, airquality[is.na(airquality$Ozone), ])
air_rpart <- airquality
air_rpart$Ozone[is.na(air_rpart$Ozone)] <- pred_ozone
model_solar <- rpart(Solar.R ~ Ozone + Wind + Temp + Month + Day, data = air_rpart[!is.na(air_rpart$Solar.R), ])
pred_solar <- predict(model_solar, air_rpart[is.na(air_rpart$Solar.R), ])
air_rpart$Solar.R[is.na(air_rpart$Solar.R)] <- pred_solar

# 13. Haz imputación por modelo, utiliza mice en las dos columnas con datos faltantes
library(mice)
imp_mice <- mice(airquality, m = 1, method = "pmm", maxit = 5, seed = 123)
air_mice <- complete(imp_mice)

# 14. Utiliza la instrucción marginplot(airquality[c("Ozone","Solar.R")]) 
#para visualizar los datos existentes y faltantes
marginplot(airquality[c("Ozone", "Solar.R")])

# 15. Envía el archivo .R con las instrucciones de tu examen realizado
#lo corri en la terminal source("C:\\Users\\veronicob\\Desktop\\r-exercise\\examt.R")

#file.choose() con eso me da la ruta el archivo.
