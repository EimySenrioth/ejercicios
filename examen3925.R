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
na_ozone_pos <- which(is.na(oairquality$Ozone))
na_solar_pos <- which(is.na(oairquality$Solar.R))
complete_data <- temp1  
sim_data <- complete_data
set.seed(123)#puse una semilla para que mi resultado cuando usted lo corra siempre sean los mismos  
sim_data$Ozone[sample(1:nrow(sim_data), 37)] <- NA
sim_data$Solar.R[sample(1:nrow(sim_data), 7)] <- NA
sim_mean <- sim_data
sim_mean$Ozone[is.na(sim_mean$Ozone)] <- mean(sim_mean$Ozone, na.rm = TRUE)
sim_mean$Solar.R[is.na(sim_mean$Solar.R)] <- mean(sim_mean$Solar.R, na.rm = TRUE)
sim_median <- sim_data
sim_median$Ozone[is.na(sim_median$Ozone)] <- median(sim_median$Ozone, na.rm = TRUE)
sim_median$Solar.R[is.na(sim_median$Solar.R)] <- median(sim_median$Solar.R, na.rm = TRUE)
rmse_mean_ozone <- sqrt(mean((sim_mean$Ozone[is.na(sim_data$Ozone)] - complete_data$Ozone[is.na(sim_data$Ozone)])^2))
rmse_mean_solar <- sqrt(mean((sim_mean$Solar.R[is.na(sim_data$Solar.R)] - complete_data$Solar.R[is.na(sim_data$Solar.R)])^2))
rmse_median_ozone <- sqrt(mean((sim_median$Ozone[is.na(sim_data$Ozone)] - complete_data$Ozone[is.na(sim_data$Ozone)])^2))
rmse_median_solar <- sqrt(mean((sim_median$Solar.R[is.na(sim_data$Solar.R)] - complete_data$Solar.R[is.na(sim_data$Solar.R)])^2))
print(paste("RMSE Media Ozone:", rmse_mean_ozone))
print(paste("RMSE Media Solar.R:", rmse_mean_solar))
print(paste("RMSE Mediana Ozone:", rmse_median_ozone))
print(paste("RMSE Mediana Solar.R:", rmse_median_solar))
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
#lo corri en la terminal source("C:\\Users\\veronicob\\Desktop\\r-exercise\\examen3925.R")
#file.choose() con eso me da la ruta el archivo.

