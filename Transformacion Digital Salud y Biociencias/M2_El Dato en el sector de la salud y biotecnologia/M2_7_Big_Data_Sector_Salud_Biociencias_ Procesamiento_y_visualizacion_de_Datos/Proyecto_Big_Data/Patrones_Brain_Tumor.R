---
# title: "PATRONES DE RECURRENCIA BASADOS EN LAS ETAPAS DEL TUMOR CEREBRAL"
# author: "Pablo López Vaca"
# date: "2024-06-12"
# output: html_document
---

# Primer paso: Cargar las librerías necesarias
  
install.packages("dplyr")
install.packages("factoextra")
library(dplyr)
library(ggplot2)
library(survival)
library(cluster)
library(factoextra)


# Segundo paso: Limpieza y procesamiento de datos 

  # 1. Eliminar filas con cualquier NA
cleaned_BrainTumor <- na.omit(BrainTumor)

  # 2. Eliminar columnas con cualquier NA
cleaned_data <- cleaned_BrainTumor[, colSums(is.na(cleaned_BrainTumor)) == 0]

  # 3. Verificar que no hay NA
sum(is.na(cleaned_data))

  # 4. Verificar filas duplicadas
duplicated_rows <- cleaned_BrainTumor[duplicated(cleaned_BrainTumor), ]


# Tercer Paso: Análisis exploratorio de datos

  # 1. Crear dos tablas separadas basadas en el género
male_data <- filter(cleaned_BrainTumor, Gender == "Male")
female_data <- filter(cleaned_BrainTumor, Gender == "Female")

  # 2. Distribución del género
ggplot(cleaned_BrainTumor, aes(x = Gender)) + 
  geom_bar(fill = "lightblue", color = "black") + 
  labs(title = "Distribución de Género", x = "Género", y = "Número de pacientes")

  # 3. Distribución de la edad
ggplot(cleaned_BrainTumor, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribución de Edad", x = "Edad", y = "Número de Pacientes")

  # 4. Distribución del tipo de tumor
ggplot(cleaned_BrainTumor, aes(x = `Tumor Type`)) + 
  geom_bar(fill = "lightgreen", color = "black") + 
  labs(title = "Distribución del Tipo de Tumor", x = "Tipo de Tumor", y = "Número de pacientes")

  # 5. Distribución del estadio del tumor
ggplot(cleaned_BrainTumor, aes(x = `Tumor Grade`)) + 
  geom_bar(fill = "lightcoral", color = "black") + 
  labs(title = "Distribución del Estadio del Tumor", x = "Estadio del Tumor", y = "Número de pacientes")

  # 6. Distribución de la frecuencia de recurrencia, Análisis de Riesgo de Recurrencia
ggplot(cleaned_BrainTumor, aes(x = `Time to Recurrence (months)`)) + 
  geom_histogram(binwidth = 3, fill = "purple", color = "black") + 
  labs(title = "Distribución del Tiempo hasta la Recurrencia", x = "Tiempo hasta la Recurrencia (meses)", y = "Número de pacientes")

  # 7. Costos y Eficacia de Tratamientos
  # Análisis de tratamientos y resultados
ggplot(cleaned_BrainTumor, aes(x = Treatment, fill = `Treatment Outcome`)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Eficacia de Tratamientos según Resultados", x = "Tratamiento", y = "Número de Pacientes")

# Cuarto paso: Historiogramas, bloxplot

  # Boxplot 1: Edad por Género
ggplot(cleaned_BrainTumor, aes(x = Gender, y = Age, fill = Gender)) + 
  geom_boxplot() + 
  labs(title = "Distribución de la Edad por Género", x = "Género", y = "Edad") +
  theme_minimal()

  # Boxplot 2: Tiempo de Supervivencia por Tipo de Tumor
ggplot(cleaned_BrainTumor, aes(x = `Tumor Type`, y = `Survival Time (months)`, fill = `Tumor Type`)) + 
  geom_boxplot() + 
  labs(title = "Distribución del Tiempo de Supervivencia por Tipo de Tumor", x = "Tipo de Tumor", y = "Tiempo de Supervivencia (meses)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Quinto paso: Aplicación de datos

# Optimización de Primas de Seguros
# Análisis de la distribución de la supervivencia según el tipo de tumor
ggplot(cleaned_BrainTumor, aes(x = `Tumor Type`, y = `Survival Time (months)`, fill = `Tumor Type`)) + 
  geom_boxplot() + 
  labs(title = "Distribución del Tiempo de Supervivencia por Tipo de Tumor", x = "Tipo de Tumor", y = "Tiempo de Supervivencia (meses)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Desarrollo de Productos Personalizados
# Análisis de la eficacia de tratamientos según resultados
ggplot(cleaned_BrainTumor, aes(x = Treatment, fill = `Treatment Outcome`)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Eficacia de Tratamientos según Resultados", x = "Tratamiento", y = "Número de Pacientes")













# Matriz de correlación
# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(corrplot)

# Segundo paso: Limpieza y procesamiento de datos

# 1. Eliminar filas con cualquier NA
cleaned_BrainTumor <- na.omit(BrainTumor)

# Seleccionar solo las columnas numéricas para la correlación
numeric_data <- select(cleaned_BrainTumor, Age, `Time to Recurrence (months)`, `Survival Time (months)`)

# Calcular la matriz de correlación
cor_matrix <- cor(numeric_data)

# Imprimir la matriz de correlación
print(cor_matrix)

# Visualizar la matriz de correlación
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)


















# Distribución de la edad
ggplot(cleaned_BrainTumor, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia")

# Distribución de la edad (male_data)
ggplot(male_data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribución de Edad", x = "Edad", y = "Numero de pacientes")

# Distribución de la edad (female_data)
ggplot(female_data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribución de Edad", x = "Edad", y = "Numero de pacientes")





# Crear el histograma de la distribución de la edad
ggplot(cleaned_BrainTumor, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  labs(title = "Distribución de Edad", x = "Edad", y = "Número de pacientes")



# Distribución del género
ggplot(cleaned_BrainTumor, aes(x = Gender)) + 
  geom_bar(fill = "lightblue", color = "black") + 
  labs(title = "Distribución de Género", x = "Género", y = "Número de Pacientes")

# Modelos Predictivos de Supervivencia
# Modelos de supervivencia (esto es solo un ejemplo básico, los modelos reales serían más complejos)
surv_model <- survfit(Surv(`Survival Time (months)`, Status) ~ 1, data = cleaned_BrainTumor)
plot(surv_model, main = "Curva de Supervivencia", xlab = "Tiempo (meses)", ylab = "Supervivencia")

# Frecuencia de Recurrencia por Tipo de Tumor
# Frecuencia de recurrencia por tipo de tumor
ggplot(cleaned_BrainTumor, aes(x = `Tumor Type`, fill = factor(`Time to Recurrence (months)`))) + 
  geom_bar(position = "dodge") + 
  labs(title = "Frecuencia de Recurrencia por Tipo de Tumor", x = "Tipo de Tumor", y = "Número de Pacientes", fill = "Recurrencia")

# Crear la columna `Status` basada en `Survival Time (months)`
# Aquí asumimos que si `Survival Time (months)` es mayor que 0, el paciente ha fallecido (esto es solo un supuesto)
cleaned_BrainTumor$Status <- ifelse(cleaned_BrainTumor$`Survival Time (months)` > 0, 1, 0)

# Crear el objeto Surv y ajustar el modelo de supervivencia
surv_model <- survfit(Surv(`Survival Time (months)`, Status) ~ 1, data = cleaned_BrainTumor)

# Graficar la curva de supervivencia
plot(surv_model, main = "Curva de Supervivencia", xlab = "Tiempo (meses)", ylab = "Supervivencia")


# Seleccionar variables relevantes y normalizarlas
data_cluster <- scale(cleaned_BrainTumor[, c('Age', 'Survival Time (months)', 'Time to Recurrence (months)')])

# Realizar clustering k-means
set.seed(123)
km_res <- kmeans(data_cluster, centers = 3, nstart = 25)

# Visualizar los clusters
fviz_cluster(km_res, data = data_cluster, geom = "point", stand = FALSE) +
  labs(title = "Clustering de Pacientes")

# Código para Describir los Clusters
# Añadir el resultado del clustering al dataset original
cleaned_BrainTumor$cluster <- as.factor(km_res$cluster)

# Resumen estadístico de cada cluster
cluster_summary <- cleaned_BrainTumor %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_age = mean(Age, na.rm = TRUE),
    mean_survival_time = mean(`Survival Time (months)`, na.rm = TRUE),
    mean_time_to_recurrence = mean(`Time to Recurrence (months)`, na.rm = TRUE),
    .groups = 'drop'
  )

print(cluster_summary)

# Calcular la suma de cuadrados intra-cluster
wss <- km_res$tot.withinss

# Calcular la suma de cuadrados inter-cluster
bss <- km_res$betweenss

cat("Within-cluster sum of squares (WSS):", wss, "\n")
cat("Between-cluster sum of squares (BSS):", bss, "\n")

# Plot del codo para determinar el número óptimo de clusters
fviz_nbclust(data_cluster, kmeans, method = "wss") +
  labs(title = "El método del codo")


#Visualización de Boxplots por Cluster
# Boxplot de edad por cluster
ggplot(cleaned_BrainTumor, aes(x = cluster, y = Age, fill = cluster)) + 
  geom_boxplot() + 
  labs(title = "Distribución de Edad por Cluster", x = "Cluster", y = "Edad")

# Boxplot de tiempo de supervivencia por cluster
ggplot(cleaned_BrainTumor, aes(x = cluster, y = `Survival Time (months)`, fill = cluster)) + 
  geom_boxplot() + 
  labs(title = "Distribución de Tiempo de Supervivencia por Cluster", x = "Cluster", y = "Tiempo de Supervivencia (meses)")

# Boxplot de tiempo hasta la recurrencia por cluster
ggplot(cleaned_BrainTumor, aes(x = cluster, y = `Time to Recurrence (months)`, fill = cluster)) + 
  geom_boxplot() + 
  labs(title = "Distribución de Tiempo hasta la Recurrencia por Cluster", x = "Cluster", y = "Tiempo hasta la Recurrencia (meses)")




