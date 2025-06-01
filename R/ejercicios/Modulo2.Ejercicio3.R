#Jorge Fausto Hernandez Andrade
library(dslabs)
library(dplyr)
library(ggplot2)

data(murders)
murders

# Ver las regiones disponibles
table(murders$region)

# Calcular la proporción de estados en North Central
total_estados <- nrow(murders)
estados_north_central <- sum(murders$region == "North Central")

proporcion <- estados_north_central / total_estados
proporcion_porcentaje <- proporcion * 100

print(paste("Total de estados:", total_estados))
print(paste("Estados en North Central:", estados_north_central))
print(paste("Proporción:", round(proporcion, 3)))
print(paste("Porcentaje:", round(proporcion_porcentaje, 1), "%"))

# También puedes usar table para ver todas las proporciones
prop.table(table(murders$region)) * 100

ggplot(murders, aes(x = region)) +
  geom_bar() +
  labs(title = "Distribución de Estados por Región",
       x = "Región",
       y = "Número de Estados") +
  theme_minimal()

#Opción 3: 25%

table(murders$region)

#Opción 2: El gráfico muestra solo cuatro números con un diagrama de barras.

data(heights)

# Filtrar solo las alturas masculinas
male_heights <- heights$height[heights$sex == "Male"]

# Crear la eCDF para las alturas masculinas
plot(ecdf(male_heights), 
     main = "eCDF para Alturas Masculinas",
     xlab = "Altura (pulgadas)",
     ylab = "Proporción acumulativa")

abline(v = 75, col = "red", lty = 2)

proporcion_menor_75 <- mean(male_heights < 75)
porcentaje_menor_75 <- proporcion_menor_75 * 100

print(paste("Proporción de hombres < 75 pulgadas:", round(proporcion_menor_75, 3)))
print(paste("Porcentaje de hombres < 75 pulgadas:", round(porcentaje_menor_75, 1), "%"))

ecdf_male <- ecdf(male_heights)
porcentaje_ecdf <- ecdf_male(75) * 100
print(paste("Usando eCDF:", round(porcentaje_ecdf, 1), "%"))

summary(male_heights)

#Opción 3: 80%

# Filtrar solo las alturas masculinas
male_heights <- heights$height[heights$sex == "Male"]

# Calcular la mediana
mediana_masculina <- median(male_heights)
print(paste("Mediana de alturas masculinas:", round(mediana_masculina, 1), "pulgadas"))

# Redondear a la pulgada más cercana
mediana_redondeada <- round(mediana_masculina)
print(paste("Mediana redondeada:", mediana_redondeada, "pulgadas"))

# Verificar que efectivamente el 50% está por debajo
proporcion_debajo_mediana <- mean(male_heights < mediana_masculina)
print(paste("Proporción por debajo de la mediana:", round(proporcion_debajo_mediana, 3)))

percentil_50 <- quantile(male_heights, 0.5)
print(paste("Percentil 50 (mediana):", round(percentil_50, 1)))

#Opción 3: 69 pulgadas
#La mediana de las alturas masculinas es aproximadamente 69 pulgadas. 
#En el gráfico eCDF, si trazas una línea horizontal en y = 0.5 (50%), 
#esta intersectará la curva aproximadamente en x = 69 pulgadas. 
#Esta es la altura donde exactamente la mitad de los masculinos 
#son más altos y la mitad son más bajos.

data("murders")
str(murders)
head(murders)

# Calcular la tasa de asesinatos por 100,000 habitantes
murders <- murders |>
  mutate(rate = total / population * 100000)

# Crear la eCDF para las tasas de asesinato
plot(ecdf(murders$rate), 
     main = "eCDF de Tasas de Asesinato por Estado",
     xlab = "Tasa de asesinatos por 100,000 habitantes",
     ylab = "Proporción acumulativa")

# Agregar línea vertical en 10 por 100,000
abline(v = 10, col = "red", lty = 2)

# Calcular cuántos estados tienen tasa > 10
estados_mayor_10 <- sum(murders$rate > 10)
print(paste("Estados con tasa > 10 por 100,000:", estados_mayor_10))

# Verificar el total de estados
total_estados <- nrow(murders)
print(paste("Total de estados:", total_estados))

# Calcular la proporción en la eCDF en el punto 10
ecdf_murders <- ecdf(murders$rate)
proporcion_hasta_10 <- ecdf_murders(10)
proporcion_mayor_10 <- 1 - proporcion_hasta_10

print(paste("Proporción de estados ≤ 10:", round(proporcion_hasta_10, 3)))
print(paste("Proporción de estados > 10:", round(proporcion_mayor_10, 3)))
print(paste("Número de estados > 10:", round(proporcion_mayor_10 * 51)))

# Ver algunos estados con tasas altas
murders |>
  filter(rate > 10) |>
  arrange(desc(rate)) |>
  select(state, rate)

#Opción 2: 5
#Aproximadamente 5 estados 
#tienen tasas de asesinato mayores a 10 por cada 100,000 personas. 
#En el gráfico, la curva eCDF en x = 10 debe estar aproximadamente 
#en y = 0.90, lo que indica que el 90% de los estados tienen tasas 
#menores o iguales a 10, dejando aproximadamente el 10% (5 estados de 51) 
#con tasas superiores a 10.

# Calcular la tasa de asesinatos por 100,000 habitantes
murders <- murders |>
  mutate(rate = total / population * 100000)

# Crear la eCDF
plot(ecdf(murders$rate), 
     main = "eCDF de Tasas de Asesinato por Estado",
     xlab = "Tasa de asesinatos por 100,000 habitantes",
     ylab = "Proporción acumulativa")

abline(v = c(2, 5, 7), col = c("blue", "green", "red"), lty = 2)
abline(h = 0.5, col = "red", lty = 2)

ecdf_murders <- ecdf(murders$rate)

# Opción 1: ¿La mitad están por encima/debajo de 7?
prop_hasta_7 <- ecdf_murders(7)
mediana <- median(murders$rate)
print(paste("Proporción ≤ 7:", round(prop_hasta_7, 2)))
print(paste("Mediana:", round(mediana, 2)))

# Opción 2: ¿La mayoría están por debajo de 2?
prop_hasta_2 <- ecdf_murders(2)
print(paste("Proporción ≤ 2:", round(prop_hasta_2, 2)))

# Opción 3: ¿Todos están por encima de 2?
estados_menor_igual_2 <- sum(murders$rate <= 2)
print(paste("Estados ≤ 2:", estados_menor_igual_2))

# Opción 4: ¿Con excepción de 4 estados, las tasas están por debajo de 5?
estados_mayor_5 <- sum(murders$rate > 5)
prop_hasta_5 <- ecdf_murders(5)
print(paste("Estados > 5:", estados_mayor_5))
print(paste("Proporción ≤ 5:", round(prop_hasta_5, 2)))

# Ver la distribución
summary(murders$rate)
#Opción 4: Con la excepción de 4 estados, 
#las tasas de asesinato están por debajo de 5 por cada 100,000.


data(heights)

# Filtrar solo las alturas masculinas
male_heights <- heights$height[heights$sex == "Male"]

# Verificar el rango de los datos
print(paste("Altura mínima:", min(male_heights)))
print(paste("Altura máxima:", max(male_heights)))
print(paste("Rango:", range(male_heights)))

# Crear el histograma con breaks apropiados
hist(male_heights, 
     breaks = 20,
     main = "Histograma de Alturas Masculinas",
     xlab = "Altura (pulgadas)",
     ylab = "Frecuencia",
     col = "lightblue")

# Agregar líneas verticales en 62.5 y 65.5
abline(v = c(62.5, 65.5), col = "red", lty = 2, lwd = 2)

# Contar cuántos hombres están entre 62.5 y 65.5 pulgadas
hombres_en_rango <- sum(male_heights > 62.5 & male_heights < 65.5)
print(paste("Hombres entre 62.5 y 65.5 pulgadas:", hombres_en_rango))

# También verificar con <= y >= para incluir los límites
hombres_en_rango_inclusivo <- sum(male_heights >= 62.5 & male_heights <= 65.5)
print(paste("Hombres entre 62.5 y 65.5 pulgadas (inclusivo):", hombres_en_rango_inclusivo))

# Crear histograma con bins que cubran todo el rango
rango_min <- floor(min(male_heights))
rango_max <- ceiling(max(male_heights))

hist(male_heights, 
     breaks = seq(rango_min, rango_max, by = 0.5),
     main = "Histograma Detallado de Alturas Masculinas",
     xlab = "Altura (pulgadas)",
     ylab = "Frecuencia")
abline(v = c(62.5, 65.5), col = "red", lty = 2, lwd = 2)

print(paste("Total de hombres:", length(male_heights)))

#Opción 2: 29


# Crear el histograma
hist(male_heights, 
     breaks = 20,
     main = "Histograma de Alturas Masculinas",
     xlab = "Altura (pulgadas)",
     ylab = "Frecuencia",
     col = "lightblue")

# Agregar línea vertical en 60 pulgadas
abline(v = 60, col = "red", lty = 2, lwd = 2)

# Calcular cuántos hombres son más bajos que 60 pulgadas
hombres_menor_60 <- sum(male_heights < 60)
total_hombres <- length(male_heights)

# Calcular el porcentaje
porcentaje_menor_60 <- (hombres_menor_60 / total_hombres) * 100

print(paste("Hombres < 60 pulgadas:", hombres_menor_60))
print(paste("Total de hombres:", total_hombres))
print(paste("Porcentaje < 60 pulgadas:", round(porcentaje_menor_60, 1), "%"))

# Ver algunos estadísticos para contexto
print(paste("Altura mínima:", min(male_heights)))
print(paste("Altura máxima:", max(male_heights)))
summary(male_heights)

# También verificar cuántos están en el rango muy bajo
hombres_menor_igual_60 <- sum(male_heights <= 60)
print(paste("Hombres ≤ 60 pulgadas:", hombres_menor_igual_60))
#Opción 1: 1%

data(murders)

# Examinar las poblaciones de los estados
poblaciones <- murders$population

# Crear el gráfico de densidad
plot(density(poblaciones), 
     main = "Gráfico de Densidad de Poblaciones Estatales",
     xlab = "Población",
     ylab = "Densidad")

# Agregar línea vertical en 10 millones
abline(v = 10000000, col = "red", lty = 2, lwd = 2)

# Calcular la proporción de estados con población > 10 millones
estados_mayor_10M <- sum(poblaciones > 10000000)
total_estados <- length(poblaciones)
proporcion_mayor_10M <- estados_mayor_10M / total_estados

print(paste("Estados con población > 10 millones:", estados_mayor_10M))
print(paste("Total de estados:", total_estados))
print(paste("Proporción > 10 millones:", round(proporcion_mayor_10M, 3)))

# Ver cuáles son esos estados
estados_grandes <- murders[poblaciones > 10000000, c("state", "population")]
print("Estados con población > 10 millones:")
print(estados_grandes)

# También crear gráfico en escala de millones para mejor visualización
plot(density(poblaciones / 1000000), 
     main = "Gráfico de Densidad de Poblaciones Estatales",
     xlab = "Población (millones)",
     ylab = "Densidad")

#Opción 2: 0.15


poblaciones <- murders$population

# Gráfico 1: Densidad estándar
plot(density(poblaciones), 
     main = "Gráfico 1: Densidad Estándar",
     xlab = "Población",
     ylab = "Densidad")

# Gráfico 2: Densidad con poco suavizado (bandwidth menor)
plot(density(poblaciones, bw = "nrd0"), 
     main = "Gráfico 2: Poco Suavizado",
     xlab = "Población", 
     ylab = "Densidad")

# Gráfico 3: Densidad con mucho suavizado (bandwidth mayor)
plot(density(poblaciones, adjust = 3), 
     main = "Gráfico 3: Mucho Suavizado",
     xlab = "Población",
     ylab = "Densidad")

# También mostrar en escala logarítmica
plot(density(log10(poblaciones)), 
     main = "Gráfico con Escala Logarítmica",
     xlab = "Log10(Población)",
     ylab = "Densidad")

# Comparar parámetros de suavizado
print("Diferentes niveles de suavizado:")
print(paste("Bandwidth por defecto:", round(density(poblaciones)$bw)))
print(paste("Bandwidth reducido:", round(density(poblaciones, adjust = 0.5)$bw)))
print(paste("Bandwidth aumentado:", round(density(poblaciones, adjust = 3)$bw)))

#Opción 4: Son el mismo conjunto de datos, pero el primero no tiene el eje x en 
#escala logarítmica, el segundo suaviza poco y el tercero suaviza demasiado