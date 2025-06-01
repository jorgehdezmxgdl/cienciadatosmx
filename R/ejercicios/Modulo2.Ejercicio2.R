#Jorge Fausto Hernandez Andrade
library(dslabs)
library(dplyr)
library(ggplot2)

data(gapminder)
gapminder


# Encontrar el país con la mayor población
gapminder |>
  arrange(desc(population)) |>
  head(1)

#Opción 3: Asia

# Calcular la mediana de población por continente
gapminder |>
  group_by(continent) |>
  summarise(median_population = median(population, na.rm = T)) |>
  arrange(desc(median_population))

ggplot(gapminder, aes(x = continent, y = population)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Distribución de Población por Continente",
       x = "Continente",
       y = "Población (escala log)")

#Opción 3: Asia

# Filtrar solo los datos de África y calcular la mediana
africa_data <- gapminder |>
  filter(continent == "Africa")

# Calcular la mediana de población para África
median_africa <- median(africa_data$population, na.rm = T)
print(median_africa)

# Redondear al millón más cercano
median_africa_millions <- round(median_africa / 1000000)
print(paste("Mediana de población de África:", median_africa_millions, "millones"))

ggplot(gapminder, aes(x = continent, y = population / 1000000)) +
  geom_boxplot() +
  labs(title = "Distribución de Población por Continente",
       x = "Continente",
       y = "Población (millones)") +
  theme_minimal()

summary(africa_data$population / 1000000)

# Opción 4: 5 Millones

# Crear boxplot para examinar la distribución
ggplot(gapminder, aes(x = continent, y = population / 1000000)) +
  geom_boxplot() +
  geom_hline(yintercept = 14, color = "red", linetype = "dashed") +
  labs(title = "Distribución de Población por Continente",
       subtitle = "Línea roja: 14 millones",
       x = "Continente",
       y = "Población (millones)") +
  theme_minimal()

# Calcular la proporción específica para Europa
europa_data <- gapminder |>
  filter(continent == "Europe")

# Contar países con población < 14 millones
paises_bajo_14M <- europa_data |>
  filter(population < 14000000) |>  nrow()

total_paises_europa <- nrow(europa_data)

proporcion <- paises_bajo_14M / total_paises_europa

print(paste("Países en Europa con población < 14M:", paises_bajo_14M))
print(paste("Total países en Europa:", total_paises_europa))
print(paste("Proporción:", round(proporcion, 2)))

# Ver los cuartiles para Europa
europa_summary <- europa_data %>%
  summarise(
    Q1 = quantile(population, 0.25, na.rm = T) / 1000000,
    Mediana = quantile(population, 0.50, na.rm = T) / 1000000,
    Q3 = quantile(population, 0.75, na.rm = T) / 1000000
  )

print(europa_summary)

#  Opción 1: 0.75

# Crear boxplot con escala logarítmica
ggplot(gapminder, aes(x = continent, y = population)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Distribución de Log(Población) por Continente",
       x = "Continente",
       y = "Población (escala logarítmica)") +
  theme_minimal()

iqr_por_continente <- gapminder |>
  group_by(continent) |>
  summarise(
    Q1_log = quantile(log10(population), 0.25, na.rm = TRUE),
    Q3_log = quantile(log10(population), 0.75, na.rm = TRUE),
    IQR_log = Q3_log - Q1_log,
    .groups = 'drop'
  ) |>
  arrange(desc(IQR_log))

print(iqr_por_continente)
#Opción 3: Asia