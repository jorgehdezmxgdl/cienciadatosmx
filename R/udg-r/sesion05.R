library(ggplot2)

datos <- data.frame(alturas = c(160, 160, 160, 158, 180, 178, 190,153, 154, 174, 164, 148))
ggplot(datos, aes(x = alturas)) + geom_histogram(binwidth = 6, fill = "lightblue",color="black", alpha = 0.7) + labs(title= "Histograma altura", x="Altura (cm)", y="Frecuencia") + theme_minimal()

#suavizado
ggplot(datos, aes(x = alturas)) +
     geom_density(fill = "skyblue", alpha=0.5) +
     labs(title= "Histograma altura", x="Altura (cm)", y="Frecuencia") + theme_minimal()

#histograma + suavidad
ggplot(datos, aes(x = alturas)) +
  geom_histogram(aes(y=..density..), binwidth = 4, fill="lightblue", color="black", alpha=0.5) +
  geom_density(color="blue", size=1) +
  labs(title= "Histograma altura", x="Altura (cm)", y="Frecuencia") + theme_minimal()

#ejercicio 1
puntuaciones <- rnorm(100, mean=75, sd=10)
probabilidad <- 1 - pnorm(80, mean = mean(puntuaciones), sd = sd(puntuaciones))
probabilidad

vector1 <- c(18,45,32,20)
vector2 <- c(45,23,9, 78)
vector3 <- c(vector1, vector2)
vector3


library(dslabs)
data("mtcars")
datatable(mtcars)

puntuaciones <- mtcars$mpg
q1 <- quantile(puntuaciones, 0.25)
q2 <- quantile(puntuaciones, 0.5)
q3 <- quantile(puntuaciones, 0.75)
q1
q2
median(puntuaciones)
q3
cat("Q1: ", q1, "\n")
cat("Q2 (mediana): ", q2, "\n")
cat("Q3: ", q3, "\n")

boxplot(puntuaciones, main='Boxplot de mpg', ylab='Millas por galon', col='lightblue', border = "darkblue")

tiempo1 <- rnorm(100, mean=10, sd=2)
tiempo2 <- rnorm(100, mean=15, sd=3)
media1  <- mean(tiempo1)
sd1 <- sd(tiempo1)

media2  <- mean(tiempo2)
sd2 <- sd(tiempo2)
cat("Dato", media1, "sd", sd1, "\n")
cat("Dato", media2, "sd", sd2, "\n")

set.seed(123)
tiempo3 <- rnorm(1000, mean=50, sd=10)
media3  <- mean(tiempo3)
sd3 <- sd(tiempo3)
cat("Mediana de la distribucion normal: ", media3, ",Desviacion estandar de la distr normal: ", sd3, "\n")


set.seed(456)
prueba <- rbinom(1000, size=10, prob=0.3)
mean(prueba)
var(prueba)


analitica de datos python
40 hrs => 20 h

datos <- rnorm(100)

histograma <- hist(datos, plot=F)
hist(datos)
histograma$breaks[-1]

valor_objetivo <- 0
indice_rojo <- which(valor_objetivo >= histograma$breaks[-length(histograma$breaks)] & valor_objetivo < histograma$breaks[-1])
colores <- rep("gray",length(histograma$counts))
colores[indice_rojo]<- "red"
colores[indice_rojo + 1] <- "blue"

plot(histograma, col= colores, main = "Histograma con barra resaltada", xlab="Valor", ylab = "Frecuencia")


library(dslabs)

data("mtcars")
mtcars

library(ggplot2)
ggplot(mtcars, aes(x=cyl, y=mpg, fill=factor(cyl) )) +
   geom_boxplot() +
   labs(title = "Boxplot de mpg", x="Numero de cilindros", y="Millas por galón") +
   scale_fill_manual(values = c("4" = "yellow", "6" = "black", "8" = "salmon"))+
   theme_gray()


data(mtcars)
mtcars$cyl <- factor(mtcars$cyl, levels = sort(unique(mtcars$cyl)))
conteo_cilindros <- table(mtcars$cyl)
ggplot(mtcars, aes(x = cyl, fill = cyl)) +
  geom_bar(color = "black") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  labs(
    title = "Distribución de Modelos por Número de Cilindros",
    x = "Número de Cilindros",
    y = "Cantidad de Modelos"
  ) +
  scale_fill_manual(values = c("skyblue", "orange", "seagreen")) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")


df_nile <- data.frame(
  year = time(Nile),
  nivel = as.numeric(Nile)
)

ggplot(df_nile, aes(x = year, y = nivel)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Serie Temporal de los Niveles del Río Nilo",
       x = "Año",
       y = "Nivel del Río Nilo") +
  theme_minimal(base_size = 15) +
  geom_point(color = "darkblue", size = 1.5, alpha = 0.6)


ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point(color = "blue", size = 3, alpha = 0.5) +
    geom_smooth(method = "auto", se = T)


install.packages("tidyr")
library(dplyr)
data("economics")
datos <- economics |>
  select(date, ingreso = psavert, gasto = unemploy) |>
  tidyr::pivot_longer(cols = c("ingreso", "gasto"), names_to = "Tipo", values_to = "Valor")

ggplot(datos, aes(x = date, y = Valor, fill = Tipo)) +
  geom_area(alpha = 0.6, position = 'identity') +
  scale_fill_manual(values = c("skyblue", "red"), labels = c("Gasto (Desempleo)", "Ingreso (Ahorro personal %)")) +
  labs(title = "Evolución Trimestral del Ingreso y Gasto",
       x = "Fecha",
       y = "Valor",
       fill = "Tipo") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


economics |>
     ggplot(aes(x = date)) +
     geom_area(aes(y = psavert, fill = "Ingreso"), alpha = 0.7) +
     geom_area(aes(y = uempmed, fill = "Gasto"), alpha = 0.7) +
     scale_fill_manual(values = c("Ingreso"= "skyblue","Gasto"="red")) +
     labs(title = "Evolución Trimestral del Ingreso y Gasto",
     x = "Fecha",
     y = "Valor",
     fill = "Tipo") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45), hjust = 1)
  
valores <- c(59000, 65000-59000)
etiquetas <- c("Valor original","Aumento")
     
porcentajes <- round(valores / sum(valores)) + 100
etiqueta <- paste(etiquetas, "-", porcentajes, "%")

pie(valores,
    labels = etiqueta,
    col = c("skyblue","orange"),
    main="Distribucion de nuevo valor")


data <- data.frame(
  categoria = c("Valor original","Aumento"),
  valor = c(59000, 65000-59000)
)

data

ggplot(data, aes(x = "", y=valor, fill=categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "") +
  theme_void()
