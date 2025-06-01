lista <- c(1:1000)
sum(lista)

lista <- seq(1:1000)
sum(lista)

log10(sqrt(100))

log_base4 <- log(1021) / log(4)
log_base4

library(dslabs)
data("movielens")
nrow(movielens)
ncol(movielens)

class(movielens$genres)
levels(movielens$genres)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)  # en minutos
time_hours <- time / 60

time_hours[4]

speed <- distance / time_hours
speed[1]

name[which.max(speed)]

# Coeficientes
a <- 2
b <- -1
c <- -4

# Fórmula cuadrática
discriminante <- b^2 - 4*a*c

# Soluciones
x1 <- (-b + sqrt(discriminante)) / (2*a)
x2 <- (-b - sqrt(discriminante)) / (2*a)

# Mostrar resultados con 3 cifras significativas
cat("Solución positiva:", signif(x1, 3), "\n")
cat("Solución negativa:", signif(x2, 3), "\n")

