library(dslabs)
data(heights)
sexo_codificado <- ifelse(heights$sex == "Female", 1, 2)
suma <- sum(sexo_codificado)
suma #1862

mayores_72 <- ifelse(heights$height > 72, 1, 0)

# Calcular la media
mean(mayores_72) #12

inches_to_ft <- function(x) {
  x / 12
}
inches_to_ft(144) #12

sum(heights$height < 60) #20

any(T,T,F)
all(F,F,F)

all(T,T,F)
any(F,F,F)

any(T,F,F)



all(T,T,T)
all(T,F,F)

any(T,T,T)

m <- 10
for (n in 1:m) {
  print(n)
}

