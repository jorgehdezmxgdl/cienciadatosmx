edad <- 45

ifelse(edad < 18, "Menor de edad","Mayor de edad")
ifelse(edad < 18, "Menor de edad",ifelse(edad < 60, "Adulto","Adulto mayor"))

if (edad < 18) {
  cat("El sujeto es menor de edad")
} else if (edad < 60) {
  Nacimiento <- as.numeric(format(Sys.Date(),"%Y")) - edad
  cat("El año de nacimiento fue ", Nacimiento, ".")
}else {
  Nacimiento <- as.numeric(format(Sys.Date(),"%Y")) - edad
  Mayor <- 60 + Nacimiento
  cat("El año de nacimiento fue ", Nacimiento, "\n Desde ", Mayor, " podia haber solicitado el INAPAM")
}

mes <- format(Sys.Date(),"%b")
mes
fecha <- as.Date("12 Dic 1999", format="%d %b %Y")
mes   <- format(mes, "%b")

dplyr::case_when(mes %in% c("Dic", "Jan", "Feb")) ~ "Invierno",
                 mes %in% c("Mar", "Abr", "May")) ~ "Primavera",
                 mes %in% c("Jun", "Jul", "Aug")) ~ "Verano",
                 mes %in% c("Sep", "Oct", "Nov")) ~ "Otoño")

n <- 1:10
for (i in n) {
  print(n)
}

for (i in n) {
  print(i)
}


library(dslabs)
data("nyc_regents_scores")
nyc_regents_scores
nyc_scores <- nyc_regents_scores |> dplyr::select(-score)

nrow(nyc_scores)
1 filas
2 columnas
apply(nyc_scores, MARGIN = 1, sum, na.rm = T)

apply(nyc_scores, MARGIN = 2, median, na.rm = T)

boxplot(nyc_scores)

library(purrr)

map(nyc_scores, median, na.rm = T)     #lista
map_dbl(nyc_scores, median, na.rm = T) #vector

vector_prueba <- 1: 100000
square <- function(x) sqrt(x)
square(4)


f_for <- function(vector) {
  resultado <- numeric(length(vector))
  for (i in seq_along(vector)) {
     resultado[i] <- square(vector[i])
  }
  return (resultado)
}

f_apply <- function(vector) {
  sapply(vector, square)
}

f_purrr <- function(vector) {
  map_dbl(vector, square)
}


f_for(vector_prueba)
f_apply(vector_prueba)
f_purrr(vector_prueba)


#evaluar rendimiento
install.packages("microbenchmark")
microbenchmark::microbenchmark(f_for(vector_prueba), times = 10)
microbenchmark::microbenchmark(f_apply(vector_prueba), times = 10)
microbenchmark::microbenchmark(f_purrr(vector_prueba), times = 10)

#ejecutarlas 
microbenchmark::microbenchmark(
   f_for   = f_for(vector_prueba), 
   f_apply = f_apply(vector_prueba),
   f_purrr = f_purrr(vector_prueba),
   times = 10)

#comparar

identical(2,2.000)
all.equal(square(vector_prueba), f_for(vector_prueba))

saludar <- function(hora, idioma = 'español') {
  if (is.numeric(hora)) {
     hh <- hora %/% 100
     chequeo <- ifelse(hh > 24,T, F)
  } else {
    print("Formato no valido")
  }  
  if (!chequeo) {
    momento <- dplyr::case_when(hh < 14 ~ "di",
                                hh < 19 ~ "ta",
                                .default = "no")
    salida <- if (idioma == "aleman") {
      dplyr::case_when(momento == "di" ~ "A",
                       momento == "ta" ~ "B",
                       .default = "C")
    } else if (idioma == "español") {
      dplyr::case_when(momento == "di" ~ "D",
                       momento == "ta" ~ "E",
                       .default = "F")
    }
    return (salida)
  }
}

saludar(2400, idioma="español")

library(coronavirus)
library(dplyr)

data("coronavirus")
View(coronavirus)



tasa_mortalidad_anual <- function(pais, anio) {
  coronavirus |>
       mutate(year = as.numeric(format(as.Date(date),"%Y"))) |>
       filter(country == pais & year == anio & type == "death") |>
       group_by(country, province) |>
       summarise(pobl = mean(population), casos = sum(cases)) |>
       mutate(tasaMortabilidad = casos / pobl * 100000) |>
       select(country, province,tasaMortabilidad)
}

tasa_mortalidad_anual("Canada", 2021)

tasa_incidencia_anual <- function(pais, anio) {
  coronavirus |>
    mutate(year = as.numeric(format(as.Date(date),"%Y"))) |>
    filter(country == pais & year == anio & type == "confirmed") |>
    group_by(country) |>
    summarise(pobl = mean(population, na.rm=T), casos = sum(cases, na.rm = TRUE)) |>
    mutate(tasa = casos / pobl * 100000) |>
    pull(tasa)
}

tasa_incidencia_anual("China",2021)



