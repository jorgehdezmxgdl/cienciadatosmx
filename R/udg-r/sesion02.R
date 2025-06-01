lista <- c(1:5)
lista
sum(lista)

colores = c('rojo','verde','azul','amarillo','negro')
colores[3]

vec_num = c(1:50)
vec_num
vec_num[vec_num > 10]

sort(vec_num, decreasing = T)[1]

items <- seq(1,10,2)

selectitems

todo <- as.factor(lista)

levels(todo)
#limpiar ctrl + L

library(dplyr)

install.packages("coronavirus")
install.packages("dplyr")

data("coronavirus")
library("coronavirus")
coronavirus
summary(coronavirus)
View(coronavirus)

# Hay NA
sum(is.na(coronavirus))

#paises que tienen menos caso
coronavirus |> select(country, type, cases) |> filter(type == 'confirmed') |> arrange(desc(cases))

library(dslabs)
data("reported_heights")
reported_heights

reported_heights <- reported_heights |> mutate(height2 = as.numeric(height))
reported_heights
reported_heights |> filter(height2 < 85)
reported_heights |> filter(sex == "Male")

reported_heights <- mutate(height_cm = height2 * 2.54) # |> summary()

alturas_validas <- reported_heights |> filter(!is.na(height_cm))

plot(alturas_validas$height2, alturas_validas$height_cm)

hist(alturas_validas$height_cm)

boxplot(alturas_validas$height_cm ~ reported_heights$sex)

boxplot(alturas_validas$height_cm ~ alturas_validas$sex)


#--------------------------

data("coronavirus")
library("coronavirus")
coronavirus
View(coronavirus)

revisa <- coronavirus |> filter(date == min(date)) |> filter(country == 'Mexico')
revisa

mexico <- coronavirus |> select(country, type, cases) |> filter(country == 'Mexico')

mexico <- coronavirus |> filter(country == 'Mexico')
mexico_anio <- mexico |> mutate(date = as.Date(date), year = format(date,"%Y")) |> filter(type == 'death') |> group_by(year) |> summarise(defunciones = sum(cases), poblacion = mean(population)) |> mutate(tasa = defunciones / poblacion * 100000)
mexico_anio

min(mexico$date)
max(mexico$date)

mexico_min <- mexico |> mutate(date = as.Date(date), year = format(date,"%Y")) |>  filter(type == 'death') |> filter(date == min(mexico$date)) |> group_by(year) |> nrow()
mexico_min

plot(mexico$date[mexico$type=="confirmed"], mexico$cases[mexico$type=="confirmed"])

plot(mexico[mexico$type=="confirmed", "date"], mexico[mexico$type=="confirmed","cases"])

lines()

mx_confirmed <-  mexico |> filter(type == 'confirmed')

my_box <- mx_confirmed |> mutate(date = as.Date(date), year = format(date, "%Y")) 

boxplot(my_box$cases ~ my_box$year)

summary(my_box$year)

mx_year <- summary(as.factor(my_box$year))
mx_year
pie(mx_year)
