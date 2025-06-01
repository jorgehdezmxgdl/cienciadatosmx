#Jorge Fausto Hernandez Andrade

library(dslabs)
data(heights)
# Extraer los nombres de las variables
names(heights)

#Opción 2: Categórica
#La variable sex es una variable categórica (también llamada cualitativa) porque:
#Representa categorías o grupos distintos (Male/Female)

x <- heights$height

# Determinar cuántas alturas únicas se informaron
length(unique(x))

#Crear tabla de frecuencias y guardarla en el objeto 'tab'
tab <- table(x)
tab

# Contar cuántos valores aparecen solo una vez
sum(tab == 1)

#
#Opción 1: Es más efectivo considerar las alturas como numéricas dada la 
#cantidad de valores únicos que observamos y el hecho de que si seguimos 
#recolectando datos se observarán aún más.

