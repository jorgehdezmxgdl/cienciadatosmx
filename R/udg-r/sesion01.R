#Sesión 01

x <- c(735, 320, 392, 15:20)
x
sum(x)
mean(x)
median(x)

xNA <- c(735, 320, NA, 15:20, NA)
xNA
sum(xNA, na.rm = T) #TRUE
mean(xNA, na.rm = T) #TRUE

library(datasets)
?datasets

data(package="datasets")
data("rivers")

rivers
#ordenar rios
sort(rivers)
sort(rivers, decreasing = T)

order(rivers)
o1 <- order(rivers)
rivers[o1]

rank(rivers)

lista <- c(1,2,3,4,5)
ordenado <- order(lista)
indice_ordenado <- order(lista)
ranking <- rank(lista)

rivers[1:5]
rivers[141]

rivers/rivers
rivers + 5

rivers + xNA
xNA + rivers

install.packages("dslabs")
library(dslabs)

data("reported_heights")
summary(reported_heights)
str(reported_heights)
#columnas

reported_heights$height2 <- as.numeric(reported_heights$height)
reported_heights

#filas
reported_heights[1:5,]

#columnas
reported_heights[,1:2]

#fila y columna
reported_heights[143,2]

#filtrar por corchetes

reported_heights[reported_heights$sex == 'Female',] #columnas despues de la coma
reported_heights[reported_heights$height2 < 85,]

reported_heights[reported_heights$sex == 'Male' | reported_heights$height2 == 165.10, ] |> View()

hist(rivers)
hist(reported_heights$height2)

boxplot(reported_heights$height2 ~ reported_heights$sex)


