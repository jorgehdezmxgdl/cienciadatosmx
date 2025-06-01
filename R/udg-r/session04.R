library(dplyr)

install.packages("readr")
install.packages("readxl")

getwd()
setwd("C:/Users/JorgeFausto/Desktop/udg-r/")

censo_puebla <- readr::read_csv("./21_CensoPoblacionVivienda2020.csv")
censo_puebla

str(censo_puebla)

type.convert(censo_puebla, as.is=T) |> str() 

censo_puebla <- readr::read_csv("./21_CensoPoblacionVivienda2020.csv", col_select=c(NOM_ENT, NOM_MUN, NOM_LOC, 
                                POBTOT, POBFEM, POBMAS, GRAPROES, GRAPROES_F, GRAPROES_M, PEA, PEA_F, PEA_M,POCUPADA,
                                POCUPADA_F, POCUPADA_M), col_types = c("c","c","c","n","n","n","n","n","n","n","n",
                                                                       "n","n","n","n"))
summary(censo_puebla)

censo_puebla$GRAPROES_F

censo_puebla$POBTOT <- as.numeric(censo_puebla$POBTOT)
censo_puebla$POBFEM <- as.numeric(censo_puebla$POBFEM)
censo_puebla$POBMAS <- as.numeric(censo_puebla$POBMAS)
censo_puebla$GRAPROES <- as.numeric(censo_puebla$GRAPROES)
censo_puebla$GRAPROES_F <- as.numeric(censo_puebla$GRAPROES_F)
censo_puebla$GRAPROES_M <- as.numeric(censo_puebla$GRAPROES_M)
censo_puebla$POCUPADA <- as.numeric(censo_puebla$POCUPADA)
censo_puebla$POCUPADA_F <- as.numeric(censo_puebla$POCUPADA_F)
censo_puebla$POCUPADA_M <- as.numeric(censo_puebla$POCUPADA_M)
censo_puebla$PEA <- as.numeric(censo_puebla$PEA)
censo_puebla$PEA_F <- as.numeric(censo_puebla$PEA_F)
censo_puebla$PEA_M <- as.numeric(censo_puebla$PEA_M)


municipios <- censo_puebla |> group_by(NOM_MUN) |> summarise(across(POBTOT:POCUPADA_M, sum, na.rm = T)) |> filter(NOM_MUN != 'Total de la entidad Puebla')

barplot(municipios$POBTOT)
boxplot(municipios$POBFEM ~ municipios$NOM_MUN)
plot(municipios$POBMAS, municipios$POBFEM)

max(municipios$POBTOT)

barplot(municipios$POBTOT[municipios$NOM_MUN != 'Puebla'])

boxplot(municipios$POBFEM[municipios$NOM_MUN != 'Puebla'] ~ municipios$NOM_MUN[municipios$NOM_MUN != 'Puebla'])

plot(municipios$POBMAS[municipios$NOM_MUN != 'Puebla'],municipios$POBFEM[municipios$NOM_MUN != 'Puebla'])

plot(municipios$PEA_M, municipios$PEA_F)


readxl::read_xlsx("./21_EncuestaOcupacionEmpleo.xlsx")

readxl::read_xlsx("./21_EncuestaOcupacionEmpleo.xlsx", skip=4, sheet=1)

readxl::read_xlsx("./21_EncuestaOcupacionEmpleo.xlsx", skip=4, sheet=1, range=c("F5:CK40"))

ocupacion <- readxl::read_xlsx("./21_EncuestaOcupacionEmpleo.xlsx", skip=4, sheet=1, range=c("F5:CK40"))

#Transformar el df de ancho a largo
# son las columnas que se van a transformar de ancho a largo
# columna que ccontendra los valores de las columnas anteriores
# columnas de tiempo
#nombre de la columna que contendra el tiempo
mujeres_ocupadas <- ocupacion |> reshape(direction="long", idvar = names(ocupacion)[1], varying = 2:84, v.names = "valor", times = names(ocupacion)[2:84], timevar =  "temporada", new.row.names = 1:2905) |> dplyr::filter(indicador== "Población ocupada - 15 años y más, mujeres")|> mutate(valor = as.numeric(valor), temporada = as.factor(temporada)) |> filter(!is.na(valor))

mujeres_ocupadas
View(mujeres_ocupadas)

plot(mujeres_ocupadas$temporada, mujeres_ocupadas$valor)

#interpretar datos
mujeres_ocupadas |> summarise(max = max(valor, na.rm = T), min = min(valor, na.rm = T), media = mean(valor, na.rm=T), desv=sd(valor, na.rm=T))

mujeres_ocupadas <- mujeres_ocupadas |> mutate(año = substr(temporada,1,4), temp = substr(temporada, 6, 7), año = as.factor(año), temp = as.factor(temp))
mujeres_ocupadas

boxplot(mujeres_ocupadas$valor ~ mujeres_ocupadas$año)
boxplot(mujeres_ocupadas$valor ~ mujeres_ocupadas$temp)
boxplot(mujeres_ocupadas$valor ~ mujeres_ocupadas$temporada)


mujeres_ocupadas |> group_by(temp) |> summarise(max = max(valor), min = min(valor), media = mean(valor), desv=sd(valor))
mujeres_ocupadas |> group_by(año) |> summarise(max = max(valor), min = min(valor), media = mean(valor), desv=sd(valor))
