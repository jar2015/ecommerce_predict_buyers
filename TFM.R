---
title: "Trabajo Final Master"
author: "Jose Rodriguez"
date: "April 20201"
---

#Carga de librerias
library(dplyr)
library(stringr)
library(tidyverse)
library(funModeling)
library(VIM)
library(corrplot)
library(car)
library(plyr)

##################################
### Lectura archivo de usuarios###
##################################
usuarios = read.delim('Linea 1 - Usuarios.txt', sep = ';', na.strings = c("","NA"))

### ANALISIS EXPLORATORIO DE LOS DATOS
##Revision de los datos
dim(usuarios) #368220 rows - 19 columns
str(usuarios) #"6 variables int y 13 Factor, estas deben ser cambiadas" 

#Cambiar las clases de las variables
#Cambiar a tipo fecha
usuarios$FECHA_REGISTRO = as.Date(usuarios$FECHA_REGISTRO, format = '%d/%m/%Y')
usuarios$FECHA_ALTA = as.Date(usuarios$FECHA_ALTA, format = '%d/%m/%Y')
usuarios$FECHA_CLIENTE = as.Date(usuarios$FECHA_CLIENTE, format = '%d/%m/%Y')

#Cambiar a tipo factores
factors = c('ID_USUARIO', 'CANAL_REGISTRO', 'IND_CLIENTE', 'IND_ALTA', 'BONDAD_EMAIL','IP_Region')
usuarios[,factors] <- lapply(usuarios[,factors], factor)
rm(factors)

#Cambiar a tipo numerica
usuarios$IPCASOS = as.numeric(usuarios$IPCASOS)

#Analisis de datos por tipo de datos
fechas = usuarios %>% select(FECHA_REGISTRO, FECHA_ALTA, FECHA_CLIENTE)%>% colnames()
summary(usuarios[fechas])
summary(usuarios$IND_ALTA)
summary(usuarios$IND_CLIENTE)
alta_1 = usuarios %>% filter(is.na(FECHA_ALTA) & IND_ALTA==1)
alta_2 = usuarios %>% filter(IND_ALTA==0 & !is.na(FECHA_ALTA))
cliente_1 = usuarios %>% filter(is.na(FECHA_CLIENTE) & IND_CLIENTE==1)
cliente_2 = usuarios %>% filter(IND_CLIENTE==0 & !is.na(FECHA_CLIENTE))

#Imputar valores en FECHA_CLIENTE y FECHA_ALTA
usuarios$FECHA_CLIENTE = as.Date(ifelse(usuarios$IND_CLIENTE==1 & usuarios$IND_ALTA==1 & is.na(usuarios$FECHA_CLIENTE),usuarios$FECHA_ALTA, usuarios$FECHA_CLIENTE), origin = "1970-01-01")
usuarios$FECHA_ALTA = as.Date(ifelse(usuarios$IND_CLIENTE==1 & usuarios$IND_ALTA==1 & is.na(usuarios$FECHA_ALTA),usuarios$FECHA_CLIENTE, usuarios$FECHA_ALTA), origin = "1970-01-01")
usuarios$FECHA_ALTA[usuarios$IND_ALTA==0 & !is.na(usuarios$FECHA_ALTA)] = NA

#Se eliminan tablas temporales de analisis
rm(alta_1, alta_2, cliente_1, cliente_2, fechas)

##Resumen variables cuanlitativas
factor_names <- usuarios %>% select(which(sapply(.,is.factor)))%>% colnames()
summary(usuarios[factor_names])
rm(factor_names)

##Eliminar duplicados
usuarios = usuarios[!duplicated(usuarios[,1:10]), ]

#Creacion de tablas de frecuencias y graficos de barras
freq(usuarios[factor_names])

#Estudio de valores NA
colSums(is.na(usuarios))

#Lista de regiones únicas
usuarios %>% distinct(IP_Region) %>% arrange(IP_Region)

##Recodificar variables
usuarios$IP_Country = recode(usuarios$IP_Country, 'M\xe9xico'='Mexico')
usuarios$IP_Country = recode(usuarios$IP_Country, 'Poland'='Polonia')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Atl\xe1ntico'='Atlantico')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Bogot\xe1 D.C.'='Bogota')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Atl<e1>ntico'='Atlantico')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Bogot<e1> D.C.'='Bogota')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Bolívar'='Bolivar')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Buenos Aires F.D.'='Buenos Aires')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Federal District'='Ciudad de Mexico')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Distrito Federal'='Ciudad de Mexico')
usuarios$IP_Region = recode(usuarios$IP_Region, 'México'='Ciudad de Mexico')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Mexico City'='Ciudad de Mexico')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Hesse'='Hessen')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Lima region'='Lima')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Navarre'='Navarra')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Nueva York'='New York')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Quind\xedo'='Quindio')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Central and Western District'='Central District')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Coquimbo Region'='Coquimbo')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Los Ríos Region'='Los Rios')
usuarios$IP_Region = recode(usuarios$IP_Region, 'Panama Oeste'='Panama')
usuarios$USU_ESTADO = recode(usuarios$USU_ESTADO, 'CANCELACI\xd3N'='CANCELACION')
usuarios$IP_Region = gsub("Provincia de ", "", usuarios$IP_Region)
usuarios$IP_Region = gsub("Provincia del ", "", usuarios$IP_Region)
usuarios$IP_Region = gsub("Departamento de ", "", usuarios$IP_Region)

##Despues de la recodificacion de la region, esta paso a ser de tipo caracter, por lo que debe cambiarse a factor
usuarios$IP_Region <- as.factor(usuarios$IP_Region)

##Resumen variables cuantitativas
num_names <- usuarios %>% select(which(sapply(.,is.numeric)))%>% colnames()
summary(usuarios[num_names])
boxplot(usuarios$IPCASOS)
describe(usuarios$IPCASOS)
rm(num_names)

#Se analizan las variables cuantitativas que presentaron valores atipicos
boxplot.stats(usuarios$IPCASOS)$out

##Revision de los datos
str(usuarios)
summary(usuarios)

#########################   BASE USUARIOS LISTA   #########################

##################################
### Lectura archivo de consumo ###
##################################
options(scipen = 999)
consumo = read.delim('Linea1-Consumos.txt', sep = ';', na.strings = c("","NA"))
dim(consumo)
#938580     11

str(consumo)
"2 variables int y 9 Factor" 

#Estudio de valores NA
#colSums(is.na(consumo))

#Cambio de clases
consumo$FECHACONSUMO = as.Date(consumo$FECHACONSUMO, format = '%d/%m/%Y')
consumo$IDCONSUMO = as.factor(as.numeric(as.character(sub("," , ".", consumo$IDCONSUMO))))
consumo$IDUSUARIO = as.factor(as.numeric(as.character(sub("," , ".", consumo$IDUSUARIO))))
consumo$IDPRODUCTO = as.factor(as.numeric(as.character(sub("," , ".", consumo$IDPRODUCTO))))
consumo$EMPCONSUL_ID = as.factor(consumo$EMPCONSUL_ID)
consumo$IDGRUPOPROD = as.factor(consumo$IDGRUPOPROD)

#Eliminar duplicados
consumo = consumo[!duplicated(consumo), ]

#Recodificacion de variables
consumo$DESCPRODUCTO = recode(consumo$DESCPRODUCTO, 'Ficha B\xe1sica' = 'Ficha Basica')
consumo$DESCGRUPOPROD = recode(consumo$DESCGRUPOPROD, 'Ficha B\xe1sica Promocional' = 'Ficha Basica Promocional')
consumo$DESCGRUPOPROD = recode(consumo$DESCGRUPOPROD, 'Modulo de Informaci\xf3n' = 'Modulo de Informacion')
consumo$EMPCONSUL_EST = recode(consumo$EMPCONSUL_EST, 'CANCELACI\xcbN' = 'CANCELACION')
consumo$EMPCONSUL_EST = recode(consumo$EMPCONSUL_EST, 'ANULACI\xcbN LIQUIDACI\xcbN' = 'ANULACION LIQUIDACION')

#########################   BASE COMSUMO LISTA   #########################

####################################
### CREACION DE NUEVAS VARIABLES  ##
####################################

##Codigo de area en archivo de usuarios
usuarios$USU_CIIU_AREA = ifelse(nchar(as.character(usuarios$USU_CIIU)) > 4,substr(usuarios$USU_CIIU,1,1),'')
#usuarios$USU_CIIU_DIV = ifelse(nchar(as.character(usuarios$USU_CIIU)) > 4,substr(usuarios$USU_CIIU,2,3),substr(usuarios$USU_CIIU,1,2))
#usuarios$USU_CIIU_CLASE = ifelse(nchar(as.character(usuarios$USU_CIIU)) > 4,substr(usuarios$USU_CIIU,2,5),substr(usuarios$USU_CIIU,1,4))

#A las variables nuevas, cambiarlas a tipo factor
factors_u = c('USU_CIIU_AREA')
usuarios[factors_u] <- lapply(usuarios[factors_u], factor)
rm(factors_u)

#Creacion de la variable dias conversion (dias desde que se registro hasta que pago)
usuarios$DIAS_CONVERSION = usuarios$FECHA_CLIENTE - usuarios$FECHA_REGISTRO
usuarios$DIAS_CONVERSION = as.numeric(usuarios$DIAS_CONVERSION)

##Ordenar base de usuarios por IP Country
usuarios_ordered = usuarios %>% arrange(IP_Country)

#Division de bases, base 1 los que tienen el pais y la 2da los que no lo tienen
usuarios_con_pais = usuarios_ordered[1:345941,]
usuarios_sin_pais = usuarios_ordered[345942:367705,]

#Conteo de cuantos paises hay asociados a cada telefono
telxpais = usuarios_ordered %>% group_by(USU_TELF) %>% filter(!is.na(IP_Country)) %>% summarise(paises = n_distinct(IP_Country))

#Filtrar los que solo tienen un telefono
tel_paises_1 = telxpais %>% filter(paises==1)
tel_paises_1 = usuarios_ordered %>% select(USU_TELF, IP_Country) %>% filter(USU_TELF %in% tel_paises_1$USU_TELF & !is.na(IP_Country))

#Imputar el pais segun el telefono
impu_pais = merge(x=usuarios_sin_pais, y= tel_paises_1, by='USU_TELF', all.x = TRUE)
impu_pais$IP_Country.x = impu_pais$IP_Country.y
impu_pais = impu_pais[,-24]
colnames(impu_pais)[13] = 'IP_Country'
impu_pais = impu_pais[!duplicated(impu_pais), ]
impu_pais = impu_pais %>% relocate(USU_TELF, .after= BONDAD_EMAIL)

#Unir base con imputaciones con la que ya tenia los paises
usuarios_nueva = rbind(impu_pais, usuarios_con_pais)
rm(impu_pais, telxpais, tel_paises_1, usuarios_con_pais, usuarios_sin_pais, usuarios_ordered, usuarios_ordered, usuarios)

##Codigo de area en archivo de consumo
consumo$EMPCIIU_AREA = ifelse(nchar(as.character(consumo$EMPCONSUL_CIIU)) > 4,substr(consumo$EMPCONSUL_CIIU,1,1),'')
#consumo$EMPCIIU_CLASE = ifelse(nchar(as.character(consumo$EMPCONSUL_CIIU)) > 4,substr(consumo$EMPCONSUL_CIIU,2,5),substr(consumo$EMPCONSUL_CIIU,1,4))
#consumo$EMPCIIU_DIV = ifelse(nchar(as.character(consumo$EMPCONSUL_CIIU)) > 4,substr(consumo$EMPCONSUL_CIIU,2,3),substr(consumo$EMPCONSUL_CIIU,1,2))

#Separacion de la base, en un lado los que si tienen consigo de area ciiu y en el otro los que no lo tienen
consumo = consumo %>% arrange(EMPCIIU_AREA)
consumo_con_ciiu = consumo[2465:938580,]
consumo_sin_ciiu = consumo[1:2464,]

#Imputacion del area ciiiu
ciiu_codes = consumo %>% select(EMPCIIU_AREA, EMPCIIU_DIV) %>% distinct()
consumo_sin_ciiu_new = merge(x=consumo_sin_ciiu, y=ciiu_codes, by='EMPCIIU_DIV', all.x = TRUE)
consumo_sin_ciiu_new = consumo_sin_ciiu_new %>% select(IDCONSUMO, IDUSUARIO, IDPRODUCTO, DESCPRODUCTO, IDGRUPOPROD,
                                                       DESCGRUPOPROD, FECHACONSUMO, EMPCONSUL_ID, EMPCONSUL_CIIU, EMPCONSUL_PROV,
                                                       EMPCONSUL_EST, EMPCIIU_AREA.y) %>% arrange(IDCONSUMO)
colnames(consumo_sin_ciiu_new)[12] = 'EMPCIIU_AREA'

consumo_listo = rbind(consumo_sin_ciiu_new, consumo_con_ciiu)

#Eliminacion de duplicados
consumo_listo = consumo_listo[!duplicated(consumo_listo[,1]), ]

#Cambio de nombre en la variable para hacer la union con la base de usuarios
consumo_listo = consumo_listo %>% rename(ID_USUARIO = IDUSUARIO)

#Debido a los cambios realizados, se les debe de asignar nuevamente el tipo factor a las variables
#consumo_listo$EMPCIIU_CLASE = as.factor(consumo_listo$EMPCIIU_CLASE)
#consumo_listo$EMPCIIU_DIV = as.factor(consumo_listo$EMPCIIU_DIV)
consumo_listo$EMPCIIU_AREA = as.factor(consumo_listo$EMPCIIU_AREA)

#str(consumo_listo)
#summary(consumo_listo)

#Eliminar tablas temporales
rm(ciiu_codes,consumo_con_ciiu, consumo_sin_ciiu, consumo_sin_ciiu_new, consumo)

#Creacion de tabla temporal para agregarle la fecha cliente a la base de consumo
fecha_cliente = usuarios %>% select(ID_USUARIO, FECHA_CLIENTE)

#Agregando la fecha de cliente a la tabla de consumo
consumo_listo = merge(x=consumo_listo, y=fecha_cliente, by='ID_USUARIO', all.x = TRUE)

#Eliminar tablas temporales
rm(fecha_cliente)

#Se crea una variable dicotomica que indica 0-consumo gratuito y 1-consumo pagado
consumo_listo$SE_PAGO = ifelse(is.na(consumo_listo$FECHA_CLIENTE) | consumo_listo$FECHACONSUMO < consumo_listo$FECHA_CLIENTE,0,1)

#Creacion de nuevas variables
#Aggregate consumo total & productos total & consumo pagado & consumo no pagado
resumen_1 = consumo_listo %>% group_by(ID_USUARIO) %>% summarise(CONSUMO_TOTAL = n_distinct(IDCONSUMO),PROD_TOT = n_distinct(IDPRODUCTO), CONSUMO_P = sum(SE_PAGO), CONSUMO_NP =(n_distinct(IDCONSUMO)-sum(CONSUMO_P)))

#Consumos pagados
consumo_p = consumo_listo %>% select(ID_USUARIO, IDCONSUMO, IDPRODUCTO, SE_PAGO) %>%filter(SE_PAGO==1)

#Agregacion de los productos consumidos-pagados
prod_paid = consumo_p  %>% group_by(ID_USUARIO) %>% summarise(PROD_P = n_distinct(IDPRODUCTO))

#Unir datos de resumen_1 y los productos pagados
resumen_2 = merge(x=resumen_1, y=prod_paid, by='ID_USUARIO', all.x = TRUE)

#Cambiar los valores NA a 0
resumen_2$PROD_P = ifelse(is.na(resumen_2$PROD_P),0,resumen_2$PROD_P)

#Creacion de la variable dias de consumo
dias = consumo_listo %>% select(ID_USUARIO, FECHACONSUMO) %>% group_by(ID_USUARIO) %>% summarise(DIAS_CONSUMO = n_distinct(FECHACONSUMO))

#Agregar los dias de consumo al resumen de datos
resumen_3 = merge(x=resumen_2, y=dias, by='ID_USUARIO', all.x = TRUE )

#Crear la variable consumo promedio por dia
resumen_3$CON_PROM_DIA = resumen_3$CONSUMO_TOTAL/resumen_3$DIAS_CONSUMO
rm(consumo_p, prod_paid, dias, resumen_1, resumen_2)

#Consumo por grupo de productos
prod_cus = table(consumo_listo$ID_USUARIO, consumo_listo$DESCGRUPOPROD)
prod_cus = as.data.frame.matrix(prod_cus)
prod_names = c('FA', 'FBP','MI','Otros','Perfil','PP','Reportes')
colnames(prod_cus) = prod_names
prod_cus = tibble::rownames_to_column(prod_cus, 'ID_USUARIO')
resumen_4 = merge(x=resumen_3, y=prod_cus, by='ID_USUARIO', all.x = TRUE)
rm(prod_cus, prod_names, resumen_3)

##Agregar CIIU del Usuario
area_cliente = usuarios_nueva %>% select(ID_USUARIO, USU_CIIU_AREA)
consumo_listo2 = merge(x=consumo_listo, y=area_cliente, by='ID_USUARIO', all.x= TRUE)
consumo_listo2$mismo_ciiu = ifelse(consumo_listo2$EMPCIIU_AREA == consumo_listo2$USU_CIIU_AREA, 1, 0)
ciiu_igual = consumo_listo2 %>% group_by(ID_USUARIO) %>% summarise(MISMO_CIIU = sum(mismo_ciiu))

#Consumo por estado de la empresa
est_emp = table(consumo_listo2$ID_USUARIO, consumo_listo2$EMPCONSUL_EST)
est_emp = as.data.frame.matrix(est_emp)
est_emp_names = c('ABSORBIDA', 'ACTIVA','ANULA_LIQ','CANCELACION','LISTA_CLINTON','DISUELTA','EXTINGUIDA','INACTIVA_TEMP', 
                  'INTERVENIDA','LEY_INSOLVENCIA', 'LIQUIDACION', 'REESTRUCTURACION', 'SALIDA_CLINTON')
colnames(est_emp) = est_emp_names
est_emp = tibble::rownames_to_column(est_emp, 'ID_USUARIO')

#########################   VARIABLES NUEVAS CREADAS   #########################

####################################
### CREACION DE REPOSIORIO FINAL  ##
####################################

##Agregando metricas al archivo de usuarios
usuarios_final = merge(x=usuarios_nueva, y=resumen_4, by='ID_USUARIO', all.x = TRUE)
usuarios_final = merge(x=usuarios_final, y=ciiu_igual, by='ID_USUARIO', all.x = TRUE)
usuarios_final = merge(x=usuarios_final, y=est_emp, by='ID_USUARIO', all.x = TRUE)
rm(area_cliente, ciiu_igual, consumo_listo, resumen_4, usuarios_nueva, est_emp, est_emp_names)

#Cambio de clase
numericas = c('CONSUMO_TOTAL','PROD_TOT', 'DIAS_CONSUMO', 'FA', 'FBP','MI','Otros','Perfil','PP','Reportes',
              'ABSORBIDA', 'ACTIVA','ANULA_LIQ','CANCELACION','LISTA_CLINTON','DISUELTA','EXTINGUIDA','INACTIVA_TEMP', 
              'INTERVENIDA','LEY_INSOLVENCIA', 'LIQUIDACION', 'REESTRUCTURACION', 'SALIDA_CLINTON')
usuarios_final[numericas] <- sapply(usuarios_final[numericas],as.numeric)
rm(numericas, est_emp_names)

#Cambiar clase de fecha cliente a fecha
usuarios_final$FECHA_CLIENTE = as.Date(usuarios_final$FECHA_CLIENTE, origin = "1970-01-01")

#Creacion de variable dicotomica basada en la ip del pais.
usuarios_final$PAIS = ifelse(usuarios_final$IP_Country=='Colombia',1,0)

#Chequeo de los datos
summary(usuarios_final)
str(usuarios_final)

######################## SELECCION DE VARIABLES  ################
rm(consumo_listo2)
colSums(is.na(usuarios_final_2))

#Primeras variables a eliminar
to_remove = c('FECHA_REGISTRO', 'FECHA_ALTA', 'FECHA_CLIENTE', 'USU_TELF', 'USU_CIIU', 'IP_Region')
usuarios_final_2 = usuarios_final %>% select(-to_remove)
rm(to_remove)

#Seleccion de variables segun el tipo
factores = usuarios_final_2 %>% select(which(sapply(.,is.factor)))%>% colnames()
numericas = usuarios_final_2 %>% select(which(sapply(.,is.numeric)))%>% colnames()
rm(num_2)

#Se eliminan usuarios con NA en todas las variables de consumo
usuarios_final_2 = usuarios_final_2[!is.na(usuarios_final_2$CONSUMO_TOTAL),]

#Analisis de correlaciones
cor1 = cor(usuarios_final_2[numericas], use = "complete.obs")
cor2 = cor(usuarios_final_2[num_2])
corrplot(cor1)
corrplot.mixed(cor2)

#Eliminar multicolinearidad
str(usuarios_final_2)
id = usuarios_final_2[1]
y = usuarios_final_2[usuarios_final_2$IND_CLIENTE]








write.csv(usuarios_final, "Usuarios_final.csv")

#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_1.RData")
save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_2.RData")
save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_3.RData") #Nuevo formato y se elimina consumo_listo2
#load("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_1.RData")




