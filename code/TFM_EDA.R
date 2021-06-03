---
  title: "Trabajo Final Master"
author: "Jose Rodriguez"
date: "April 20201"
---
  
#Carga de librerias
#library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(funModeling)
#library(VIM)
library(corrplot)
#library(car)
library(caTools)


##################################
### Lectura archivo de usuarios###
##################################
usuarios = read.delim('Linea 1 - Usuarios.txt', sep = ';', na.strings = c("","NA"))
colSums(is.na(usuarios))
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

##Eliminar duplicados
usuarios = usuarios[!duplicated(usuarios[,1:10]), ]

#Creacion de tablas de frecuencias y graficos de barras
freq(usuarios[factor_names])
rm(factor_names)

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
levels(consumo$DESCPRODUCTO)
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

############################# ARCHIVO USUARIO #############################

##Codigo de area en archivo de usuarios
usuarios$USU_CIIU_AREA = ifelse(nchar(as.character(usuarios$USU_CIIU)) > 4,substr(usuarios$USU_CIIU,1,1),'')

#A las variables nuevas, cambiarlas a tipo factor
factors_u = c('USU_CIIU_AREA')
usuarios[factors_u] <- lapply(usuarios[factors_u], factor)
rm(factors_u)

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
impu_pais = impu_pais %>% select(-IP_Country.y)

#Unir base con imputaciones con la que ya tenia los paises
usuarios_nueva = rbind(impu_pais, usuarios_con_pais)
rm(impu_pais, telxpais, tel_paises_1, usuarios_con_pais, usuarios_sin_pais, usuarios_ordered, usuarios)

#Creacion de variable dicotomica basada en la ip del pais.
usuarios_nueva$PAIS = ifelse(usuarios_nueva$IP_Country=='Colombia',1,0)
usuarios_nueva$PAIS = as.factor(usuarios_nueva$PAIS)
usuarios_nueva = usuarios_nueva %>% select(-IP_Country)

############################# ARCHIVO CONSUMO #############################

#Cambio de nombre en la variable para hacer la union con la base de usuarios
consumo = consumo %>% rename(ID_USUARIO = IDUSUARIO)

#Creacion de tabla temporal para agregarle la fecha cliente a la base de consumo
fecha_cliente = usuarios_nueva %>% select(ID_USUARIO, FECHA_CLIENTE)

#Agregando la fecha de cliente a la tabla de consumo
consumo = merge(x=consumo, y=fecha_cliente, by='ID_USUARIO', all.x = TRUE)

#Eliminar tablas temporales
rm(fecha_cliente)

##Codigo de area en archivo de consumo
consumo$EMPCIIU_AREA = ifelse(nchar(as.character(consumo$EMPCONSUL_CIIU)) > 4,substr(consumo$EMPCONSUL_CIIU,1,1),'')
consumo$EMPCIIU_DIV = ifelse(nchar(as.character(consumo$EMPCONSUL_CIIU)) > 4,substr(consumo$EMPCONSUL_CIIU,2,3),substr(consumo$EMPCONSUL_CIIU,1,2))

#Separacion de la base, en un lado los que si tienen consigo de area ciiu y en el otro los que no lo tienen
consumo = consumo %>% arrange(EMPCIIU_AREA)
consumo_na_ciiu = consumo %>% filter(is.na(EMPCIIU_AREA))
consumo_sin_ciiu = consumo %>% filter(EMPCIIU_AREA=='')
consumo_con_ciiu = consumo %>% filter(!is.na(EMPCIIU_AREA))
consumo_con_ciiu = consumo %>% filter(!EMPCIIU_AREA=='')

#Imputacion del area ciiiu
ciiu_codes = consumo %>% select(EMPCIIU_AREA, EMPCIIU_DIV) %>% filter(!EMPCIIU_AREA=='') %>% distinct()
consumo_sin_ciiu_new = merge(x=consumo_sin_ciiu, y=ciiu_codes, by='EMPCIIU_DIV', all.x = TRUE)
consumo_sin_ciiu_new = consumo_sin_ciiu_new %>% select(-EMPCIIU_AREA.x) %>% arrange(IDCONSUMO)
colnames(consumo_sin_ciiu_new)[14] = 'EMPCIIU_AREA'

consumo_listo = rbind(consumo_sin_ciiu_new, consumo_con_ciiu, consumo_na_ciiu)
consumo_listo = consumo_listo %>% select(-EMPCIIU_DIV)

#Eliminacion de duplicados
consumo_listo = consumo_listo[!duplicated(consumo_listo[,2]), ]

#Debido a los cambios realizados, se les debe de asignar nuevamente el tipo factor a las variables
consumo_listo$EMPCIIU_AREA = as.factor(consumo_listo$EMPCIIU_AREA)

#str(consumo_listo)
#summary(consumo_listo)

#Eliminar tablas temporales
rm(ciiu_codes,consumo,consumo_con_ciiu, consumo_sin_ciiu, consumo_sin_ciiu_new, consumo_na_ciiu)

#Se crea una variable dicotomica que indica 0-consumo gratuito y 1-consumo pagado
consumo_listo$SE_PAGO = ifelse(is.na(consumo_listo$FECHA_CLIENTE) | consumo_listo$FECHACONSUMO < consumo_listo$FECHA_CLIENTE,0,1)

#Se filtra unicamente por lo consumido previamente a hacerse cliente
consumo_prev = consumo_listo %>% filter(SE_PAGO == 0)

#Creacion de nuevas variables
#Aggregate consumo total & productos total & consumo pagado & consumo no pagado
resumen_1 = consumo_prev %>% group_by(ID_USUARIO) %>% summarise(CONSUMO_TOTAL = n_distinct(IDCONSUMO),PROD_TOT = n_distinct(IDPRODUCTO))

#Cambio de etiquetas
resumen_1$ID_USUARIO = recode(resumen_1$ID_USUARIO, '7e+06'='7000000')
resumen_1$ID_USUARIO = recode(resumen_1$ID_USUARIO, '8e+06'='8000000')
consumo_prev$ID_USUARIO = recode(consumo_prev$ID_USUARIO, '7e+06'='7000000')
consumo_prev$ID_USUARIO = recode(consumo_prev$ID_USUARIO, '8e+06'='7000000')

#Consumo por grupo de productos
prod_cus = table(consumo_prev$ID_USUARIO, consumo_prev$DESCGRUPOPROD)
prod_cus = as.data.frame.matrix(prod_cus)
prod_names = c('FA', 'FBP','MI','Otros','Perfil','PP','Reportes')
colnames(prod_cus) = prod_names
prod_cus = tibble::rownames_to_column(prod_cus, 'ID_USUARIO')
resumen_2 = merge(x=resumen_1, y=prod_cus, by='ID_USUARIO', all.x = TRUE)
rm(prod_cus, prod_names, resumen_1)
resumen_2 = resumen_2 %>% select(-Otros)

#Creacion de la variable dias de consumo
dias = consumo_prev %>% select(ID_USUARIO, FECHACONSUMO) %>% group_by(ID_USUARIO) %>% summarise(DIAS_CONSUMO = n_distinct(FECHACONSUMO))

#Agregar los dias de consumo al resumen de datos
resumen_3 = merge(x=resumen_2, y=dias, by='ID_USUARIO', all.x = TRUE )

#Crear la variable consumo promedio por dia
resumen_3$CON_PROM_DIA = resumen_3$CONSUMO_TOTAL/resumen_3$DIAS_CONSUMO
rm(dias, resumen_2)

#Consumo por estado de la empresa
est_emp = table(consumo_prev$ID_USUARIO, consumo_prev$EMPCONSUL_EST)
est_emp = as.data.frame.matrix(est_emp)
est_emp_names = c('ABSORBIDA', 'ACTIVA','ANULA_LIQ','CANCELACION','LISTA_CLINTON','DISUELTA','EXTINGUIDA','INACTIVA_TEMP', 
                  'INTERVENIDA','LEY_INSOLVENCIA', 'LIQUIDACION', 'REESTRUCTURACION', 'SALIDA_CLINTON')
colnames(est_emp) = est_emp_names
est_emp = tibble::rownames_to_column(est_emp, 'ID_USUARIO')

##Agregar CIIU del Usuario
area_cliente = usuarios_nueva %>% select(ID_USUARIO, USU_CIIU_AREA)
consumo_prev2 = merge(x=consumo_prev, y=area_cliente, by='ID_USUARIO', all.x= TRUE)
consumo_prev2$USU_CIIU_AREA = as.character(consumo_prev2$USU_CIIU_AREA)
consumo_prev2$USU_CIIU_AREA[consumo_prev2$USU_CIIU_AREA==''] =NA
consumo_prev2$USU_CIIU_AREA = as.factor(consumo_prev2$USU_CIIU_AREA)
consumo_prev2$mismo_ciiu = ifelse(consumo_prev2$EMPCIIU_AREA == consumo_prev2$USU_CIIU_AREA, 1, 0)
ciiu_igual = consumo_prev2 %>% group_by(ID_USUARIO) %>% summarise(MISMO_CIIU = sum(mismo_ciiu))


#########################   AGREGANDO NUEVAS VARIABLES A USUARIOS #########################

##Agregando metricas al archivo de usuarios
usuarios_final = merge(x=usuarios_nueva, y=resumen_3, by='ID_USUARIO', all.x = TRUE)
usuarios_final = merge(x=usuarios_final, y=ciiu_igual, by='ID_USUARIO', all.x = TRUE)
usuarios_final = merge(x=usuarios_final, y=est_emp, by='ID_USUARIO', all.x = TRUE)
rm(area_cliente, ciiu_igual, consumo_listo, consumo_prev,resumen_3, usuarios_nueva, est_emp, est_emp_names, factor_names)

#Cambio de clase
numericas = c('CONSUMO_TOTAL','PROD_TOT', 'DIAS_CONSUMO', 'FA', 'FBP','MI','Perfil','PP','Reportes',
              'ABSORBIDA', 'ACTIVA','ANULA_LIQ','CANCELACION','LISTA_CLINTON','DISUELTA','EXTINGUIDA','INACTIVA_TEMP', 
              'INTERVENIDA','LEY_INSOLVENCIA', 'LIQUIDACION', 'REESTRUCTURACION', 'SALIDA_CLINTON')
usuarios_final[numericas] <- sapply(usuarios_final[numericas],as.numeric)
rm(numericas)

#Cambiar clase de fecha cliente a fecha
#usuarios_final$FECHA_CLIENTE = as.Date(usuarios_final$FECHA_CLIENTE, origin = "1970-01-01")

#Chequeo de los datos
summary(usuarios_final)
str(usuarios_final)

######################## SELECCION DE VARIABLES INICIAL  ################
#Primeras variables a eliminar
to_remove = c('FECHA_REGISTRO', 'FECHA_ALTA', 'FECHA_CLIENTE', 'USU_TELF', 'USU_CIIU', 'IP_Region')
usuarios_final = usuarios_final %>% select(-to_remove)
rm(to_remove)

#Analisis de los datos
str(usuarios_final)

#Mover variable a predecir al final de la base
usuarios_final = usuarios_final %>% relocate(IND_CLIENTE, .after = SALIDA_CLINTON)


#######################################################
###### ELECCION DE VARIABLES Y CREACION DE BASES #####
######################################################

#Creacion de grupos
grupo_1 = usuarios_final %>% filter(!is.na(USU_TAMANIO))
grupo_2 = usuarios_final %>% filter(is.na(USU_TAMANIO))

################ ANALISIS GRUPO 1 ############################
#Usar una semilla para obtener siempre los mismos resultados
set.seed(111)

#Creacion de la muestra
muestra_1 = sample.split(grupo_1$IND_CLIENTE,SplitRatio = 0.8)

#Creacion base de entrenamiento y testeo
train_g1 = subset(grupo_1, muestra_1 == TRUE)
test_g1 = subset(grupo_1, muestra_1 == FALSE)

#Chequeo de bases
prop.table(table(train_g1$IND_CLIENTE))
prop.table(table(test_g1$IND_CLIENTE))

#Seleccion de variables numericas
numericas = train_g1 %>% select(which(sapply(.,is.numeric)))%>% colnames()

#Calculo de correlacion
cor_g1 = cor(train_g1[numericas], use = "complete.obs")
corrplot(cor_g1, tl.cex = 0.5)
rm(cor_g1)

#Relativizando variables de consumo
train_g1$FA = train_g1$FA/train_g1$CONSUMO_TOTAL
train_g1$FBP = train_g1$FBP/train_g1$CONSUMO_TOTAL
train_g1$MI= train_g1$MI/train_g1$CONSUMO_TOTAL
train_g1$Perfil = train_g1$Perfil/train_g1$CONSUMO_TOTAL
train_g1$PP = train_g1$PP/train_g1$CONSUMO_TOTAL
train_g1$Reportes = train_g1$Reportes/train_g1$CONSUMO_TOTAL

#Relativizando variables de estado
train_g1$ABSORBIDA = train_g1$ABSORBIDA/train_g1$CONSUMO_TOTAL
train_g1$ACTIVA = train_g1$ACTIVA/train_g1$CONSUMO_TOTAL
train_g1$ANULA_LIQ = train_g1$ANULA_LIQ/train_g1$CONSUMO_TOTAL
train_g1$CANCELACION = train_g1$CANCELACION/train_g1$CONSUMO_TOTAL
train_g1$LISTA_CLINTON = train_g1$LISTA_CLINTON/train_g1$CONSUMO_TOTAL
train_g1$DISUELTA = train_g1$DISUELTA/train_g1$CONSUMO_TOTAL
train_g1$EXTINGUIDA = train_g1$EXTINGUIDA/train_g1$CONSUMO_TOTAL
train_g1$INACTIVA_TEMP = train_g1$INACTIVA_TEMP/train_g1$CONSUMO_TOTAL
train_g1$INTERVENIDA = train_g1$INTERVENIDA/train_g1$CONSUMO_TOTAL
train_g1$LEY_INSOLVENCIA = train_g1$LEY_INSOLVENCIA/train_g1$CONSUMO_TOTAL
train_g1$LIQUIDACION = train_g1$LIQUIDACION/train_g1$CONSUMO_TOTAL
train_g1$REESTRUCTURACION = train_g1$REESTRUCTURACION/train_g1$CONSUMO_TOTAL
train_g1$SALIDA_CLINTON = train_g1$SALIDA_CLINTON/train_g1$CONSUMO_TOTAL

#Calculo de correlacion despues de relativizar
cor_g1_2 = cor(train_g1[numericas], use = "complete.obs")
corrplot(cor_g1_2, tl.cex = 0.5)
rm(cor_g1_2)

#Remover variables
remo_coli = c('FA','MI','Perfil','PP','Reportes', 'ABSORBIDA','ANULA_LIQ', 'LISTA_CLINTON', 'DISUELTA','EXTINGUIDA'
              ,'INACTIVA_TEMP', 'INTERVENIDA','LEY_INSOLVENCIA', 'LIQUIDACION', 'REESTRUCTURACION','SALIDA_CLINTON')
train_g1 = train_g1 %>% select(-remo_coli)
rm(remo_coli)

#Seleccion de variables categoricas (Sin incluir ID y ID_Cliente)
categoricas = train_g1 %>% select(which(sapply(.,is.factor)), -ID_USUARIO) %>% select(-IND_CLIENTE) %>% colnames() 

#Prueba chi cuadrado de independencia
prueba_chi_g1_1 = data.frame(lapply(train_g1[categoricas], function(x) chisq.test(table(x,train_g1$IND_CLIENTE), simulate.p.value = TRUE)$p.value))
prueba_chi_g1_2 = data.frame(lapply(train_g1[categoricas], function(x) chisq.test(table(x,train_g1$IND_CLIENTE))$p.value))
prueba_chi_g1_1
prueba_chi_g1_2
rm(prueba_chi_g1_1, prueba_chi_g1_2, usuarios_final, consumo_prev2)

#Seleccion de variables
eliminar = c('PAIS', 'TIPOUSUARIO')
train_g1 = train_g1 %>% select(-eliminar) 
rm(eliminar)

#Carga de nuevas librerias
library(ROSE)
library(caret)
library(Boruta)
library(randomForest)
library(rpart)
library(rpart.plot)
library(pROC)

#Remover momentaneamente el ID
id_g1 = train_g1 %>% select(ID_USUARIO)
g1_sin_ID = train_g1 %>% select(-ID_USUARIO)
g1_sin_ID_2 = g1_sin_ID %>% select(-IND_ALTA)

#Boruta feature selection
boruta_train_g1 = Boruta(IND_CLIENTE ~ ., data = na.omit(g1_sin_ID), doTrace = 2)
print(boruta_train_g1)
boruta_train_g1 <- TentativeRoughFix(boruta_train_g1)
print(boruta_train_g1)
getSelectedAttributes(boruta_train_g1, withTentative = F)
attStats(boruta_train_g1)

############# TEST PARA ELEGIR SAMPLING ########################
rejected = c('USU_TIPO', 'USU_ESTADO', 'USU_DEPARTAMENTO', 'USU_CIIU_AREA')
train_boruta_g1 = train_g1 %>% select(-rejected)
rm(rejected)

############# TEST FINALIZADO ########################

######################## FIN ANALISIS GRUPO 1 #########################

############# ANALISIS GRUPO 2 ###############################

#Creacion de la muestra
muestra_2 = sample.split(grupo_2$IND_CLIENTE,SplitRatio = 0.8)

#Creacion base de entrenamiento y testeo
train_g2 = subset(grupo_2, muestra_2 == TRUE)
test_g2 = subset(grupo_2, muestra_2 == FALSE)

#Chequeo de bases
prop.table(table(train_g2$IND_CLIENTE))
prop.table(table(test_g2$IND_CLIENTE))

#Eliminar variables con 100% de nulos
var_nulos = c('USU_TIPO', 'USU_TAMANIO', 'USU_ESTADO', 'USU_DEPARTAMENTO',  'USU_CIIU_AREA' , 'MISMO_CIIU')
train_g2 = train_g2 %>% select(-var_nulos)
rm(var_nulos)

#Boruta feature selection
boruta_train_g2 = Boruta(IND_CLIENTE ~ ., data = na.omit(train_g2[,2:32]), doTrace = 2)
print(boruta_train_g2)
boruta_train_g2 <- TentativeRoughFix(boruta_train_g2)
print(boruta_train_g2)
getSelectedAttributes(boruta_train_g2, withTentative = F)
attStats(boruta_train_g2)

#Variables confirmadas
confirmed = c('ID_USUARIO','IND_CLIENTE','CANAL_REGISTRO', 'IND_ALTA','BONDAD_EMAIL', 'IPCASOS', 'PROD_TOT', 'FBP', 'PP', 'CON_PROM_DIA', 'ACTIVA')
train_boruta_g2 =train_g2 %>% select(confirmed)


library(scales)
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, alpha = 0.3, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = alpha(as.integer(p)+1L, alpha = alpha), pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}
x = rose_rec_1[,c(3,8)]
decisionplot(tree_bal, rose_rec_1[,c(2,8:9,16)], class = "IND_CLIENTE", main = "CART")




write.csv(usuarios_final, "Usuarios_final.csv")

#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_1.RData")
#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_2.RData")
#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_5.RData") #Hasta eliminar variable con multi y chi
#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_6.RData") #Despues de boruta y elegir rose
#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_7.RData") #Despues de boruta y elegir rose en grupo 2
#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_8.RData") # CON todo
#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_9.RData") # Eliminando cosas
#load("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_1.RData")
#load("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_2.RData")
#load("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_9.RData")





