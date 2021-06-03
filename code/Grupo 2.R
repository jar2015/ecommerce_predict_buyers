#load("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/TFM_7.RData")

#Carga de librerias
library(dplyr)
library(stringr)
library(tidyverse)
library(funModeling)
library(corrplot)
library(caTools)
library(ROSE)
library(caret)
library(Boruta)
library(randomForest)
library(rpart)
library(rpart.plot)
library(pROC)
library(ggplot2)
library(ROCR)

#Resultado de seleccion de variables por medio de Boruta
attStats(boruta_train_g2)

#Base original desbalanceada sin codificar
#Grupo 2 -> train_boruta_g2

#Sustituir NA a 0
train_boruta_g2$PROD_TOT[is.na(train_boruta_g2$PROD_TOT)] = 0
train_boruta_g2$FBP[is.na(train_boruta_g2$FBP)] = 0
train_boruta_g2$PP[is.na(train_boruta_g2$PP)] = 0
train_boruta_g2$CON_PROM_DIA[is.na(train_boruta_g2$CON_PROM_DIA)] = 0

#Bases balanceadas
rose_g2 = ROSE(IND_CLIENTE ~ ., data = train_boruta_g2, seed = 1)$data

#Base de prueba
test_g2

#####################################################################################################
##########################            MODELOS / MODELOS / MODELOS          ##########################
#####################################################################################################

##########  ARBOLES DE DECISIONES
set.seed(111)
dt_control_g2 <- rpart.control(minsplit = 3, maxdepth = 2, cp = 0)
tree_bal_g2 <- rpart(IND_CLIENTE ~ ., data = rose_g2[,2:11], control=dt_control_g2)
pred_tree_bal_g2 <- predict(tree_bal_g2, newdata = test_g2)
pred_tree_bal2_g2 <- predict(tree_bal_g2, newdata = test_g2, type = "class")
roc_bal_g2 = roc.curve(test_g2$IND_CLIENTE, pred_tree_bal_g2[,2])
roc_bal_g2
cm_bal_g2 = confusionMatrix(pred_tree_bal2_g2, test_g2$IND_CLIENTE)
cm_bal_g2
rpart.plot(tree_bal_g2)
printcp(tree_bal_g2)

#Grafica de importancia
tree_importance2 = data.frame(imp = tree_bal_g2$variable.importance)
tree_data2 = tree_importance2 %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot2::ggplot(tree_data2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

##########  RANDOM FOREST
set.seed(111)
rf_bal_g2 = randomForest(IND_CLIENTE ~., rose_g2[,2:11], na.action=na.omit, mtry=3, ntree=250)
rf_pred_bal_g2 <- predict(rf_bal_g2, newdata = test_g2, type = "class")
cm_rf_bal_g2 = confusionMatrix(rf_pred_bal_g2, test_g2$IND_CLIENTE)
cm_rf_bal_g2
pred_test_bal_g2 <- predict(rf_bal_g2, test_g2, index=2, type="prob", norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
pred_test_bal_g2 <- data.frame(pred_test_bal_g2)
pred_test_roc_bal_g2 <- roc(test_g2$IND_CLIENTE, pred_test_bal_g2$X1)
auc(pred_test_roc_bal_g2)
plot(pred_test_roc_bal_g2)
importance(rf_bal_g2)

############ REGRESION LOGISTICA
rl_g2 = glm(IND_CLIENTE ~., rose_g2[,2:11], family = "binomial",na.action=na.omit)
rl_g1_pred_g2 <- predict(rl_g2, newdata = test_g2, type="response")
rl_clases_g2 = ifelse(rl_g1_pred_g2 > 0.5, 1, 0)
rl_clases_g2 = as.factor(rl_clases_g2)
rl_cm_g1_g2 = confusionMatrix(rl_clases_g2 , test_g2$IND_CLIENTE)
rl_cm_g1_g2
pred_rl_g2 <- as.numeric(rl_clases_g2)
pred_rl_roc_g2 <- roc(test_g2$IND_CLIENTE, pred_rl_g2)
auc(pred_rl_roc_g2)
plot(pred_rl_roc_g2)
summary(rl_g2)

#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/Grupo_2.RData")
load("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/Grupo_2.RData")
