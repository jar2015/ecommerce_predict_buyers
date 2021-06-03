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
#Grupo 1 -> train_boruta_g1

#Bases codificadas
train_rec_1 = train_boruta_g1
train_rec_1$TIPOEMAIL = as.numeric(factor(train_rec_1$TIPOEMAIL))
train_rec_1$USU_TAMANIO = as.numeric(factor(train_rec_1$USU_TAMANIO))
test_rec_1 = test_g1
test_rec_1$TIPOEMAIL = as.numeric(factor(test_rec_1$TIPOEMAIL))
test_rec_1$USU_TAMANIO = as.numeric(factor(test_rec_1$USU_TAMANIO))

#Recodificacion de bases de test
test_g1$CONSUMO_TOTAL[is.na(test_g1$CONSUMO_TOTAL)] = 0
test_g1$PROD_TOT[is.na(test_g1$PROD_TOT)] = 0
test_g1$CON_PROM_DIA[is.na(test_g1$CON_PROM_DIA)] = 0
test_g1$DIAS_CONSUMO[is.na(test_g1$DIAS_CONSUMO)] = 0
test_g1$FBP[is.na(test_g1$FBP)] = 0
test_g1$MISMO_CIIU[is.na(test_g1$MISMO_CIIU)] = 0
test_rec_1$CONSUMO_TOTAL[is.na(test_rec_1$CONSUMO_TOTAL)] = 0
test_rec_1$PROD_TOT[is.na(test_rec_1$PROD_TOT)] = 0
test_rec_1$CON_PROM_DIA[is.na(test_rec_1$CON_PROM_DIA)] = 0
test_rec_1$DIAS_CONSUMO[is.na(test_rec_1$DIAS_CONSUMO)] = 0
test_rec_1$FBP[is.na(test_rec_1$FBP)] = 0
test_rec_1$MISMO_CIIU[is.na(test_rec_1$MISMO_CIIU)] = 0


#Bases balanceadas sin codificar
rose_1 = ROSE(IND_CLIENTE ~ ., data = train_boruta_g1, seed = 1)$data
both_1 = ovun.sample(IND_CLIENTE ~ ., data = train_boruta_g1, method = "both",
                     p = 0.5, na.action=options("na.action")$na.action,seed = 1)$data

#Bases balanceadas codificadas
rose_rec_1 = ROSE(IND_CLIENTE ~ ., data = train_rec_1, seed = 1)$data
both_rec_1 = ovun.sample(IND_CLIENTE ~ ., data = train_rec_1, method = "both", 
                         p = 0.5, na.action=options("na.action")$na.action,seed = 1)$data

#####################################################################################################
##########################            MODELOS / MODELOS / MODELOS          ##########################
#####################################################################################################

##########  ARBOLES DE DECISIONES
set.seed(111)
#ROSE REC
#dt_control <- rpart.control(minsplit = 5, maxdepth = 8, cp = 0)
#dt_control <- rpart.control(minsplit = 9, maxdepth = 10, cp = 0)
#dt_control <- rpart.control(minsplit = 5, maxdepth = 5, cp = 0)
#dt_control <- rpart.control(minsplit = 5, maxdepth = 4, cp = 0)
tree_bal <- rpart(IND_CLIENTE ~ ., data = both_rec_1[,2:16], control=dt_control)
pred_tree_bal <- predict(tree_bal, newdata = test_rec_1)
pred_tree_bal2 <- predict(tree_bal, newdata = test_rec_1, type = "class")
roc_bal = roc.curve(test_rec_1$IND_CLIENTE, pred_tree_bal[,2])
roc_bal
cm_bal = confusionMatrix(pred_tree_bal2, test_rec_1$IND_CLIENTE)
cm_bal
rpart.plot(tree_bal)
printcp(tree_bal)

tree_importance = data.frame(imp = tree_bal$variable.importance)
tree_data = tree_importance %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot2::ggplot(tree_data) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

rm(dt_control, roc_bal)

##########  RANDOM FOREST
set.seed(111)
rf_bal2 = randomForest(IND_CLIENTE ~., rose_1[,2:16], na.action=na.omit, mtry=4, ntree=1000)
rf_pred_bal2 <- predict(rf_bal2, newdata = test_g1, type = "class")
cm_rf_bal2 = confusionMatrix(rf_pred_bal2, test_g1$IND_CLIENTE)
cm_rf_bal2
pred_test_bal2 <- predict(rf_bal2, test_g1, index=2, type="prob", norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
pred_test_bal2 <- data.frame(pred_test_bal2)
pred_test_roc_bal2 <- roc(test_g1$IND_CLIENTE, pred_test_bal2$X1)
auc(pred_test_roc_bal2)
plot(pred_test_roc_bal2)
importance(rf_bal2)

########## REGRESION LOGISTICA
#Regresion logistica
#Base con sampling
rl_2 = glm(IND_CLIENTE ~., rose_rec_1[,2:16], family = "binomial",na.action=na.omit)
rl_g1_pred_2 <- predict(rl_2, newdata = test_rec_1, type="response")
rl_clases_2 = ifelse(rl_g1_pred_2 > 0.5, 1, 0)
rl_clases_2 = as.factor(rl_clases_2)
rl_cm_g1_2 = confusionMatrix(rl_clases_2 , test_rec_1$IND_CLIENTE)
rl_cm_g1_2
pred_rl2 <- as.numeric(rl_clases_2)
pred_rl_roc2 <- roc(test_rec_1$IND_CLIENTE, pred_rl2)
auc(pred_rl_roc2)
plot(pred_rl_roc2)


#save.image("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/Grupo_1.RData")
load("~/Documents/UOC - Master Data Science/6 - I Semester 2021/TFM/Bases y Análisis/Grupo_1.RData")

#################### ANEXOS #####################

##BALANCEADA (ROSE) SIN RECODIFICADAR 
#rf_bal = randomForest(IND_CLIENTE ~., rose_1[,2:16], na.action=na.omit)
#rf_pred_bal <- predict(rf_bal, newdata = test_g1, type = "class")
#cm_rf_bal = confusionMatrix(rf_pred_bal, test_g1$IND_CLIENTE)
#cm_rf_bal
#pred_test_bal <- predict(rf_bal, test_g1, index=2, type="prob", norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
#pred_test_bal <- data.frame(pred_test_bal)
#pred_test_roc_bal <- roc(test_g1$IND_CLIENTE, pred_test_bal$X1)
#auc(pred_test_roc_bal)
#plot(pred_test_roc_bal)

#Buscando la mejor combinacion de parametros
#cvcontrol <- trainControl(method="repeatedcv", number = 10, allowParallel=TRUE)
#train.rf <- train(IND_CLIENTE ~ .,
#                  data=rose_1[,2:16],
#                  method="rf",
#                  trControl=cvcontrol,
#                  #tuneLength = 3,
#                  na.action=na.omit,
#                  importance=TRUE)
#train.rf
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 16

##NO BALANCEADA NI RECODIFICADA
#rf_nobal = randomForest(IND_CLIENTE ~., train_boruta_g1[,2:16], na.action=na.omit)
#rf_nobal
#plot(rf_nobal)
#importance(rf_nobal)
#rf_pred_nobal <- predict(rf_nobal, test_g1)
#cm_rf_nobal = confusionMatrix(rf_pred_nobal , test_g1$IND_CLIENTE)
#cm_rf_nobal
#curva Roca
#pred_test <- predict(rf_nobal, test_g1, index=2, type="prob", norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
#head(pred_test)
#pred_test <- data.frame(pred_test)
#pred_test_roc <- roc(test_g1$IND_CLIENTE, pred_test$X1)
#auc(pred_test_roc)
#plot(pred_test_roc)

##NO BALANCEADA RECODIFICADA
#na.action=na.roughfix
#rf_nobal_rec = randomForest(IND_CLIENTE ~., train_rec_1[,2:16], na.action=na.omit)
#rf_pred_nobal_rec <- predict(rf_nobal_rec, newdata = test_rec_1, type = "")
#cm_rf_nobal_rec = confusionMatrix(rf_pred_nobal_rec, test_rec_1$IND_CLIENTE)
#cm_rf_nobal_rec
#pred_test_nobal_rec <- predict(rf_nobal_rec, test_rec_1, index=2, type="prob", norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
#head(pred_test)
#pred_test_nobal_rec <- data.frame(pred_test_nobal_rec)
#pred_test_roc_nobal_rec <- roc(test_rec_1$IND_CLIENTE, pred_test_nobal_rec$X1)
#auc(pred_test_roc_nobal_rec)
#plot(pred_test_roc_nobal_rec)

##BALANCEADA RECODIFICADA
#ROSE
#rf_bal_rec = randomForest(IND_CLIENTE ~., rose_rec_1[,2:16], na.action=na.omit)
#rf_pred_bal_rec <- predict(rf_bal_rec, newdata = test_rec_1, type = "class")
#cm_rf_bal_rec = confusionMatrix(rf_pred_bal_rec, test_rec_1$IND_CLIENTE)
#cm_rf_bal_rec
#pred_test_bal_rec <- predict(rf_bal_rec, test_rec_1, index=2, type="prob", norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
#pred_test_bal_rec <- data.frame(pred_test_bal_rec)
#pred_test_roc_bal_rec <- roc(test_rec_1$IND_CLIENTE, pred_test_bal_rec$X1)
#auc(pred_test_roc_bal_rec)
#plot(pred_test_roc_bal_rec)

#Base sin sampling
#rl_g1 = glm(IND_CLIENTE ~., train_rec_1[,2:16], family = "binomial",na.action=na.omit)
#rl_g1_pred <- predict(rl_g1, newdata = test_rec_1, type="response")
#rl_clases = ifelse(rl_g1_pred > 0.5, 1, 0)
#rl_clases = as.factor(rl_clases)
#rl_cm_g1 = confusionMatrix(rl_clases , test_rec_1$IND_CLIENTE)
#rl_cm_g1
#pred_rl <- as.numeric(rl_clases)
#pred_rl_roc <- roc(test_rec_1$IND_CLIENTE, pred_rl)
#auc(pred_rl_roc)
#plot(pred_rl_roc)

