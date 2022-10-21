setwd("E:/tjo_r/Projet/Project")

library(tidyverse)
library(FactoMineR)
library(randomForest)
library(rpart)
library (gbm)
library(xgboost)
library(glmnet)
library('caret')
library(ggplot2)

df_in = read.csv('Data/input_training_processed.csv', sep = ',')
df_out = read.csv('Data/output_training.csv', sep = ',')

df_out = subset(df_out, select = -c(ID))
df_in = subset(df_in, select = -c(X))

df_in = df_in %>% 
  mutate(Y = df_out$SO2_MAS)

####Création de la variable bloc qui constitue les x différents bloc pour la validation croisée#####

nb_bloc = 10

set.seed(1234)

bloc = sample(rep(1:nb_bloc,length=nrow(df_in)))
df_in$group = bloc
df_in$group

RES = data.frame(Y=df_out)

YY <- df_out$SO2_MAS
XX <- as.matrix(df_in)

for(ii in 1:nb_bloc){
  donA = df_in[bloc!=ii,]
  donT = df_in[bloc == ii,]
  
  XXA =  XX[bloc!=ii,]
  XXT =  XX[bloc==ii,]
  YYA =  YY[bloc!=ii]
  YYT =  YY[bloc==ii]
            
            model_total = lm(Y~.,data=donA)
            RES[bloc==ii,"model_MCO"] = predict(model_total,donT)
            
            modele_step = step(model_total)
           RES[bloc==ii,"model_step"] = predict(modele_step,donT)
            
            model_ridge = cv.glmnet(XXA,YYA,alpha=0,nfolds=10)
            RES[bloc==ii,"model_ridge_min"] = predict(model_ridge,XXT, s=model_ridge$lambda.min)
            RES[bloc==ii,"model_ridge_1se"] = predict(model_ridge,XXT, s=model_ridge$lambda.1se)
            
            model_lasso = cv.glmnet(XXA,YYA,alpha=1,nfolds=10)
            RES[bloc==ii,"model_lasso_min"] = predict(model_lasso,XXT, s=model_lasso$lambda.min)
            RES[bloc==ii,"model_lasso_1se"] = predict(model_lasso,XXT, s=model_lasso$lambda.1se)
            
            model_elas = cv.glmnet(XXA,YYA,alpha=0.5,nfolds=10)
            RES[bloc==ii,"model_elas_min"] = predict(model_elas,XXT, s=model_elas$lambda.min)
            RES[bloc==ii,"model_elas_1se"] = predict(model_elas,XXT, s=model_elas$lambda.1se)
            
            model_random_forest_500 = randomForest(Y~.,data=donA, ntree=500)
            RES[bloc==ii,"model_random_forest_500"] = predict(model_random_forest_500,donT)
            
            model_random_forest_100 = randomForest(Y~.,data=donA, ntree=100)
            RES[bloc==ii,"model_random_forest_100"] = predict(model_random_forest_100,donT)
            
            model_arbre = rpart(Y~.,data=donA)
            RES[bloc==ii,"model_arbre"] = predict(model_arbre,donT)
            
            model_gbm = gbm(Y~.,data=donA, distribution ="gaussian",n.trees=1000)
            RES[bloc==ii,"model_gbm"] = predict(model_gbm,donT,n.tree=gbm.perf(model_gbm))
            
            xgb_train = xgb.DMatrix(data = XXA, label= YYA)
            xgb_test = xgb.DMatrix(data = XXT, label= YYT)
            
            # model_xgboost = xgboost(data = XXA,
            #                         label = YYA,
            #                         max_depth = 2, 
            #                         eta = 1, nthread = 2, 
            #                         nrounds = 2,
            #                         objective = "rmse")
            # RES[bloc==ii,"model_xgboost"] = predict(model_xgboost, xgb_test)
            }

saveRDS(RES,"RES.RDS")


RES


monerreur <- function(X,Y){mean((X-Y)^2)}
sort(apply(RES,2,monerreur,Y=RES$SO2_MAS)[-1])


df_in_test = read.csv('Data/input_test_processed.csv', sep = ',')
df_out_test = read.csv('Data/output_test.csv', sep = ',')

df_out_test = subset(df_out_test, select = -c(ID))
df_in_test = subset(df_in_test, select = -c(X))


RES_test = data.frame(Y=df_out_test)



