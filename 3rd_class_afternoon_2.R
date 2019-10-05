setwd("C:/education/R/workspace")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caTools)
library(randomForest)
library(caret)
library(e1071)

install.packages("xgboost")
install.packages("DiagrammeR")
library(xgboost)
library(dplyr)
library(DiagrammeR)

df_breast_cancer <- read.csv("breast_cancer.csv")

View(df_breast_cancer)

#id 변수 제거
df_breast_cancer <- df_breast_cancer %>% 
  dplyr::select(-id)

View(df_breast_cancer)

df_breast_cancer$diagnosis <- ifelse(
                              df_breast_cancer$diagnosis == 'M',0,1)
View(df_breast_cancer)
table(df_breast_cancer$diagnosis)

set.seed(123)
sample = sample.split(df_breast_cancer, SplitRatio = 0.8)

trainset = subset(df_breast_cancer, sample == TRUE)

testset = subset(df_breast_cancer, sample == FALSE)

View(trainset)
View(testset)

clf <- xgboost(data = data.matrix(trainset %>% 
                                    dplyr::select(-diagnosis)),
                                  label = trainset$diagnosis,
                                  eta = 0.1,
                                  depth = 3,
                                  nrounds = 100,
                                  objective = "binary:logistic",
                                  eval_metrix = "logloss")
colnames(df_breast_cancer)[-1]
xgb.plot.tree(feature_names = colnames(df_breast_cancer)[-1],
              model = clf)
importance_matrix <- xgb.importance(colnames(df_breast_cancer)[-1],
                                    model =clf)
importance_matrix

xgb.plot.importance(importance_matrix)

pred = predict(clf, data.matrix(testset %>% dplyt::select(-diagnosis)))
View(pred)

pred <- ifelse(pred<=0.5, 0, 1)
table(pred)

confusionMatrix(factor(pred), factor(testset$diagnosis))

