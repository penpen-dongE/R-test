setwd("C:/education/R/workspace")
install.packages("rattle")
installed.packages("e1071")
install.packages("caret")
install.packages("rpart.plot")
install.packages("caTools")
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caTools)

df_play_tennis <-read.csv("play_tennis.csv")
View(df_play_tennis)
set.seed(123)
sample = sample.split(df_play_tennis, SplitRatio = 0.75)
trainset = subset(df_play_tennis, sample == TRUE)
testset = subset(df_play_tennis, sample == FALSE)
View(trainset)
View(testset)

tree1 = rpart(PlayTennis~., data=trainset, 
              control = rpart.control(minsplit=2))
printcp(tree1)

rpart.plot(tree1)
tree1

pred <- predict(tree1, testset, type='class')
View(pred)

confusionMatrix(pred, testset$PlayTennis)

precision(testset$PlayTennis, pred)
recall(testset$PlayTennis, pred)

