setwd("C:/education/R/workspace")
install.packages("randomForest")
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caTools)
library(randomForest)
library(ggplot2)

#iris 데이터셋 조회
View(iris)

table(iris$Species)

set.seed(123)
sample = sample.split(iris, SplitRatio = 0.8)
trainset = subset(iris, sample ==TRUE)
testset = subset(iris, sample ==FALSE)
View(trainset)
View(testset)

tree1 <- randomForest(Species ~., trainset)
tree1

#randomForest 그래프 그리기
opar <- par(mfrow=c(1,2))
plot(tree1)
varImpPlot(tree1)
par(opar)

pred <- predict(tree1, testset)
View(pred)

confusionMatrix(pred, testset$Species)
