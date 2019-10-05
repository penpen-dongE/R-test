setwd("C:/education/R/workspace")
df_train <- read.csv("./data/titanic/train.csv", na.string="")

View(df_train)
str(df_train)
colSums(is.na(df_train))
nrow(df_train)
df_nan=colSums(is.na(df_train))/nrow(df_train)
df_nan
table(df_train["Survived"])
table(df_train["Pclass"])
library(dplyr)
df_train %>% 
  select(Pclass,Survived) %>% 
  group_by(Pclass) %>% 
  table

library(ggplot2)
ggplot(df_train, aes(x=Pclass, fill=factor(Survived))) +
  geom_bar(width=0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

df_train %>% 
  select(Sex, Survived) %>% 
  group_by(Sex) %>% 
  table

ggplot(df_train, aes(x=Sex, fill=factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

df_train %>% 
  na.omit() %>% 
  select(Survived, Pclass, Age, SibSp, Parch, Fare) %>% 
  cor

library(fBasics)
df_train %>% 
  select(Survived, Pclass, Age, SibSp, Parch, Fare) %>% 
  skewness

hist(df_train$Fare)
df_train$Fare=log1p(df_train$Fare)
hist(df_train$Fare)

df_train <-
  df_train %>% 
  mutate(Family_size = SibSp+Parch+1)

View(df_train)

df_train %>% 
  select(Family_size, Survived) %>% 
  group_by(Family_size) %>% 
  table

ggplot(df_train, aes(x=Family_size, fill = factor(Survived))) +
         geom_bar(width = 0.5) +
         xlab("Family_size") +
         ylab("Total Count") +
         labs(fill = "Survived")
       
title <- gsub("^.*, (.*?)\\..*$","\\1",df_train$Name)
View(title)
table(title)

df_train <-
  df_train %>% 
  mutate(Title = ifelse (
    title %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", title),
    Title = ifelse(title == "Mme", "Mrs", title),
    Title = ifelse(title %in% c("Capt", "Col", 
                                "Major", "Dr", "Rev", "Don",
                                "Sir", "the Countess",
                                "Jonkheer"),
                   "officer", title))

table(df_train$Title)

title_age_mean <-
  df_train %>% 
  select(Title, Age) %>% 
  group_by(Title) %>% 
  summarise(Title_mean=mean(Age, na.rm = TRUE))

View(title_age_mean)

title_age_mean_df = as.data.frame(title_age_mean)
View(title_age_mean_df)

age_nan_index=which(is.na(df_train$Age))
age_nan_index

df_train[6, ]
df_train[6,6]
df_train[6,6] <- 0

for (index in c(age_nan_index)){
  
  title = df_train[index, 14]
  title_age_mean <-
    title_age_mean_df %>%  dplyr::filter(Title==title) %>% 
    select(Title_mean)
  print(paste0("index:",index,":title:",title,
               ":title_age_mean :", title_age_mean))
  df_train[index,6] <- title_age_mean
}

View(df_train)

df_train$Embarked <-
  if_else(is.na(df_train$Embarked), "S", df_train$Embarked)

df_train$Embarked <- replace(df_train$Embarked, which(
  is.na(df_train$Embarked)),'S')

df_train$Age %/% 10

df_train <-
  df_train %>% 
  mutate(Age_cat = Age %/% 10)

View(df_train)

df_train <- subset(df_train,
                   select = -c(PassengerId, Cabin, Name, Age, Ticket))

df_train <- na.omit(df_train)

df_train$Survived <- as.integer(df_train$Survived)
str(df_train)

library(caTools)
library("e1071")
sample = sample.split(df_train, SplitRatio = 0.8)
trainset = subset(df_train, sample==TRUE)
testset=subset(df_train, sample==FALSE)

View(trainset)
View(testset)

install.packages("xgboost")
install.packages("DiagrammeR")
library(xgboost)
library(dplyr)
library(DiagrammeR)

clf <- xgboost(data= data.matrix(trainset %>% 
                                   dplyr::select(-Survived)),
               label = trainset$Survived,
               eta = 0.1,
               depth = 3,
               nrounds = 100,
               objective = "binary:logistic",
               eval_metric = "logloss")

pred = predict(clf , data.matrix(testset %>% dplyr::select(-Survived)))
View(pred)

pred <- ifelse(pred<=0.5, 0, 1)
table(pred)
library(caret)
confusionMatrix(factor(pred), factor(testset$diagnosis))
confusionMatrix(factor(pred), factor(testset$Survived))

importance_matrix <- xgb.importance(colnames(trainset)[-1],
                                    model = clf)
importance_matrix

xgb.plot.importance(importance_matrix)
