setwd('c:/education/R/workspace')
df_train <- read.csv("./data/titanic/train.csv",
                     na.strings="")
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
ggplot(df_train, aes(x=Pclass, fill=factor(Survived)))+
  geom_bar(width = 0.5) + 
  xlab("Pclass") +
  ylab("Total count")+
  labs(fill = "Survived")

df_train %>% 
  select(Sex,Survived) %>% 
  group_by(Sex) %>% 
  table

ggplot(df_train, aes(x=Sex, fill = factor(Survived)))+
  geom_bar(width = 0.5) +
  xlab("Sex")+
  ylab("Total count") +
  labs(fill = "Survived")

df_train %>% 
  na.omit() %>% 
  select(Survived, Pclass, Age, SibSp, Parch, Fare) %>% 
  cor

install.packages("fBasics")
library('fBasics')
library(dplyr)
df_train %>% 
  select(Survived,Pclass,Age,SibSp,Parch,Fare) %>% 
  skewness

hist(df_train$Fare)

df_train$Fare=log1p(df_train$Fare)
hist(df_train$Fare)

df_train <-
  df_train %>% 
    mutate(Family_Size= SibSp+Parch+1)

View(df_train)

df_train %>% 
  select(Family_Size, Survived) %>% 
  group_by(Family_Size) %>% 
  table

ggplot(df_train, aes(x= Family_Size, fill= factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Family_Size") +
  ylab("Total Count") +
  labs(fill= "Survived")

title <- gsub("^.*, (.*?)\\..*$","\\1",df_train$Name)
View(title)
table(title)

df_train <-
  df_train %>% 
  mutate(Title = ifelse(
    title %in% c("Mlle","Ms","Lady","Dona"),
                  "Miss", title),
    Title = ifelse(title == "Mme", "Mrs", title),
    Title = ifelse(title %in% c("Capt", "Col",
                                "Major","Dr","Rev","Don",
                                "Sir","the Countess",
                                "Jonkheer"),
                   "officer", title)
  )

table(df_train$Title)

title_age_mean <-
  df_train %>% 
    select(Title,Age) %>% 
    group_by(Title) %>% 
    summarise(Title_mean=mean(Age, na.rm = TRUE))
View(title_age_mean)

title_age_mean_df = as.data.frame(title_age_mean)
View(title_age_mean_df)

age_nan_index=which(is.na(df_train$Age))
age_nan_index

df_train[6,]
df_train[6,6]
df_train[6,6] <- 0

for (index in c(age_nan_index)){
  title = df_train[index,14]
  
  title_age_mean <-
    title_age_mean_df %>%  dplyr::filter(Title==title) %>% 
    select(Title_mean)
  print(paste0("index:",index,":title:",title,
               ":title_age_mean :", title_age_mean))
  df_train[index,6] <- title_age_mean
}
