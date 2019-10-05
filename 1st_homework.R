setwd("c:/education/R/workspace")
install.packages("plyr")
install.packages("stringr")
install.packages("dygraphs")
install.packages("zoo")
install.packages("xts")
install.packages("dplyr")


library4 <-c("ggplot2","stringr","dygraphs","zoo","xts","dplyr")
unlist(lapply(library4, require, 
              character.only = TRUE))
product <- read.csv("./data/product.csv",header = T, stringsAsFactors = F)
weather <- read.csv("./data/weather.csv", header = T, stringsAsFactors = F)
code <- read.csv("./data/code.csv", header = T, stringsAsFactors = F)
head(product)
View(weather)
code_rownum <-grep("지역코드",code$구분코드설명)
code_rownum
code[code_rownum,]
category <-subset(code, code$구분코드설명=="품목코드")
category
colnames(product) <- c('date','category','item','region','mart','price')
colnames(category)<-c('code','exp','item','name')
seoul_item<-product[which(product$region==1101),]
View(seoul_item)
seoul_item %>% arrange(item) %>% View
seoul_item <- seoul_item %>% 
              group_by(item, date) %>% 
              summarise(mean_price=mean(price))
View(seoul_item)
View(category)
install.packages("dplyr")
library(dplyr)
seoul_item <- left_join(seoul_item, category, by="item")
View(seoul_item)
seoul_item_mean <- seoul_item[, c("item","date",
                                  "name","mean_price")]
head(seoul_item_mean)
colnames(seoul_item_mean)<- c('item','date','item_name',
                              'mean_price')
head(seoul_item_mean, n=10)
str(weather)
colnames(weather) <- c('region', 'category', 'value', 'date')
seoul_rownum <- grep("서울", weather$region)
seoul_rownum
seoul_weather <- weather[seoul_rownum,]
View(seoul_weather)
seoul_rain <- seoul_weather %>% filter(category=="강수량")
View(seoul_rain)
seoul_item_rain <- left_join(seoul_rain, seoul_item_mean, by="date")
View(seoul_item_rain)
seoul_item_rain <- seoul_item_rain %>% filter(value>0)
View(seoul_item_rain)
seoul_item_rain <- seoul_item_rain %>% 
                  filter(item_name %in% c("상추", "호박"))
View(seoul_item_rain)
str(seoul_item_rain)
seoul_item_rain$date <- as.Date(seoul_item_rain$date)
str(seoul_item_rain)
ggplot(data=seoul_item_rain, aes(x=date))+
        geom_line(aes(y=mean_price, colour = item_name))
ggplot(data=seoul_item_rain, aes(x=date))+
         geom_line(aes(y=mean_price, colour = item_name))+
            geom_line(aes(y=value,colour="강수량"))
       
ggplot(data=seoul_item_rain,aes(x=date))+
  geom_line(aes(y=mean_price, colour = item_name))+
  geom_line(aes(y=value*10,colour = "강수량"))

ggplot(data=seoul_item_rain,aes(x=date))+
  geom_line(aes(y=mean_price,colour=item_name))+
  geom_line(aes(y=value*10, colour="강수량"))+
  ylab("변화량")+xlab("날짜")+
  labs(colour="비교대상",
       title="강수량과 농작물 가격 비교")
