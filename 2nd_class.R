setwd("C:/education/R/workspace")
library(dplyr)
exam <- read.csv("csv_exam.csv")
View(exam)

exam %>% filter(class == 1)
exam %>% filter(class != 1)
exam %>% filter(math > 50)
exam %>% filter(english >= 80)
exam %>% filter(class==1 & math >= 50)
exam %>% filter(class == 2 & english >= 80)
exam %>% filter(math >= 90 | english >= 90)
exam %>% filter(english <= 90 | science < 50)
exam %>% filter(class %in% c(1,3,5))
class1 <- exam %>% filter(class == 1)
mean(class1$math)
View(class1)
class2 <- exam %>% filter(class == 2)
exam %>% select(math)
exam %>% select(english)
exam %>% select(class, math, english)
exam %>% select(-math)
exam %>% select(-math, -english)
exam %>% filter(class == 1) %>% select(english)
exam %>% 
  filter(class == 1) %>% 
  select(english)
exam %>% 
  select(id, math) %>% 
  head(6)
exam %>% arrange(math)
exam %>% arrange(desc(math))
exam %>% arrange(class, math)
exam %>% 
  mutate(total = math + english + science) %>% 
  head(6)
exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3 ) %>% 
  head(5)
exam %>% 
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>% 
  head(5)
exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head(5)
exam %>% summarise(mean_math = mean(math))
exam %>% group_by(class) %>% 
  summarise(mean_math = mean(math))
exam %>% group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())
mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)
library(ggplot2)
install.packages("ggplot2")

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)
mpg %>% group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(tot = (cty+hwy)/2) %>%
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,80))
total <- left_join(test1, test2, by = "id")
total
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name
data <- read.csv("./data/ch02.csv", header=F, na.strings = c("."))
str(data)
data$V1 <- factor(data$V1, levels = c(1,2),
                  labels = c("남자", "여자"))
data$V3 <- factor(data$V3, levels=1:14, 
                  labels=c("가구주",  "가구주의 배우자",  "자녀",  "자녀의 배우자",  "가구주의 부모",  "배우자의 부모",  "손자녀, 그 배우자",  "증손자녀, 그 배우자",  "조부모",  "형제자매, 그 배우자",  "형제자매의 자녀, 그 배우자",  "부모의 형제자매, 그 배우자",  "기타 친인척",  "그외같이사는사람") )
data$V4 <- factor(data$V4, levels=1:8, 
                  labels=c("안 받았음", "초등학교", "중학교", "고등학교", "대학-4년제 미만", "대학-4년제 이상", "석사과정", "박사과정") )
str( data )
save.image("data.rda")
head(data)
tail (data)
ranicafe <- read.csv("./data/cafedata.csv")
head(ranicafe)
head(ranicafe$Coffees)
table(ranicafe$Coffees)
ranicafe <- read.csv("./data/cafedata.csv", header=T, na.strings="na", 
                     stringsAsFactors=FALSE )
ranicafe <- na.omit(ranicafe)
str(ranicafe)

install.packages("ggplot2")
library(ggplot2)

ggplot(ranicafe, aes(Coffees)) + 
  geom_bar(fill="gray") + 
  ggtitle("라니의 카페 커피 판매량") + 
  theme(plot.title = element_text(size = 20, face="bold")) +
  xlim(0, 50) + xlab("판매량") +
  ylab("횟수") + scale_y_continuous(breaks=0:10)
sort(ranicafe$Coffees)
sort(ranicafe$Coffees)[1]
sort(ranicafe$Coffees, decreasing = TRUE)
sort(ranicafe$Coffees, decreasing = TRUE)[1]
min(ranicafe$Coffees)
max(ranicafe$Coffees)
rc <- ranicafe$Coffees
stem(rc)
weight <- (1/length(rc))
weight
sum (rc*weight)
rc
length(rc)
rc <- c(rc, NA)
tail (rc, n=5)
mean (rc)
mean(rc, na.rm = TRUE)

rc <- ranicafe$Coffees
weight <- (1/length(rc))
sum(rc*weight)
rc[rc ==max(rc)]
max(rc)
rc <- ranicafe$Coffees
max(rc)
rc[rc == max(rc)] <- 480
mean (rc)

median(rc)
height <- c(164, 166, 168, 170, 172, 174,176)
(height.m <- mean(height))
(height.dev <- height - height.m)
sum(height.dev)

(height.dev2 <- height.dev^2)
sum(height.dev^2)
mean(height.dev^2)

sqrt(mean(height.dev^2))
var(height)
sd(height)
quantile(rc)
(qs <- quantile(rc))
print(qs[4]-qs[2])
IQR(rc)
bp <- boxpolt(rc, main="커피 판매량에 대한
              상자도표", axes=F)
