//시행횟수는 6

n <- 6  

//성공확률은 1/3이고 이를 변수 p에 저장
p <- 1/3

//확률변수 x가 가질 수 있는 값의 범위 지정
x <- 0:n

//확률질량함수 구하기.
(dbinom(2, size=n, prob=p))
(dbinom(4, size=n, prob=p))
(px <- dbinom(x, size=n, prob = p))
plot(x, px, type = "s", xlab="성공횟수(x)", ylab="확률(p[X=x})",
      main="B(6, 1/3)")

height <- c(1196,1340,1232,1184,1295,1247,1201,1185,
              1192,1287,1159,1160,1243,1264,1276)
install.packages('sqldf')
library('sqldf')
mean(height)
stdev(height)
sqrt(15)
()
qt(0.025, df=14)

qt(0.95, df=17)


data <- read.table("./data/babyboom.dat.txt",header = F)
str(data)
names(data) <- c('time','gender','weight','minutes')
tmp <- subset(data, gender==1)
weight <- tmp[[3]]
barx <- mean(weight)
s <- sd(weight)
n <- length(weight)
h0 <- 2800
(t.t <- (barx -h0)/(s/sqrt(n)))
alpha <- 0.05
(c.u <- qt(1-alpha, df=n-1))
(p.value <- 1 - pt(t.t, df=n-1))

t.test(weight, mu=2800, alternative = "greater")


tmp <- read.table("./data/restitution.txt", header=T)
rel <- ifelse(tmp$rst < 0.4134 | tmp$rst > 0.4374, 1, 0)
n <- length(rel)
nos <- sum(rel)
sp <- nos/n
hp <- 0.1
(z <- (sp - hp) / sqrt((hp*(1-hp))/n))
alpha <- 0.05
(c.u <- qnorm(1-alpha))
(p.value <- 1 - pnorm(z))


prop.test(nos, n, p=0.1, alternative="greater", correct=FALSE)
