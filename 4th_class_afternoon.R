setwd("C:/education/R/workspace")
x = c(3.0, 6.0, 9.0, 12.0)
y = c(3.0, 4.0, 5.5, 6.5)
m = lm( y ~ x)
m

plot(x, y)
abline(m, col='red')

coef(m)
fitted(m)
residuals(m)
deviance(m)/length(x)

summary(m)

newx = data.frame(x=c(1.2, 2.0, 20.65))
predict(m, newdata=newx)


#단순 선형 회귀 적용 : cars 데이터

str(cars)
head(cars)
plot(cars)
car_model = lm(dist~speed, data=cars)
coef(car_model)
abline(car_model, col='red')

nx1 = data.frame(speed=c(21.5))
predict(car_model, nx1)
nx2 = data.frame(speed = c(25.0, 25.5, 26.0, 26.5,
                           27, 27.5, 28.0))
predict(car_model, nx2)

nx = data.frame(speed=c(21.5, 25.0, 25.5, 26.0, 26.5, 27.0, 
                        27.5, 28.0))
plot(nx$speed, predict(car_model, nx), col='red',
   
       cex=2, pch=20)
abline(car_model)

#고차 다항식 적용과 분산 분석(ANOVA)

plot(cars, xlab='속도', ylab='거리')
x=seq(0, 25, length.out = 200)
x
df <- data.frame(speed=x)
df

for (i in 1:4) {
  m = lm(dist~poly(speed, i), data=cars)
  assign(paste('m', i, sep='.'),m)
  lines(x, predict(m, data.frame(speed=x)), col=i)
}