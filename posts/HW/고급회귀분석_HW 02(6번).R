getwd()
setwd('D:/포맷/개인적/대학원/고급회귀분석론/')

library(ggplot2)


dt <- data.frame(x = c(1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978),
                  y1 = c(25.82,25.35,24.29,24.05,24.89,25.35,25.23,25.06,27.13,27.36,26.65,27.13,27.49,27.08,27.51,27.54,26.21),
                 y2 = c(18.24,16.50,20.26,20.97,19.43,19.31,20.85,19.54,20.49,21.91,22.51,18.81,19.42,19.10,18.80,18.80,17.57))

dt



plot(dt$x, dt$y1, 
     xlab = "YEAR",
     ylab = "High",
     pch  = 16,  #포인트의모양
     cex  = 2,   #포인트의 크기
     col  = "darkorange")


plot(dt$x, dt$y2, 
     xlab = "YEAR",
     ylab = "low",
     pch  = 16, 
     cex  = 2,  
     col  = "darkorange")



plot(dt$y1, dt$y2, 
     xlab = "High",
     ylab = "Low",
     pch  = 16,
     cex  = 2,   
     col  = "darkorange")


model1 <- lm(y1 ~ x, dt) 
model1

summary(model1) 


model2 <- lm(y2 ~ x, dt)
model2

summary(model2)

model3 <- lm(y1 ~ y2, dt)
model3
summary(model3)


dt2 <- data.frame(x = c(1962,1963,1964,1965,1966,1967,1968,1969),
                 y = c(25.82,25.35,24.29,24.05,24.89,25.35,25.23,25.06,18.24,16.50,20.26,20.97,19.43,19.31,20.85,19.54))

dt2

plot(y~x, data=dt2, 
     xlab = "Year",
     ylab = "아마존강수위",
     pch  = 16,  
     cex  = 2,   
     col  = "darkorange")

res<-lm(y~x, data=dt2)
abline(res)


model4 <- lm(y ~ x, dt2)
model4

summary(model4)
anova(model4)


dt3 <- data.frame(x = c(1970,1971,1972,1973,1974,1975,1976,1977,1978),
                  y = c(27.13,27.36,26.65,27.13,27.49,27.08,27.51,27.54,26.21,20.49,21.91,22.51,18.81,19.42,19.10,18.80,18.80,17.57))

dt3

plot(y~x, data=dt3, 
     xlab = "Year",
     ylab = "아마존강수위",
     pch  = 16,  #포인트의모양
     cex  = 2,   #포인트의 크기
     col  = "darkorange")

res2<-lm(y~x, data=dt3)
abline(res2)

model5 <- lm(y ~ x, dt3)
model5

summary(model5)
anova(model5)




dt6 <- data.frame(x = c(1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978),
                 y = c(25.82,25.35,24.29,24.05,24.89,25.35,25.23,25.06,27.13,27.36,26.65,27.13,27.49,27.08,27.51,27.54,26.21
                 ,18.24,16.50,20.26,20.97,19.43,19.31,20.85,19.54,20.49,21.91,22.51,18.81,19.42,19.10,18.80,18.80,17.57))

dt6
model6 <- lm(y ~ x, dt6)
model6

summary(model6)
anova(model6)

     
