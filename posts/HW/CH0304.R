#####################################################
##########  선형회귀분석
##########  
#####################################################
setwd('C:/R-Project/DAT/Regression/')

library(ggplot2)


############ 데이터 불러오기 
dt <- data.frame(x = c(4,8,9,8,8,12,6,10,6,9),
                 y = c(9,20,22,15,17,30,18,25,10,20))
dt

cor(dt$x, dt$y)

plot(y~x, 
     data = dt,
     xlab = "광고료",
     ylab = "총판매액",
     pch  = 16,
     cex  = 2,
     col  = "darkorange")

ggplot(dt, aes(x, y)) +
  geom_point(col='steelblue', lwd=2) +
  # geom_abline(intercept = co[1], slope = co[2], col='darkorange', lwd=1.2) +
  xlab("광고료")+ylab("총판매액")+
  # scale_x_continuous(breaks = seq(1,10))+
  theme_bw() +
  theme(axis.title = element_text(size = 14))



############ 적합
### hat y = hat (E(y|X=x)) = hat beta_0 +hat beta_1 * x

#### H0 : beta0 =0 vs H1 : beta0 != 0
#### H0 : beta1 =0 vs H1 : beta1 != 0

## y = beta0 + beta1*x + epsilon
model1 <- lm(y ~ x, dt)
model1


summary(model1) 

names(model1)

model1$fitted.values  ##hat y
model1$coefficients

anova(model1)  ## 회귀모형의 유의성 검정


a <- summary(model1)
ls(a)

summary(model1)$coef   ## 회귀계수의 유의성 검정

confint(model1, level = 0.95)  ##회귀계수의 신뢰구간
## beta +- t_alpha/2 (n-2) * se(beta)
qt(0.025, 8)
qt(0.975, 8)


## y = beta1*x + epsilon
model2 <- lm(y ~ 0 + x, dt)
summary(model2)

############
plot(y~x, data = dt,
     xlab = "광고료",
     ylab = "총판매액",
     pch  = 20,
     cex  = 2,
     col  = "darkorange")
abline(model1, col='steelblue', lwd=2)
abline(model2, col='green', lwd=2)

co <- coef(model1)

ggplot(dt, aes(x, y)) +
  geom_point(col='steelblue', lwd=1) +
  geom_abline(intercept = co[1], slope = co[2], col='darkorange', lwd=1) +
  xlab("광고료")+ylab("총판매액")+
  theme_bw()+
  theme(axis.title = element_text(size = 16))



######## LSE 구하기

dt1 <- data.frame(
  i = 1:nrow(dt),
  x = dt$x,
  y = dt$y,
  x_barx = dt$x - mean(dt$x),
  y_bary = dt$y - mean(dt$y))

dt1$x_barx2 <- dt1$x_barx^2
dt1$y_bary2 <- dt1$y_bary^2
dt1$xy <-dt1$x_barx * dt1$y_bary

dt1
round(colSums(dt1),3)

### hat beta1 = S_xy / S_xx
##hat beta0 = bar y - hat beta_1 * bar x
beta1 <- as.numeric(colSums(dt1)[8]/colSums(dt1)[6])
beta0 <- mean(dt$y) - beta1 *  mean(dt$x)

cat("hat beta0 = ", beta0)
cat("hat beta1 = ", beta1)



############# 평균반응, 개별 y 추정
## E(Y|x0), y = E(Y|x0) + epsilon
# x0 = 4.5
new_dt <- data.frame(x = 4.5)

# hat y0 = hat beta0 + hat beta1 * 4.5

predict(model1, 
        newdata = new_dt,
        interval = c("confidence"), level = 0.95)

predict(model1, newdata = new_dt, 
        interval = c("prediction"), level = 0.95)


dt_pred <- data.frame(
  x = 1:12,
  predict(model1, 
          newdata=data.frame(x=1:12), 
          interval="confidence", level = 0.95))
dt_pred

dt_pred2 <- as.data.frame(predict(model1, 
                                  newdata=data.frame(x=1:12), 
                                  interval="prediction", level = 0.95))
dt_pred2

names(dt_pred2)[2:3] <- c('plwr', 'pupr')


dt_pred3 <- cbind.data.frame(dt_pred, dt_pred2[,2:3])

barx <- mean(dt$x)
bary <- mean(dt$y)

plot(y~x, data = dt,
     xlab = "광고료",
     ylab = "총판매액",
     pch  = 20,
     cex  = 2,
     col  = "grey",
     ylim = c(min(dt_pred3$plwr), max(dt_pred3$pupr)))
abline(model1, lwd = 5, col = "darkorange")

lines(dt_pred3$x, dt_pred3$lwr, col = "dodgerblue", lwd = 3, lty = 2)
lines(dt_pred3$x, dt_pred3$upr, col = "dodgerblue", lwd = 3, lty = 2)
lines(dt_pred3$x, dt_pred3$plwr, col = "dodgerblue", lwd = 3, lty = 3)
lines(dt_pred3$x, dt_pred3$pupr, col = "dodgerblue", lwd = 3, lty = 3)

abline(h=bary,v=barx, lty=2, lwd=0.2, col='dark grey')

ggplot(dt_pred3, aes(x, fit)) +
  geom_line(col='steelblue', lwd=2) +
  xlab("")+ylab("")+
  scale_x_continuous(breaks = seq(1,10))+
  geom_line(aes(x, lwr), lty=2, lwd=1.5, col='darkorange') +
  geom_line(aes(x, upr), lty=2, lwd=1.5, col='darkorange') +
  geom_line(aes(x, plwr), lty=2, lwd=1.5, col='dodgerblue') +
  geom_line(aes(x, pupr), lty=2, lwd=1.5, col='dodgerblue') +
  geom_vline(xintercept = barx, lty=2, lwd=0.2, col='dark grey')+
  geom_hline(yintercept = bary, lty=2, lwd=0.2, col='dark grey')+
  theme_bw()




##############################

bb <- summary(model1)$sigma * ( 1 + 1/10 +(dt$x - 8)^2/46)
dt$ma95y <- model1$fitted + 2.306*bb
dt$mi95y <- model1$fitted - 2.306*bb

ggplot(dt, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  geom_line(aes(x, mi95y), col = 'darkgrey', lty=2) +
  geom_line(aes(x, ma95y), col = 'darkgrey', lty=2) +
  theme_bw() +
  theme(axis.title = element_blank())


############ 잔차분석 
### epsilon : 선형성, 등분산성, 정규성, 독립성 

dt
dt$yhat <- model1$fitted
# fitted.values(model1)
dt$resid <- model1$residuals
# resid(model1)

par(mfrow=c(1,2))
plot(resid ~ x, dt, pch=16, ylab = 'Residual')
abline(h=0, lty=2, col='grey')
plot(resid ~ yhat, dt, pch=16, ylab = 'Residual')
abline(h=0, lty=2, col='grey')
par(mfrow=c(1,1))

# 독립성검정 : DW test
library(lmtest)
## 
dwtest(model1, alternative = "two.sided")  #H0 : uncorrelated vs H1 : rho != 0
# dwtest(model1, alternative = "greater")  #H0 : uncorrelated vs H1 : rho > 0
# dwtest(model1, alternative = "less")

## 정규분포 (QQ plot)
qqnorm(dt$resid, pch=16)
qqline(dt$resid, col = 2)

ggplot(dt, aes(sample = resid)) + 
  stat_qq() + stat_qq_line() +
  theme_bw()

## 정규분포 검정 
shapiro.test(dt$resid)  ##shapiro-wilk test
#H0 : normal distributed vs H1 : not

## 등분산성 검정 
bptest(model1) #Breusch–Pagan test
# H0 : 등분산 vs H1 : 이분산 






################################################################
################################################################
################################################################
# install.packages('UsingR')
library(UsingR)
data(father.son)

names(father.son)

lm.fit<-lm(sheight~fheight, data=father.son)
summary(lm.fit)


plot(sheight~fheight, 
     data=father.son, 
     pch=16, cex=0.5,
     xlab="father’s height (inches)", 
     ylab="son’s height (inches)")
abline(lm.fit)




################################################################
################################################################
################################################################

amazon<-read.csv("amazon.csv")
plot(High   ~Year  , amazon, pch=16)

lm.fit<-lm(High~Year, data=amazon)
summary(lm.fit)

confint(lm.fit)

par(mfrow=c(1,2))
scatter.smooth(x=1:length(amazon$Year), y=residuals(lm.fit), xlab="Year")
scatter.smooth(x=predict(lm.fit), y=residuals(lm.fit), xlab=expression(hat(y)))

library(lmtest)
dwtest(lm.fit)

