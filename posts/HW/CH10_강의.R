#####################################################
##########  회귀진단
#####################################################


library(lmtest)

################################# 데이터 입력

dt <- data.frame(x = c(15,26,10,9,15,20,18,11,
                       8,20,7,9,10,11,11,10,12,42,17,11,10),
                 y = c(95,71,83,91,102,87,93,100,
                       104,94,113,96,83,84,102,100,
                       105,57,121,86,100))


######## 산점도 
plot(y~x, dt,pch  = 20,cex  = 2,col  = "darkorange")


######## 회귀적합 
model_reg <- lm(y~x, dt)
summary(model_reg)

plot(y~x, dt,pch  = 20,cex  = 2,col  = "darkorange")
abline(model_reg, col='steelblue', lwd=2)


######## 잔차 
residual <- model_reg$residuals  ## e_i = y_i - hat(y_i)
## resid(model_reg)

## 내적으로 표준화된 잔차
s_residual <- rstandard(model_reg)

# # 또는
# s_xx <- sum((dt$x-mean(dt$x))^2)  #S_xx
# h_ii <- 1/21 + (dt$x- mean(dt$x))^2/s_xx
# ## h_ii <- influence(model_reg)$hat
# hat_sigma <-  summary(model_reg)$sigma   #hat sigma
# s_residual <- residual/(hat_sigma*sqrt(1-h_ii)) ## 내적


## 외적으로 스튜던트화된 잔차
s_residual_i <- rstudent(model_reg) ## 외적으로 스튜던트화 잔차

# # 또는
# hat_sigma_i <- sqrt(((21-1-1)*hat_sigma^2 - residual^2/(1-h_ii) )/(21-1-2))
# ## hat_sigma_i <- influence(model_reg)$sigma
# s_residual_i <-  residual/(hat_sigma_i*sqrt(1-h_ii)) ## 외적

## 잔차그림 

par(mfrow = c(2, 2))

plot(fitted(model_reg), residual, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "Residuals", 
     main = "residual plot")
abline(h=0, lty=2)

plot(fitted(model_reg), s_residual, # 표준화된 잔차
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals", 
     ylim=c(min(-3, min(s_residual)), 
            max(3,max(s_residual))),
     main = "standardized residual plot")
abline(h=c(-2,0,2), lty=2)

plot(fitted(model_reg), s_residual_i, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals_(i)", 
     ylim=c(min(-3, min(s_residual_i)), 
            max(3,max(s_residual_i))),
     main = "studentized residual plot")
abline(h=c(-3,-2,0,2,3), lty=2)

plot(fitted(model_reg), s_residual_i, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals_(i)", 
     ylim=c(min(-3, min(s_residual_i)), 
            max(3,max(s_residual_i))),
     main = "studentized residual plot")
abline(h=c(-qt(0.975,21-2),0,qt(0.975,21-2)), lty=2)
text (fitted(model_reg)[which(abs(s_residual_i)>qt(0.975,21-2))],
      s_residual_i[which(abs(s_residual_i)>qt(0.975,21-2))], 
      which(abs(s_residual_i)>qt(0.975,21-2)),adj = c(0,1))

## 이상치 검정
qt(0.975,21-2) #기각역 
s_residual_i
s_residual_i[which(abs(s_residual_i)>qt(0.975,21-2))]



## 정규성 검정
par(mfrow=c(1,2))
hist(resid(model_reg),
     xlab   = "Residuals",
     main   = "Histogram of Residuals",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)

qqnorm(resid(model_reg), 
       main = "Normal Q-Q Plot", 
       col = "darkgrey",
       pch=16)
qqline(resid(model_reg), col = "dodgerblue", lwd = 2)

graphics.off()

## 독립성 검정
lmtest::dwtest(model_reg)

######## 영향점

par(mfrow=c(1,1))
plot(y~x, dt,pch  = 20,cex  = 2,col  = "darkorange")
abline(model_reg, col='steelblue', lwd=2)

# influence(model_reg)
influence.measures(model_reg)

hatvalues(model_reg)
dffits(model_reg)  
cooks.distance(model_reg)
covratio(model_reg)
  
summary(influence.measures(model_reg))

## 기각역
p <- 1
n <- 21
2*(p+1)/n #hat (h_ii)
2*sqrt((p+1)/(n-p-1)) #Dffits
qf(0.5, p+1, n-p-1) #Cook d


par(mfrow=c(2,2))
plot(y~x, dt,pch  = 20,
     cex  = 2,col  = "darkorange",
     main = "전체 데이터")
abline(model_reg, col='steelblue', lwd=2)
text (dt[18:19,],c('18', '19'),adj = c(0,0))


## 18제거 전후 
plot(y~x, dt,pch  = 20,
     cex  = 2,col  = "darkorange",
     main = "18번 제거")
abline(model_reg, col='steelblue', lwd=2)
abline(lm(y~x, dt[-18,]), col='red', lwd=2)
legend('topright', legend=c("full", "del(18)"),
       col=c('steelblue', 'red'), lty=1, lwd=2)
# high leverage and high influence, not outlier


## 19제거 전후 
plot(y~x, dt,pch  = 20,
     cex  = 2,col  = "darkorange",
     main = "19번 제거")
abline(model_reg, col='steelblue', lwd=2)
abline(lm(y~x, dt[-19,]), col='red', lwd=2)
legend('topright', legend=c("full", "del(19)"),
       col=c('steelblue', 'red'), lty=1, lwd=2)
# not leverage and high influence, outlier


## 18, 19제거 전후 
plot(y~x, dt,pch  = 20,
     cex  = 2,col  = "darkorange",
     main = "18,19번 제거")
abline(model_reg, col='steelblue', lwd=2)
abline(lm(y~x, dt[-c(18,19),]), col='red', lwd=2)
legend('topright', legend=c("full", "del(18,19)"),
       col=c('steelblue', 'red'), lty=1, lwd=2)


## 회귀진단 그림 
par(mfrow = c(2, 2))
plot(model_reg, pch=16)



###################################################
#### Hitters

library(ISLR)
hitters <- na.omit(Hitters)
dim(hitters)
head(hitters)


reg_model <- lm(Salary ~ AtBat + Hits + HmRun, hitters)
summary(reg_model)

vcov(reg_model)

pairs(hitters[,c(1,2,3,19)])

## 잔차그림 
residual <- resid(reg_model)
stad.res <- rstandard(reg_model)
stu.res <- rstudent(reg_model)

par(mfrow = c(2, 2))
plot(fitted(reg_model), residual, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "Residuals", 
     main = "residual plot")
abline(h=0, lty=2)

plot(fitted(reg_model), stad.res, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals", 
     ylim=c(min(-3, min(stad.res)), 
            max(3,max(stad.res))),
     main = "standardized residual plot")
abline(h=c(-2,0,2), lty=2)

plot(fitted(reg_model), stu.res, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals_(i)", 
     ylim=c(min(-3, min(stu.res)), 
            max(3,max(stu.res))),
     main = "studentized residual plot")
abline(h=c(-2,0,2), lty=2)

plot(fitted(reg_model), stu.res, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals_(i)", 
     ylim=c(min(-3, min(stu.res)), 
            max(3,max(stu.res))),
     main = "studentized residual plot")
abline(h=c(-qt(0.975,nrow(hitters)-4),0,qt(0.975,nrow(hitters)-4)), lty=2)
text (fitted(reg_model)[which(abs(stu.res)>qt(0.975,nrow(hitters)-4))],
      stu.res[which(abs(stu.res)>qt(0.975,nrow(hitters)-4))], 
      which(abs(stu.res)>qt(0.975,nrow(hitters)-4)),adj = c(0,1))

stu.res[abs(stu.res)>qt(0.975,nrow(hitters)-4)]
which(abs(stu.res)>qt(0.975,nrow(hitters)-4))

influence(reg_model)
influence.measures(reg_model)

summary(influence.measures(reg_model))

## 기각역
p <- 3
n <- nrow(hitters)
2*(p+1)/n #hat (h_ii)
3*sqrt((p+1)/(n-p-1)) #Dffits
2*sqrt((p+1)/(n)) #Dffits
qf(0.5, p+1, n-p-1) #Cook d


##########################################################
#### 변수 변환
##########################################################

hitters$log_Salary <- log(hitters$Salary)

reg_model_2 <- lm(log_Salary ~ AtBat + Hits + HmRun, hitters)
summary(reg_model_2)


## 잔차그림 
residual <- resid(reg_model_2)
stad.res <- rstandard(reg_model_2)
stu.res <- rstudent(reg_model_2)

par(mfrow = c(2, 2))
plot(fitted(reg_model_2), residual, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "Residuals", 
     main = "residual plot")
abline(h=0, lty=2)

plot(fitted(reg_model_2), stad.res, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals", 
     ylim=c(min(-3, min(stad.res)), 
            max(3,max(stad.res))),
     main = "standardized residual plot")
abline(h=c(-2,0,2), lty=2)

plot(fitted(reg_model_2), stu.res, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals_(i)", 
     ylim=c(min(-3, min(stu.res)), 
            max(3,max(stu.res))),
     main = "studentized residual plot")
abline(h=c(-2,0,2), lty=2)

plot(fitted(reg_model_2), stu.res, 
     pch=20,cex  = 2,col  = "darkorange",
     xlab = "Fitted", ylab = "S_Residuals_(i)", 
     ylim=c(min(-3, min(stu.res)), 
            max(3,max(stu.res))),
     main = "studentized residual plot")
abline(h=c(-qt(0.975,nrow(hitters)-4),0,qt(0.975,nrow(hitters)-4)), lty=2)
text (fitted(reg_model_2)[which(abs(stu.res)>qt(0.975,nrow(hitters)-4))],
      stu.res[which(abs(stu.res)>qt(0.975,nrow(hitters)-4))], 
      which(abs(stu.res)>qt(0.975,nrow(hitters)-4)),adj = c(0,1))

stu.res[abs(stu.res)>qt(0.975,nrow(hitters)-4)]
which(abs(stu.res)>qt(0.975,nrow(hitters)-4))

influence(reg_model_2)
influence.measures(reg_model_2)

summary(influence.measures(reg_model_2))
summary(influence.measures(reg_model))


plot(reg_model, pch=16)
plot(reg_model_2, pch=16)


