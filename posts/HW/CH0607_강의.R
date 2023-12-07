#####################################################
##########  선형회귀분석
##########  CH0607
#####################################################

#########################
library(MASS)

data(Boston)
head(Boston)
#보스턴 집값 데이터 이 데이터는 보스턴 근교 지역의 집값 및 다른 정보를 포함한다.
#MASS 패키지를 설치하면 데이터를 로딩할 수 있다.


# B보스턴 근교 506개 지역에 대한 범죄율 (crim)등 14개의 변수로 구성 
# • crim : 범죄율
# • zn: 25,000평방비트 기준 거지주 비율
# • indus: 비소매업종 점유 구역 비율
# • chas: 찰스강 인접 여부 (1=인접, 0=비인접)
# • nox: 일산화질소 농도 (천만개 당)
# • rm: 거주지의 평균 방 갯수 ***
# • age: 1940년 이전에 건축된 주택의 비율
# • dis: 보스턴 5대 사업지구와의 거리
# • rad: 고속도로 진입용이성 정도
# • tax: 재산세율 (10,000달러 당)
# • ptratio: 학생 대 교사 비율
# • black: 1000(B − 0.63)2, B: 아프리카계 미국인 비율
# • lstat : 저소득층 비율 ****
# • medv: 주택가격의 중앙값 (단위:1,000달러 당)


pairs(Boston[,which(names(Boston) %in% c('medv', 'rm', 'lstat'))], pch=16, col='darkorange')

fit_Boston<-lm(medv~rm+lstat, data=Boston)
summary(fit_Boston)

dt <- Boston[,which(names(Boston) %in% c('medv', 'rm', 'lstat'))]
head(dt)
fit_Boston<-lm(medv~., data=dt)

## hat y = -1.3583 + 5.0948*rm - 0.6424*lstat

anova(fit_Boston)
vcov(fit_Boston)  ##var(hat beta) = (X^TX)^-1 \sigma^2


confint(fit_Boston, level = 0.95)

coef(fit_Boston) + qt(0.975, 503) * summary(fit_Boston)$coef[,2]
coef(fit_Boston) - qt(0.975, 503) * summary(fit_Boston)$coef[,2]


############# 평균반응, 개별 y 추정
## E(Y|x0), y = E(Y|x0) + epsilon
new_dt <- data.frame(rm=7, lstat=10)

# hat y0 = -1.3583 + 5.0948*7 - 0.6424*10
predict(fit_Boston, newdata = new_dt)

predict(fit_Boston, 
        newdata = new_dt,
        interval = c("confidence"), 
        level = 0.95)  ##평균반응

predict(fit_Boston, newdata = new_dt, 
        interval = c("prediction"), 
        level = 0.95)  ## 개별 y


############ 잔차분석 
### epsilon : 선형성, 등분산성, 정규성, 독립성 

yhat <- fitted(fit_Boston)
res <- resid(fit_Boston)

plot(res ~ yhat,pch=16, ylab = 'Residual')
abline(h=0, lty=2, col='grey')

plot(res ~ dt$rm,pch=16, ylab = 'Residual')
abline(h=0, lty=2, col='grey')

plot(res ~ dt$lstat,pch=16, ylab = 'Residual')
abline(h=0, lty=2, col='grey')



# 독립성검정 : DW test
library(lmtest)
## 
dwtest(fit_Boston, alternative = "two.sided")  #H0 : uncorrelated vs H1 : rho != 0


## 잔차의 QQ plot
qqnorm(res, pch=16)
qqline(res, col = 2)


###################
##
head(dt)
dt$lstat2 <- (dt$lstat)^2
head(dt)
fit_Boston2<-lm(medv~., data=dt)

##
fit_Boston2<-lm(medv~rm+lstat+I(lstat^2), data=Boston)
summary(fit_Boston2)

yhat2 <- fitted.values(fit_Boston2)
res2 <- resid(fit_Boston2)

plot(res2 ~ yhat2,pch=16, ylab = 'Residual')
abline(h=0, lty=2, col='grey')

qqnorm(res2, pch=16)
qqline(res2, col = 2)

dwtest(fit_Boston2, alternative = "two.sided")  #H0 : uncorrelated vs H1 : rho != 0


fit_Boston3 <- lm(medv~rm, data=Boston)
fit_Boston4 <- lm(medv~lstat, data=Boston)

summary(fit_Boston3)
summary(fit_Boston4)






##########################################
##########################################

x1<-c(4,8,9,8,8,12,6,10,6,9)
x2<-c(4,10,8,5,10,15,8,13,5,12)
y<-c(9,20,22,15,17,30,18,25,10,20)
fit<-lm(y~x1+x2)  ##FM
summary(fit)

anova(fit)


# install.packages("car")
library(car)

## H0 : T*beta = c 


#b1-b2=0 => (0,1,-1) *beta 
#H_0 : beta_1 = beta2
linearHypothesis(fit, c(0,1,-1), 0)

#H_0 : beta_1 = 1
linearHypothesis(fit, c(0,1,0), 1)

#H_0 : beta_1 = beta2 + 1
linearHypothesis(fit, c(0,1,-1), 1)


##H_0 : beta_1 = beta2 + 1
#y=b0 + b1x1 + b2x2 + e = b0+x1 + b2(x1+x2)+e
#y-x1 = b0+b2(x1+x2)+e :   RM

y1 <- y-x1
z1 <- x1 + x2

fit2 <- lm(y1~z1)
summary(fit2)
anova(fit2)

anova(fit)  ##FM
anova(fit2)  #RM

# F = {(SSE_RM - SSE_FM)/r} / {SSE_FM/(n-p-1)}
SSE_FM <- anova(fit)$Sum[3]  #SSE_FM
SSE_RM <- anova(fit2)$Sum[2]  #SSE_RM

F0 <- (SSE_RM-SSE_FM)/(SSE_FM/7)
F0

#기각역 F_{0.05}(1,7)
qf(0.95, 1, 7)


