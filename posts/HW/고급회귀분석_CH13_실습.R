#####################################################
##########  가변수
#####################################################

library(ggplot2)

#### Example #############################################
dt <- data.frame(
  y = c(17,26,21,30,22,1,12,19,4,16,
        28,15,11,38,31,21,20,13,30,14),
  x1 = c(151,92,175,31,104,277,210,120,290,238,
         164,272,295,68,85,224,166,305,124,246),
  x2 = factor(rep(c(0,1), each=10))
)

head(dt)

contrasts(factor(dt$x2))

m <- lm(y~x1+x2, dt)
summary(m)


# x2 = factor(rep(c('M','F'), each=10)) 로 입력한 경우 
#y = b0 + b1x1 + b2x2 
# x2 = 0,  F
# x2 = 1,  M
#E(y|M) : b0 + b1x1 + b2 = (b0 + b2) + b1x1
#E(y|F) : b0 + b1x1

# x2 = factor(rep(c(0,1), each=10))로 입력한 경우 
# y = b0 + b1x1 + b2x2 
# x2 = 0,  M
# x2 = 1,  F
#E(y|M) : b0 + b1x1
#E(y|F) : b0 + b1x1+ b2 = = (b0 + b2) + b1x1

ggplot(dt, aes(x1, y, col=x2)) + 
  geom_point() + 
  theme_bw() +
  guides(col=guide_legend(title="성별")) +
  scale_color_manual(labels = c("남자", "여자"), 
                     values = c("darkorange", "steelblue"))

m <- lm(y~x1+x2, dt)
summary(m)


ggplot(dt, aes(x1, y, col=x2)) + 
  geom_point() + 
  theme_bw() + 
  geom_abline(slope = coef(m)[2], intercept = coef(m)[1], col= 'darkorange')+
  geom_abline(slope = coef(m)[2], intercept = coef(m)[1]+coef(m)[3], col= 'steelblue')+
  guides(col=guide_legend(title="성별")) +
  scale_color_manual(labels = c("남자", "여자"), values = c("darkorange", "steelblue"))


########## 교호작용 
m1 <- lm(y~x1*x2, dt)
summary(m1)

## y = b0 + b1x1 + b2x2 + b3x1x2
## M : x2=0 => E(y|M) = b0+b1x1
## F : x2=1 => E(y|F) = b0 + b1x1 + b2 + b3x1 
##                    = (b0+b2) + (b1+b3)x1

ggplot(dt, aes(x1, y, col=x2)) + 
  geom_point() + 
  theme_bw() + 
  geom_abline(slope = coef(m1)[2], intercept = coef(m1)[1], col= 'darkorange')+
  geom_abline(slope = coef(m1)[2]+coef(m1)[4], intercept = coef(m1)[1]+coef(m1)[3], col= 'steelblue')+
  guides(col=guide_legend(title="성별")) +
  scale_color_manual(labels = c("남자", "여자"), values = c("darkorange", "steelblue"))


#######################################################
#######################################################
library(ISLR)

head(Carseats)
dim(Carseats)
# • Sales : 판매량 (단위: 1,000)
# • Price : 각 지점에서의 카시트 가격
# • ShelveLoc : 진열대의 등급 (Bad, Medium, Good)
# • Urban :도시 여부 (Yes, No)
# • US: 미국 여부 (Yes, No)

fit <- lm(fit<-lm(Sales~Price+ShelveLoc+US, 
                  data=Carseats))
summary(fit)          

contrasts(Carseats$ShelveLoc)

## y= b0 + b1x1 + b2x2 + b3x3 + b4x4 + b5x5

library(car)
##H0 : beta2 = beta3 = 0
## b2 = 0, b3=0
C<-rbind(c(0,0,1,0,0,0),
         c(0,0,0,1,0,0))
linearHypothesis(fit, C)






############ 구간별 회귀분석 
dt <- data.frame(
  y = c(377,249,355,475,139,452,440,257),
  x1 = c(480,720,570,300,800,400,340,650)
)
dt$x2 = sapply(dt$x1, function(x) max(0, x-500))


m <- lm(y ~ x1+x2, dt)
summary(m)


dt2 <- rbind(dt[,2:3], c(500,0))
dt2$y <- predict(m, newdata = dt2)


# this is the predicted line of multiple linear regression
ggplot(data = dt, aes(x = x1, y = y)) + 
  geom_point(color='steelblue') +
  geom_line(color='darkorange',data = dt2, aes(x=x1, y=y))+
  geom_vline(xintercept = 500, lty=2, col='red')+
  theme_bw()