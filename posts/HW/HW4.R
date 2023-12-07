# 완전모형


# 남자인 경우
dt_boy <- data.frame(
  y = c(17,26,21,30,22,1,12,19,4,16),
  x = c(151,92,175,31,104,277,210,120,290,238)
)
m1 <- lm(y~x, dt_boy)
summary(m1)
anova(m1)

one=c(1,1,1,1,1,1,1,1,1,1)
x = c(151,92,175,31,104,277,210,120,290,238)
x_boy=cbind(one,x)

x_boy

xtx = t(x_boy)%*%x_boy
xtxi = solve(xtx)
xtxi

y=c(17,26,21,30,22,1,12,19,4,16)
xty=t(x_boy)%*%y


beta = xtxi%*%xty
beta

t(y)%*%y

mean(y)
16.8*16.8
3568-10*282.24

xbeta=x_boy%*%beta
e=y-xbeta
e

t(e)%*%e
745.6-100.3747


#여자인 경우
dt2<-data.frame(
  
  y_girl=c(28,15,11,38,31,21,20,13,30,14),
  x_girl=c(164,272,295,68,85,224,166,305,124,24)
)
y_girl=c(28,15,11,38,31,21,20,13,30,14)
x_girl=c(164,272,295,68,85,224,166,305,124,24)
one=c(1,1,1,1,1,1,1,1,1,1)
xgirl=cbind(one,x_girl)

xtx=t(xgirl)%*%xgirl
xtx
xinv2=solve(xtx)
xinv2
yty=t(y_girl)%*%y_girl
yty
xty=t(xgirl)%*%y_girl

betagirl=xinv2%*%xty
betagirl


m2<-lm(y_girl~x_girl,dt2)
summary(m2)
anova(m2)
281.8+475.1
100.3747+475.1

# 축소모형


y = c(17,26,21,30,22,1,12,19,4,16,28,15,11,38,31,21,20,13,30,14)
x = c(151,92,175,31,104,277,210,120,290,238,164,272,295,68,85,224,166,305,124,246)


dt3<-data.frame(
  y = c(17,26,21,30,22,1,12,19,4,16,28,15,11,38,31,21,20,13,30,14),
  x = c(151,92,175,31,104,277,210,120,290,238,164,272,295,68,85,224,166,305,124,246))
  


m3<-lm(y~x,dt3)
summary(m3)
anova(m3)
one=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
x=cbind(one,x)
xtx=t(x)%*%x
xtx
xtxi=solve(xtx)
xtxi
xty=t(x)%*%y
xty
beta=xtxi%*%xty
beta
t(y)%*%y

qf(0.05, 2,16,lower.tail=F)
qf(0.95,2,16)


# 문제 2번

x1 = c(151,92,175,31,104,277,210,120,290,238,
       164,272,295,68,85,224,166,305,124,246)
x2 = c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1
)

x=cbind(one,x1,x2)
x
y = c(17,26,21,30,22,1,12,19,4,16,
      28,15,11,38,31,21,20,13,30,14)

m4 <-lm(y~x1+x2)
m4
summary(m4)


t(x)
xtx=t(x)%*%x
xtx
xtxi=solve(xtx)
xtxi
anova(m4)

xtxi*306.78
sqrt(0.002337398)

qt(0.95,3)
qt(0.975,16)
qt(0.975,17)
-0.1009+2.109816*0.04834664



 # 보기 3번.. 적합시켜보기
x1 = c(151,92,175,31,104,277,210,120,290,238,
       164,272,295,68,85,224,166,305,124,246)
x2 = c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1
)
y = c(17,26,21,30,22,1,12,19,4,16,
      28,15,11,38,31,21,20,13,30,14)
x3=c(0,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
x4=c(1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,1,0)
x=cbind(one,x1,x2,x3,x4)
x

m5<-lm(y~x1+x2+x3+x4)
summary(m5)
anova(m5)

xtx=t(x)%*%x
xtx
xtxin=solve(xtx)

xty=t(x)%*%y
xty
beta=xtxin%*%xty
beta
qt(0.975,15)

#교호작용
x1 = c(151,92,175,31,104,277,210,120,290,238,
       164,272,295,68,85,224,166,305,124,246)
x2 = c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1
)
y = c(17,26,21,30,22,1,12,19,4,16,
      28,15,11,38,31,21,20,13,30,14)
x3=c(0,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
x4=c(1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,1,0)

m6<-lm(y~x1+x2+x3+x4+x1*x2+x1*x3+x1*x4+x2*x3+x2*x4)
summary(m6)
anova(m6)

x=cbind(one,x1,x2,x3,x4,x1*x2,x1*x3,x1*x4,x2*x3,x2*x4)
x
txt=t(x)%*%x
solve(txt)
qt(0.95,10)


# 2번문제
dt <- data.frame(
x1=c(1,2,2,3,4,5,5,6,7),
x2=c(0,0,0,0,1,1,1,1,1),
y=c(2.0,3.2,3.4,4.1,5.2,7.0,7.4,9.7,11.5))

plot(x1,y)
m <- lm(y ~ x1+x2, dt)
summary(m)
x1=c(1,2,2,3,4,5,5,6,7)
x2=c(0,0,0,0,1,1,1,1,1)
one=c(1,1,1,1,1,1,1,1,1)

x=cbind(one,x1,x2)
x
y=c(2.0,3.2,3.4,4.1,5.2,7.0,7.4,9.7,11.5)
xtx=t(x)%*%x
xtxi=solve(xtx)
xty=t(x)%*%y
beta=xtxi%*%xty
beta
xtxi
m <- lm(y ~ x1+x2, dt)
summary(m)


anova(m)
qt(0.95,6)
max(4,4)

-1.6854-1.94318*0.4981967

dt <- data.frame(
  x1=c(1,2,2,3,4,5,5,6,7),
  y=c(2.0,3.2,3.4,4.1,5.2,7.0,7.4,9.7,11.5))


dt$x2 = sapply(dt$x1, function(x) max(0, x-4))
dt

m <- lm(y ~ x1+x2, dt)
summary(m)
anova(m)

dt2 <- rbind(dt[,2:3], c(3,0))
dt2$y <- predict(m, newdata = dt2)


# this is the predicted line of multiple linear regression
ggplot(data = dt, aes(x = x1, y = y)) + 
  geom_point(color='steelblue') +
  geom_line(color='darkorange',data = dt2, aes(x=x1, y=y))+
  geom_vline(xintercept = 500, lty=2, col='red')+
  theme_bw()


dt <- data.frame(
  x1=c(1,2,2,3,4,5,5,6,7),
  x2=c(0,0,0,0,0,1,1,2,3),
  y=c(2.0,3.2,3.4,4.1,5.2,7.0,7.4,9.7,11.5))

plot(x1,y)
m <- lm(y ~ x1+x2, dt)
summary(m)
x1=c(1,2,2,3,4,5,5,6,7)
x2=c(0,0,0,0,0,1,1,2,3)
one=c(1,1,1,1,1,1,1,1,1)

x=cbind(one,x1,x2)
x
y=c(2.0,3.2,3.4,4.1,5.2,7.0,7.4,9.7,11.5)
xtx=t(x)%*%x
xtx
xtxi=solve(xtx)
xtxi
xty=t(x)%*%y
xty
beta=xtxi%*%xty
beta
xtxi
m <- lm(y ~ x1+x2, dt)
summary(m)
anova(m)

e=y-x%*%beta
t(e)%*%e

qt(0.9,6)

1.1323-1.4398*0.1485
