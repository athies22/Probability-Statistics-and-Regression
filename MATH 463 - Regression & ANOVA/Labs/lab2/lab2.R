### R code from vignette source 'lab2.Rnw'

###################################################
### code chunk number 1: lab2.Rnw:114-117
###################################################
Sig = matrix(c(1,0.3,0,0.3,1,0,0,0,1),ncol=3,byrow=T)
SigSD = eigen(Sig)
SigSR = SigSD$vectors%*%diag(sqrt(SigSD$values))%*%t(SigSD$vectors)


###################################################
### code chunk number 2: lab2.Rnw:128-129
###################################################
xwv = t(SigSR%*%matrix(rnorm(I(653*3)),ncol=653))


###################################################
### code chunk number 3: lab2.Rnw:134-139
###################################################
 ystar = .2*xwv[,1]+.5*xwv[,2]+xwv[,3]
 y = as.numeric(ystar>0)
 fit = glm(y~xwv[,1]+xwv[,2]-1,family = binomial(link="probit"))
 ab=coef(fit)
 ab


###################################################
### code chunk number 4: lab2.Rnw:153-163
###################################################
#Sig = matrix(c(1,0.3,0,0.3,1,0.1,0,0.1,1),ncol=3,byrow=T)
ab = matrix(rep(1,2000),nrow=1000)
for(i in 1:1000){
  xwv = t(SigSR%*%matrix(rnorm(1500),ncol=500))
  y = .2*xwv[,1]+.5*xwv[,2]+xwv[,3]
  y = as.numeric(y>0)
  fit = glm(y~xwv[,1]+xwv[,2]-1,family = binomial(link="probit"))
  ab[i,]=coef(fit)
}
mean(ab[,2])-0.5; sd(ab[,2])/sqrt(1000)


###################################################
### code chunk number 5: lab2.Rnw:172-173
###################################################
Stangle("lab2.Rnw")


