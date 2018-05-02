### R code from vignette source 'sim.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: sim.Rnw:74-76
###################################################
set.seed(101)
R = 100


###################################################
### code chunk number 2: tstatgen
###################################################
tstat = 1:R
x1 = rnorm(10,0,3)
x2 = rnorm(10,0,5)
for (i in 1:100){
  ep = rnorm(10,5)
  y = 3 + 0*x1 + 5*x2 + ep
  tstat[i] = summary(lm(y~x1+x2))$coef[2,3]
}



###################################################
### code chunk number 3: hist
###################################################
hist(tstat,prob=T,nclass=12)
x = seq(-4,4,by=0.02)
y = dt(x,df=7)
lines(x,y)


###################################################
### code chunk number 4: nonnormal
###################################################
x1 = rnorm(10,0,3)
x2 = rnorm(10,0,5)

f = function(R,x1,x2){
  tval = 1:R
  for(i in 1:R){
    y = 3 + 5*x2 + rexp(10,rate=1)
    tval[i]=summary(lm(y~x1+x2))$coef[2,3]
  }
  tval
}

hist(f(250,x1,x2),prob=T)


###################################################
### code chunk number 5: sim.Rnw:132-133
###################################################
Stangle("sim.Rnw")


