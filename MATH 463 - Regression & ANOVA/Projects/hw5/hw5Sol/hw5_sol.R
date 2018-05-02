### R code from vignette source 'hw5_sol.Rnw'

###################################################
### code chunk number 1: hw5_sol.Rnw:166-169
###################################################
  M = matrix(c(1,0.438,0.438,1),nrow=2,byrow=T)
  XW = matrix(c(0.538,0.417),ncol=1)
  cdhat = round(solve(M)%*%XW,3)


###################################################
### code chunk number 2: hw5_sol.Rnw:209-212
###################################################
  M = matrix(c(1,0.438,0.538,0.438,1,0.417,0.538,0.417,1),ncol=3,byrow=T)
  MY = matrix(c(0.596,0.405,0.541),ncol=1)
  efghat = round(solve(M)%*%MY,3)


###################################################
### code chunk number 3: hw5_sol.Rnw:288-291
###################################################
  M = matrix(c(1,0.52,0.52,1),nrow=2,byrow=T)
  MY = matrix(c(-0.26,-0.42),ncol=1)
  betahat = round(solve(M)%*%MY,3)


###################################################
### code chunk number 4: hw5_sol.Rnw:325-332
###################################################
pr = 1 - (betahat[1,1]^2+betahat[2,1]^2+2*betahat[1,1]*betahat[2,1]*0.52)
sigmahat = sqrt(pr*(26-1)/(26-3))
SigmaB = round((sigmahat^2)/(26-1)*solve(M),4)
Mi = solve(M)
sediff = sqrt(SigmaB[1,1]+SigmaB[2,2]-2*SigmaB[1,2])
cil = betahat[1]-betahat[2]-2*sediff
ciu = betahat[1]-betahat[2]+2*sediff


###################################################
### code chunk number 5: hw5_sol.Rnw:526-553
###################################################
library(xtable)
set.seed(101)
rho = 0.5
Sig = matrix(c(1,0,0,0,1,rho,0,rho,1),nrow=3,byrow=3)
SigSD = eigen(Sig)
SigSR = SigSD$vectors%*%diag(sqrt(SigSD$values))%*%t(SigSD$vectors)
n = 500
xde = t(SigSR%*%matrix(rnorm(I(3*n)),nrow=3))
abcde = c(1,2,1,3,2)
y = 1 + 2*xde[,1]+xde[,2]
w = 1 + 3*xde[,1]+2*y + xde[,3]
fit = lm(w~xde[,1]+y)
cde = coef(fit)

R = 1000
cde = matrix(rep(1,I(3*R)),nrow=R)
for(i in 1:R){
  xde = t(SigSR%*%matrix(rnorm(I(3*n)),nrow=3))
  y = 1 + 2*xde[,1]+xde[,2]
  w = 1 + 3*xde[,1]+2*y + xde[,3]
  fit = lm(w~xde[,1]+y)
  cde[i,] = coef(fit)  
}
ests = cbind(I(apply(cde,2,mean)-c(1,3,2)),apply(cde,2,sd)/sqrt(R))
colnames(ests) = c("mean of sim","s.e.")
rownames(ests) = c("bias of a-hat","bias of b-hat","bias of c-hat")
xtable(ests,digits = 5, caption="Mean of simulated OLS estimates", label="Tab:SimE")


###################################################
### code chunk number 6: hw5_sol.Rnw:568-569
###################################################
Stangle("hw5_sol.Rnw")


