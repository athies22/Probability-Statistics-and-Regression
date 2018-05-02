################################ 1 ################################
rm(list=ls())

ozone <- read.table("http://pages.uoregon.edu/dlevin/DATA/ozo.txt", header=T)
ozone$HO <- as.numeric(ozone$HO)
mod1 <- glm(HO~temp+humidity, data = ozone, family = binomial(link = "probit"))
summary(mod1)
cov <- summary(mod1)$cov.unscaled
x0 <- mod1$coefficients[1]
x1 <- mod1$coefficients[2]
x2 <- mod1$coefficients[3]
hum <- 80
temp <- 95

#### 1a ####
XB <- x0 + x1*temp + x2*hum
y <- pnorm(XB, mean = 0, sd = 1)

#### 1b ####
a <- cbind(1,95,80)
at <- t(a)
sig <- (a %*% cov %*% at)**(0.5)
CI1low <- XB - 1.96*sig[1]
CI1high <- XB + 1.96*sig[1]

#### 1c ####
CI2low <- pnorm(CI1low)
CI2high <- pnorm(CI1high)


################################ 2 ################################
rm(list=ls())
di = read.table("http://pages.uoregon.edu/dlevin/DATA/DI.txt",header=T,row.names=1,sep="\t")
plot(NDIR~Taxes, data=di, pch=19, cex=0.2)
text(di$Taxes,di$NDIR,row.names(di), cex=0.4)
gmod = lm(NDIR~Taxes, data=di)
abline(gmod)

plot(NDIR~Taxes, data = di, col = Region, pch=19, cex=0.5)
f1 <- lm(NDIR~Taxes+Region+Taxes*Region, data = di)
betahat <- f1$coef
betahat.base <- f1$coef[1:2]
abline(betahat.base)
for(i in 0:3){
    abline(betahat.base+betahat[c(2+i,5+i)], col=1+i)
}
legend(3000,100,pch = 19, col = 1:4, legend = levels(di$Region))
plot(Taxes~Region, data = di)

################################ 3 ################################
rm(list=ls())


################################ 4 ################################
rm(list=ls())
library(expm)
set.seed(935867)

##### Simulation for n=10, rho=0.8, tau=1 #####
## OLS ##
n <- 10
sig <- 1
tau <- 1
rho <- .8

z1 <- rnorm(n)
z2 <- rnorm(n)
SM <- matrix(c(sig^2, rho*sig*tau, rho*sig*tau, tau^2), byrow=T, nrow=2)
SME <- eigen(SM)
SMSR <- SME$vectors %*% diag(sqrt(SME$values)) %*% t(SME$vectors)
W <- matrix(rnorm(n*2), nrow=2)
de <- t(SMSR %*% W)

d <- de[,1]
e <- rnorm(n)

X.OLS <- z1 + 2*z2 + d
Y.OLS <- 0.5*(X.OLS) + e
beta.hat <- solve(t(X.OLS) %*% X.OLS) %*% t(X.OLS) %*% Y.OLS

# bias
bias.OLS <- beta.hat - 0.5
# variance
var.OLS <- 1^2*(solve(t(X.OLS)%*%X.OLS))
# MSE
MSE.bh1 <- sqrt(var.OLS + bias.OLS**2)

## IVLS ##
Z.IVLS <- cbind(z1,z2)
Zt.IVLS <- sqrtm(solve(t(Z.IVLS) %*% Z.IVLS))
X.IVLS <- Zt.IVLS %*% t(Z.IVLS) %*% X.OLS
Y.IVLS <- Zt.IVLS %*% t(Z.IVLS) %*% Y.OLS
beta.tilde <- solve(t(X.IVLS) %*% X.IVLS) %*% t(X.IVLS) %*% Y.IVLS
# bias
bias.IVLS <- beta.tilde - 0.5
# variance
var.IVLS <- 1^2*(solve(t(X.IVLS)%*%X.IVLS))
# MSE
MSE.bt1 <- sqrt(var.IVLS + bias.IVLS**2)

##### Simulation for n=10, rho=0.3, tau=1 #####
## OLS ##
n <- 10
sig <- 1
tau <- 1
rho <- .3

z1 <- rnorm(n)
z2 <- rnorm(n)
SM <- matrix(c(sig^2, rho*sig*tau, rho*sig*tau, tau^2), byrow=T, nrow=2)
SME <- eigen(SM)
SMSR <- SME$vectors %*% diag(sqrt(SME$values)) %*% t(SME$vectors)
W <- matrix(rnorm(n*2), nrow=2)
de <- t(SMSR %*% W)

d <- de[,1]
e <- rnorm(n)

X.OLS <- z1 + 2*z2 + d
Y.OLS <- 0.5*(X.OLS) + e
beta.hat <- solve(t(X.OLS) %*% X.OLS) %*% t(X.OLS) %*% Y.OLS

# bias
bias.OLS <- beta.hat - 0.5
# variance
var.OLS <- 1^2*(solve(t(X.OLS)%*%X.OLS))
# MSE
MSE.bh2 <- sqrt(var.OLS + bias.OLS**2)

## IVLS ##
Z.IVLS <- cbind(z1,z2)
Zt.IVLS <- sqrtm(solve(t(Z.IVLS) %*% Z.IVLS))
X.IVLS <- Zt.IVLS %*% t(Z.IVLS) %*% X.OLS
Y.IVLS <- Zt.IVLS %*% t(Z.IVLS) %*% Y.OLS
beta.tilde <- solve(t(X.IVLS) %*% X.IVLS) %*% t(X.IVLS) %*% Y.IVLS
# bias
bias.IVLS <- beta.tilde - 0.5
# variance
var.IVLS <- 1^2*(solve(t(X.IVLS)%*%X.IVLS))
# MSE
MSE.bt2 <- sqrt(var.IVLS + bias.IVLS**2)

##### Simulation for n=10, rho=0, tau=1 #####
##### Simulation for n=10000, rho=0.8, tau=1 #####
##### Simulation for n=10000, rho=0.3, tau=1 #####
##### Simulation for n=10000, rho=0, tau=1 #####

##### Simulation for n=10, rho=0.8, tau=50 #####
##### Simulation for n=10, rho=0.3, tau=50 #####
##### Simulation for n=10, rho=0, tau=50 #####
##### Simulation for n=10000, rho=0.8, tau=50 #####
##### Simulation for n=10000, rho=0.3, tau=50 #####
##### Simulation for n=10000, rho=0, tau=50 #####


################################ 5 ################################
rm(list=ls())


################################ 6 ################################
rm(list=ls())

V1 <- c(1,1,1,1,1,1,1,1)
V2 <- c(1,1,1,1,-1,-1,-1,-1)
V3 <- c(1,1,-1,-1,1,1,-1,-1)
V4 <- c(1,-1,1,-1,1,-1,1,-1)
X <- data.frame(cbind(V1,V2,V3,V4))

V3perp <- residuals(lm(V3~.-V4, data = X))
V4perp <- residuals(lm(V4~.-V3, data = X))
d1.num <- ((0.1^2)*sum(V3perp^2) + (0.1^2)*sum(V4perp^2) + (2*0.1*0.1)*sum(V3perp*V4perp))

W1 <- c(-1.70, -0.09, -1.03, -0.49, -0.42, 0.45, 0.33, 0.31)
W2 <- c(-1.45, -0.01, -1.27, 0.39, -2.25, 0.66, 0.33, -0.78)
W3 <- c(-0.55, 0.02, -1.47, -1.26, -0.93, 0.15, 0.92, 0.72)
W4 <- c(-0.85, 1.32, 0.24, -0.57, 0.02, 1.41, -0.36, 0.34)
Z <- data.frame(cbind(W1,W2,W3,W4))

W3perp <- residuals(lm(W3~.-W4, data = Z))
W4perp <- residuals(lm(W4~.-W3, data = Z))
d2.num <- ((0.1^2)*sum(W3perp^2) + (0.1^2)*sum(W4perp^2) + (2*0.1*0.1)*sum(W3perp*W4perp))

fstar <- qf(0.95, 2, 4)

powah <- function(crit,del,sig){1-pf(crit,2,4,I(del/sig**2))}

# trial and error
sig1 <- 0.0648601
powah(fstar, d1.num, sig1)

sig2 <- 0.03552299
powah(fstar,d2.num,sig2)