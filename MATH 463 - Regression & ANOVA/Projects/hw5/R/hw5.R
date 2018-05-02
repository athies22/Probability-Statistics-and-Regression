## Problem 1
### Part (a)
### Estimate path coefficients and st.dev of error
n <- 20000
MM <- matrix(c(1, 0.438, 0.538,
             0.438, 1, 0.417,
             0.538, 0.417, 1),
             nrow = 3)
MMinv <- solve(n*(MM))
MY <- matrix(c(0.596, 0.405, 0.541))
hats1 <- MMinv%*%(n*MY)
s2hat1 <- (1 - hats1[1]**2 - hats1[2]**2 - hats1[3]**2 - 2*hats1[1]*hats1[2]*0.438 - 2*hats1[2]*hats1[3]*0.417 - 2*hats1[1]*hats1[3]*0.538)**0.5
### Part (b)
### Estimate SE's of the hats, use Theorem 4.3
SE.hats1 <- ((s2hat1**2)*MMinv)**0.5
SE.ehat <- SE.hats1[1,1]
SE.fhat <- SE.hats1[2,2]
SE.ghat <- SE.hats1[3,3]

## Problem 2
### Part (a)
m <- 26
NN <- matrix(c(1,0.52,
               0.52, 1),
             nrow=2)
NNinv <- solve(m*NN)
NR <- matrix(c(-0.26, -0.42))
hats2 <- NNinv%*%(m*NR)
### Part (b)
s2hat2 <- (1 - hats2[1]**2 - hats2[2]**2 - 2*hats2[1]*hats2[2]*0.52)*(26/23)
### Part (c)
SE.hats2 <- (s2hat2)*NNinv
SE.ahat <- SE.hats2[1,1]**0.5
SE.bhat <- SE.hats2[2,2]**0.5
### Part (d)
Var.ahat <- SE.hats2[1,1]
Var.bhat <- SE.hats2[2,2]
Cov.ahatbhat <- SE.hats2[1,2]
SE.ahatbhat <- (Var.ahat + Var.bhat + 2*Cov.ahatbhat)**0.5

## Problem 3
### No R required

## Problem 4
set.seed(14008)

Sig <- matrix(c(1,0,0, 0,1,0.5, 0,0.5,1), nrow = 3, byrow = TRUE)
SigSD <- eigen(Sig)
SigSR <- SigSD$vectors %*% diag(sqrt(SigSD$values)) %*% t(SigSD$vectors)

R <- 1000
S <- 500

chat <- 1:R
dhat <- 1:R
ehat <- 1:R
y <- 1:S

for (j in 1:R){
    for(i in 1:S){
        xde <- t(SigSR %*% matrix(rnorm(I(S*3)), ncol = S))
    }
    y <- 1 + 2*xde[,1] + xde[,2]
    W <- 1 + 3*xde[,1] + 2*y + xde[,3]
    mod1 <- lm(W ~ xde[,1] + y)
    chat[j] <- summary(mod1)$coefficients[1]
    dhat[j] <- summary(mod1)$coefficients[2]
    ehat[j] <- summary(mod1)$coefficients[3]
}

chatlow <- (mean(chat) - 1) - 1.96*(sd(chat)/sqrt(R))
chatmean <- (mean(chat) - 1)
chathigh <- (mean(chat) - 1) + 1.96*(sd(chat)/sqrt(R))

dhatlow <- (mean(dhat) - 3) - 1.96*(sd(dhat)/sqrt(R))
dhatmean <- (mean(dhat) - 3)
dhathigh <- (mean(dhat) - 3) + 1.96*(sd(dhat)/sqrt(R))

ehatlow <- (mean(ehat) - 2) - 1.96*(sd(ehat)/sqrt(R))
ehatmean <- (mean(ehat) - 2)
ehathigh <- (mean(ehat) - 2) + 1.96*(sd(ehat)/sqrt(R))