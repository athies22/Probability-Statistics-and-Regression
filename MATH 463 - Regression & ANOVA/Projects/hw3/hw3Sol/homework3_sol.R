### R code from vignette source 'homework3_sol.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: homework3_sol.Rnw:80-83
###################################################
smsa = na.omit(
  read.table("http://pages.uoregon.edu/dlevin/DATA/smsa.txt",
                          header=T,sep="\t",row.names=1))


###################################################
### code chunk number 2: homework3_sol.Rnw:132-142
###################################################
smsa = na.omit(read.table("http://pages.uoregon.edu/dlevin/DATA/smsa.txt",
                          header=T,sep="\t",row.names=1))
library(xtable)
smsa$NOxPot <- NULL
fit1 = lm(Mortality~., data=smsa)
fit2 = lm(Mortality~Education+PopDensity+X.NonWhite+X.WC+pop+pop.house+income+HCPot+S02Pot+NOx, data=smsa)
prIn = round(predict(fit1, interval="confidence")["Indianapolis, IN",2:3],2)
xtable(anova(fit2,fit1),label="Tab:Anova",caption="F-test that climate coefficients are zero")
ci2 = round(confint(fit2)[11,],3)
ci1 = round(confint(fit1)[15,],3)


###################################################
### code chunk number 3: homework3_sol.Rnw:203-214
###################################################
lm1 = lm(Mortality~., data=smsa)
sig = round(summary(lm1)$sigma,3)
eduperp = residuals(lm(Education~.-NOx-Mortality,data=smsa))
NOxperp = residuals(lm(NOx~.-Education-Mortality,data=smsa))
nseduperp = round(sum(eduperp^2),3)
nsNOxperp = round(sum(NOxperp^2),3)
ipeduNOx = round(sum(NOxperp*eduperp),3)
desig2 = 100*nseduperp+nsNOxperp-20*ipeduNOx
de = round(desig2/sig^2,3)
fc = round(qf(0.95,2,44),3)
pwr = round(I(1 - pf(fc,2,44,de)),3)


###################################################
### code chunk number 4: homework3_sol.Rnw:607-608
###################################################
Stangle("homework3_sol.Rnw")


