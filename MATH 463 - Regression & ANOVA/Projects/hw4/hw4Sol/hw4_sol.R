### R code from vignette source 'hw4_sol.Rnw'

###################################################
### code chunk number 1: hw4_sol.Rnw:328-335
###################################################
library(xtable)
x2= c(3,-1,4,6,3,3); x3 = c(7,3,2,0,3,3); x4 = c(8,4,9,-5,4,4)
y = c(4,36,44,12,16,8)
z = x2+x3
f = lm(y~z)
f2 = lm(y~x2+x3+x4)
xtable(anova(f,f2),caption="F statistic comparing $V_0$ and $V$",label="Tab:F1")


###################################################
### code chunk number 2: hw4_sol.Rnw:376-388
###################################################
library(faraway)
library(xtable)
data(teengamb)
teengamb$sex = as.factor(teengamb$sex)
f = lm(gamble~status+income+verbal+sex, data=teengamb)
xtable(summary(f)$coef, caption="All variables (no interaction)",
       label="Tab:Sum")
g1 = lm(gamble~income+sex, data=teengamb)
xtable(anova(g1,f), caption="F test on verbal and status",
       label="Tab:Fvs")
g2 = lm(gamble~income+sex+sex:income, data=teengamb)
xtable(anova(g1,g2), caption="F test on sex:income", label="Tab:Fis")


###################################################
### code chunk number 3: teeng
###################################################
plot(gamble~income, data=teengamb, pch=19, col=sex)
abline(coef(g2)[1:2])
cf = coef(g2)[1:2]+coef(g2)[3:4]
abline(cf, col="red")
legend(1,150,c("male","female"),pch=19,col=c("black","red"), 
       cex=0.5)


###################################################
### code chunk number 4: hw4_sol.Rnw:437-441
###################################################
delta = as.numeric(as.character(teengamb$sex))
g3 = lm(gamble~I(income*(1-delta))+delta, data=teengamb)
xtable(anova(g3,g2),caption="Test that coefficient of
       income for females is zero", label="Tab:Tcz")


###################################################
### code chunk number 5: hw4_sol.Rnw:496-507
###################################################
library(xtable)
y = c(52, 64, 53, 56, 57, 55, 60, 62, 58, 56, 50)
x = as.factor(c(1,2,3,1,2,3,1,2,3,1,3))
g = lm(y~x)
xtable(anova(g),caption="F test of equal means",label="Tab:Fcorn")
d = (5^2*3*(1-3/11)+25^2*4*(1-4/11)-2*5*25*3*4/11)/400
fcrit = qf(0.95,2,8)
pwr = 1-pf(fcrit,2,8,ncp=d)
pow = function(m){1-pf(fcrit,2,I(3*m-3),I(0.875*m))}
#need m = 20 (trial and error)



###################################################
### code chunk number 6: hw4_sol.Rnw:593-594
###################################################
Stangle("hw4_sol.Rnw")


