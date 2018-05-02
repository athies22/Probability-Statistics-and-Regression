### R code from vignette source 'intro2.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: fig1
###################################################

elas.url = url(
  "http://pages.uoregon.edu/dlevin/DATA/elastic2.txt")
elas = read.table(elas.url,header=T)
plot(distance~stretch, data=elas, pch=19)


###################################################
### code chunk number 2: fig1b
###################################################
elas.lm <- lm(distance~stretch, data=elas)
plot(distance~stretch, data=elas, pch=19)
abline(elas.lm)


###################################################
### code chunk number 3: ls
###################################################
plot(distance~stretch, data=elas, pch=19,cex=0.15,ylim=c(40,300))
abline(elas.lm,col="red")
segments(elas$stretch,fitted.values(elas.lm),elas$stretch,elas$distance,col="red")
b0=-130;b1=6.5
abline(b0,b1,col="blue")
fitb = b0 + b1*elas$stretch
segments(elas$stretch+0.1,fitb,elas$stretch+0.1,elas$distance,col="blue")



###################################################
### code chunk number 4: intro2.Rnw:138-140
###################################################
elas.lm <- lm(distance~stretch, data=elas)
summary(elas.lm)


###################################################
### code chunk number 5: intro2.Rnw:159-161
###################################################
library(xtable)
xtable(summary(elas.lm), digits=2, label="Tab:Elas", caption = "Coefficient and standard errors")


###################################################
### code chunk number 6: intro2.Rnw:167-168
###################################################
Stangle("intro2.Rnw")


