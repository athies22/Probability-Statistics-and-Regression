### R code from vignette source 'fpower.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: fpower.Rnw:101-108
###################################################
library(xtable)
boston = read.table(url("http://pages.uoregon.edu/dlevin/DATA/BostonB.txt"),
                    header=T)
g1 = lm(medv~.,data=boston)
g2 = lm(medv~.-age-indus,data=boston)
xtable(anova(g2,g1),caption="F test on coefficients of age and industry",
       label="Tab:F")


###################################################
### code chunk number 2: fpower.Rnw:152-156
###################################################
ageperp = residuals(lm(age~.-medv-indus,data=boston))
indusperp = residuals(lm(indus~.-medv-age,data=boston))
de = (0.05^2*sum(ageperp^2) + 0.001^2*sum(indusperp^2) 
      + 2*0.001*0.05*sum(ageperp*indusperp))/summary(g1)$sigma^2


###################################################
### code chunk number 3: fpower.Rnw:161-162
###################################################
fstar = qf(0.95,2,492)


###################################################
### code chunk number 4: fpower.Rnw:166-167
###################################################
1 - pf(fstar,2,492,ncp = de)


###################################################
### code chunk number 5: fpower.Rnw:180-181
###################################################
crime = read.table(url("http://pages.uoregon.edu/dlevin/DATA/crime.txt"),header=T)


###################################################
### code chunk number 6: fpower.Rnw:215-216
###################################################
Stangle("fpower.Rnw")


