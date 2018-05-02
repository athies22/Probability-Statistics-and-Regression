long.1 = lm(Employed~GNP+Population, data=longley)
plot(residuals(long.1)~Year, data=longley,pch=19)

X = model.matrix(long.1)
rhoest <- function(v){
    rc = cor(v[-1],v[-16])
    return(rc)
}
f <- function(rh){
    Ma = diag(16)
    Ma = rh^abs(row(Ma)-col(Ma))
    ea <- eigen(Ma)
    Va <- ea$vectors
    Sa = Va%*%diag(I(sqrt(1/ea$values)))%*%t(Va)
    Sai = Va%*%diag(sqrt(ea$values))%*%t(Va)
    SYa = Sa%*%longley$Employed
    SXa = Sa%*%X
    return(list(lm(SYa~SXa-1),Sai))
}
rhov = 1:11
rhov[1] = rhoest(residuals(long.1))
for(i in 1:10){
    rh = rhov[i]
    newf = f(rhov[i])
    rhov[i+1] = rhoest(newf[[2]]%*%residuals(newf[[1]]))
    }
rhov
