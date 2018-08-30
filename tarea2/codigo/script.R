library("caret")
library(mvtnorm)



dat =read.table("wine.data", sep=",")
names(dat) = c("Class","Alcohol", "Malic acid", "Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")
##library(ElemStatLearn)





##Aplicar clasificador


N = 178
CLS= 3

particiones <- createDataPartition (y=dat$Class, p=0.8, list=FALSE)

datE = dat[particiones,] #ENTRENAMIENTO....
datP = dat[-particiones,]##PRUEBA

y = datE[,1]
datE = datE[,-1]
medias = by(datE, y, colMeans);
vars = by(datE, y, var)
ns = by(datE, y, function(x) dim(x)[1])
Sp = matrix(0, dim(datE)[2], dim(datE)[2])
for(i in 1:CLS){Sp = Sp+(ns[[i]]-1)*vars[[1]]}
Sp = Sp/(sum(ns)-CLS)

#datP = ENTRENAMIENTO...
Np = dim(datP)[1]	
yP = datP[,1]
datP[,-1]
qsP = matrix(0, Np, CLS)
for(j in 1:CLS){qsP[,j] = dmvnorm(datP, mean=medias[[j]], sigma=Sp)}
mmP = apply(qsP, 1, max)
clasepreP = rep(0,Np)
for(i in 1:Np){
   aa = which( qsP[i,] == mmP[i])
   nn = length(aa)
   if(nn==1){ clasepreP[i] = aa}
   else{ clasepreP[i] = aa[sample(1:nn, size=1)]}
}
aa = table(yP, clasepreP)

errLDAP = 100*(1-sum(diag(aa))/sum(aa))
print(errLDAP)
