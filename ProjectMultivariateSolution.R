setwd("D:\\Msc. Biostatistics\\Level One\\Second Semester\\Multivariate and Hierrachical Data\\Multivariate Project")
getwd()
#install.packages("ldr")
#library(ldr)
load("CanadianWeather.rda")
da<-CanadianWeather[[1]]
da<-da[,,"Precipitation.mm"] # precipitation data
str(data.frame(da))
MetaData<-data.frame(city=colnames(da), region=CanadianWeather$region, province=CanadianWeather$province, coord=CanadianWeather$coordinates)
head(MetaData)

days<-1:365
days<-(days-min(days))/(diff(range(days))) # rescaling to [0,1]
phi<-poly(days,degree=15)


#constructing the x matrix
xmatrix <-matrix(,nrow = 0,ncol=16)
for (i in 1:35) {
  cityfunction <-lm(da[,i]~phi)
  xmatrix <- rbind(xmatrix,cityfunction$coefficients)
}

dim(xmatrix)
head(xmatrix)

n <-nrow(xmatrix)
h<-diag(n)-1/n*matrix(1,ncol=n,nrow = n) # creating the centering matrix
xmatrix[,]<-h%*%as.matrix(xmatrix) #Centring the xmatrix
xmatrix.svd<-svd(xmatrix) #SVD 
xmatrix.svd

uk <-xmatrix.svd$u[,1:2]
dk <-diag(xmatrix.svd$d[1:2])
zk <- uk%*%dk

dim(zk)
rownames(zk)<-colnames(da)
plot(zk,type = "n",xlab = "z1",ylab = "z2")
text(zk,rownames(zk),cex=1.3)



