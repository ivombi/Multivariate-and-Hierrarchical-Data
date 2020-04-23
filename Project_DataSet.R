setwd("D:\\Msc. Biostatistics\\Level One\\Second Semester\\Multivariate and Hierrachical Data\\Project Association")
data<-read.table("count_data_group1.csv", sep=",", header = TRUE)
head(data)
summary(data)
data$Compound<-as.factor(data$Compound)

data$CutTime<-as.factor(data$CutTime)
data$Garden<-as.factor(data$Garden)

data$RoseType<-as.factor(data$RoseType)
data$Rater<-as.factor(data$Rater)

boxplot(data$Days~data$Compound, data=data, ylim=c(0,25))
with(data, plotMeans(Days, Compound, error.bars="se", connect=F))
with(data, plotMeans(Days, Compound,CutTime , error.bars="se", connect=F))
with(data, plotMeans(Days, Compound,RoseType, error.bars="se", connect=F))

dim(data)
require(Rcmdr)

### Logistic regression
# Days follow a poisson distribution
# Days~Compound => log(u) = a+ B2x2 + B3x3 +...+B15x15

model1<-glm(Days~Compound, data=data, family = poisson())
summary(model1)
plot(model1)
result<-c(
exp(2.542),#water
exp(2.542-0.14352),#C2
exp(2.542+ 0.19305),#C3
exp(2.542+0.25000),#C4
exp(2.542+0.06073),#C5
exp(2.542+ 0.12575),#C6
exp(2.542+0.01053),#C7
exp(2.542-0.02729),#C8
exp(2.542+0.05187),#C9
exp(2.542-0.11224),#C10
exp(2.542+0.08547),#C11
exp(2.542+0.01173),#C12
exp(2.542+0.15129),#C13
exp(2.542+0.02598),#C14
exp(2.542+0.28405)#C15
)
plot(1,exp(2.542), xlim=c(0,15), col=1)
for(i in 2:15){
	points(i, result[i], col=1)
}

#IT will be nice to know if you can make a confidence interval for every point and see which one takes 21 as a value
# we can fit a model with just the intercep and check which one works better
model1.1<-glm(Days~1, data=data, family = poisson())
summary(model1.1)

#Check over dipsersion

# Uses test statistices -2(Lo-L1)~chi-sqr distribution
mode1.test=-2*(logLik(model1.1)-logLik(model1))
mode1.test
c(mode1.test,1-pchisq(mode1.test,14))
#we have a p-value close to zero, therefore we reject Ho(Ho: B1=B2=...=B15=0)



# we can add Random Effect model 
library(lme4)
model2<-glmer(Days~Compound+(1|BushID), data=data, 
		family=poisson(link = "log"))
summary(model2)


head(data)
#### model elimination via backward procedure, no interaction is take into account due magnitude
library(lme4)
model2.1<-glmer(Days~Compound+Rater+CutTime+Garden+RoseType+(1|BushID), data=data, family=poisson(link = "log"))
summary(model2.1)
# Eliminate garden
model2.2<-glmer(Days~Rater+CutTime+Compound+RoseType+(1|BushID), data=data, family=poisson(link = "log"))
summary(model2.2)
# Eliminate Rater
model2.3<-glmer(Days~CutTime+Compound+RoseType+(1|BushID), data=data, family=poisson(link = "log"))
summary(model2.3)
#Eliminate CutTime
model2.4<-glmer(Days~Compound+RoseType+(1|BushID), data=data, family=poisson(link = "log"))
summary(model2.4)
# Eliminate RoseType
model2.5<-glmer(Days~CutTime+Compound+(1|BushID), data=data, family=poisson(link = "log"))
summary(model2.5)
# Just compound
model2.5<-glmer(Days~Compound+(1|BushID), data=data, family=poisson(link = "log"))
summary(model2.5)
#Just Rand Intercept
model2.6<-glmer(Days~(1|BushID), data=data, family=poisson(link = "log"))
summary(model2.6)




##Can you create a compound 16 that have as a response variable 21 and see which one of the betas is equal to b16?
## we will have to make a lot of nested models, but it might work

#Working with binary data 
head(data2)
data2<-data
data2$Binary<-NA
data2$Binary<-ifelse(data2$Days>20 ,1,0)

model3<-glm(Binary~Compound, data=data2, binomial(link = "logit"))
summary(model3)
binary.coef<-model3$coefficients
deviance(model3)



vec.prob=NA
vec.prob[1]=exp(binary.coef[1])/(1+exp(binary.coef[1]))
for (i in 2:15){
	vec.prob[i]=exp(binary.coef[1]+binary.coef[i])/(1+exp(binary.coef[1]+binary.coef[i]))
}
vec.prob

V<- vcov(model3)
ASE<-V[1,1]*(2*V[1,15])*V[15,15]
a<-(-1.89417+1.39155)-(1.96*sqrt(ASE*-1))
b<-(-1.89417+1.39155)+(1.96*sqrt(ASE*-1))
exp(a)/(1+exp(a)) #lower limir
exp(b)/(1+exp(b))	#high limit

#What if we have clustering data?

model4<-glmer(Binary~Compound+(1|BushID), 
	family=binomial(link = "logit"), data=data2)
summary(model4)

#We can do GEE 
library(gee)

model5<-gee(Binary~Compound, family=binomial(link="logit"), id=BushID, 
		corstr="independence", data=data2)
aa<-summary(model5)
aa$coefficients

model6<-gee(Binary~Compound, family=binomial(link="logit"), id=BushID, 
		corstr="exchangeable", data=data2)
bb<-summary(model6)
bb$coefficients

#Beta binomial
library(aod)
model7<-betabin(Binary~Compound,~1,data=data4)
summary(model7) 

#Taking more covariates into account and doing model selection
model8.1<-glmer(Binary~(1|BushID)+Rater+CutTime+Compound+Garden+RoseType,family=binomial(link = "logit"), data=data2)
summary(model8.1)

#Taking garden out
model8.2<-glmer(Binary~(1|BushID)+Rater+CutTime+Compound+RoseType,family=binomial(link = "logit"), data=data2)
summary(model8.2)

#Taking rater out
model8.3<-glmer(Binary~(1|BushID)+CutTime+Compound+RoseType,family=binomial(link = "logit"), data=data2)
summary(model8.3)

#Taking ROseType out
model8.4<-glmer(Binary~(1|BushID)+Rater+CutTime+Compound,family=binomial(link = "logit"), data=data2)
summary(model8.4)

#Taking Cut time out
model8.5<-glmer(Binary~(1|BushID)+Rater+Compound,family=binomial(link = "logit"), data=data2)
summary(model8.5)

#Taking Rater out
model8.5<-glmer(Binary~(1|BushID)+Compound,family=binomial(link = "logit"), data=data2)
summary(model8.5)

#Taking just cluster
model8.5<-glmer(Binary~(1|BushID),family=binomial(link = "logit"), data=data2)
summary(model8.5)

##1######################################################################################
##### For ht gaussian data 
########################################################################################

setwd("C:\\Users\\Usuario\\Documents\\Project\\DataSet")
G.data<-read.table("gaussian_data_group1.csv", sep=",", header = TRUE)
head(G.data)
summary(G.data)
dim(G.data)
G.data$Compound<-as.factor(G.data$Compound)
G.data$Garden<-as.factor(G.data$Garden)
G.data$Type<-as.factor(G.data$Type)

G.data[1,2:22]

par(mfrow=c(3,5))
par(mfrow=c(1,3))
#C1
plot(seq(0,20), G.data[1,2:22], type="l", main="compound 1")
for(i in 2:12){
	lines(seq(0,20),G.data[i,2:22])
}


#lines(seq(0,20), colMeans(G.data[1,2:22]), col=2, lwd=2)

#C2
plot(seq(0,20), G.data[13,2:22], type="l", main="compound 2")
for(i in 14:24){
	lines(seq(0,20),G.data[i,2:22] )
}

#C3
plot(seq(0,20), G.data[25,2:22], type="l", main="compound 3")
for(i in 26:36){
	lines(seq(0,20),G.data[i,2:22] )
}

#C4
plot(seq(0,20), G.data[37,2:22], type="l", main="compound 4")
for(i in 38:48){
	lines(seq(0,20),G.data[i,2:22] )
}

#C5
plot(seq(0,20), G.data[49,2:22], type="l", main="compound 5")
for(i in 50:60){
	lines(seq(0,20),G.data[i,2:22] )
}

#C6
plot(seq(0,20), G.data[61,2:22], type="l", main="compound 6")
for(i in 62:72){
	lines(seq(0,20),G.data[i,2:22] )
}

#C7
plot(seq(0,20), G.data[73,2:22], type="l", main="compound 7")
for(i in 74:84){
	lines(seq(0,20),G.data[i,2:22] )
}

#C8
plot(seq(0,20), G.data[85,2:22], type="l", main="compound 8")
for(i in 86:96){
	lines(seq(0,20),G.data[i,2:22] )
}


#C9
plot(seq(0,20), G.data[97,2:22], type="l", main="compound 9")
for(i in 98:108){
	lines(seq(0,20),G.data[i,2:22] )
}

#C10
plot(seq(0,20), G.data[109,2:22], type="l", main="compound 10")
for(i in 110:120){
	lines(seq(0,20),G.data[i,2:22] )
}

#C11
plot(seq(0,20), G.data[121,2:22], type="l", main="compound 11")
for(i in 122:132){
	lines(seq(0,20),G.data[i,2:22] )
}

#C12
plot(seq(0,20), G.data[133,2:22], type="l", main="compound 12")
for(i in 134:144){
	lines(seq(0,20),G.data[i,2:22] )
}

#C13
plot(seq(0,20), G.data[145,2:22], type="l", main="compound 13")
for(i in 146:156){
	lines(seq(0,20),G.data[i,2:22] )
}

#C14
plot(seq(0,20), G.data[157,2:22], type="l", main="compound 14")
for(i in 158:168){
	lines(seq(0,20),G.data[i,2:22] )
}

#C15
plot(seq(0,20), G.data[169,2:22], type="l", main="compound 15")
for(i in 170:180){
	lines(seq(0,20),G.data[i,2:22] )
}

head(G.data)
summary(G.data)
G.data[G.data$Subplot==1,2:22]


par(mfrow=c(2,3))
values<-G.data[G.data$Subplot==18,2:22]
plot(seq(0,20), values[1,], type="l", main="Subplot 18")
for(i in 2:nrow(values)){
	lines(seq(0,20),values[i,])
}
lines(seq(0,20),colMeans(values), col=2)

values<-G.data[G.data$Subplot==4,2:22]
lines(seq(0,20), values[1,], type="l", lty=2)
for(i in 2:nrow(values)){
	lines(seq(0,20),values[i,], lty=2)
}
lines(seq(0,20),colMeans(values), col=2, lty=2)

## just another plot
values<-G.data[G.data$Subplot==18,2:22]
plot(seq(0,20),colMeans(values), col=1, type="l")
values<-G.data[G.data$Subplot==4,2:22]
lines(seq(0,20),colMeans(values), col=2, type="l")
values<-G.data[G.data$Subplot==6,2:22]
lines(seq(0,20),colMeans(values), col=3, type="l")
values<-G.data[G.data$Subplot==15,2:22]
lines(seq(0,20),colMeans(values), col=4, type="l")

## 
# i like more this one!!!

values<-G.data[G.data$Subplot==1,2:22]
plot(seq(0,20),colMeans(values,na.rm = TRUE), col=1, type="l", ylim=c(0,15))
for(i in 2:18){
	values<-G.data[G.data$Subplot==i,2:22]
	lines(seq(0,20),colMeans(values,na.rm = TRUE), col=i, type="l")

}



setwd("C:\\Users\\Usuario\\Documents\\Project\\DataSet")
G2.data<-read.table("Fix_Gaussian.csv", sep=";", header = TRUE, dec=",")
head(G2.data)
summary(G2.data)
dim(G2.data)
G2.data$Compound<-as.factor(G2.data$Compound)
G2.data$Garden<-as.factor(G2.data$Garden)
G2.data$Type<-as.factor(G2.data$Type)

# the flower is the cluster, because we have several measurement from each flower






