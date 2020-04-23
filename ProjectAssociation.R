#Script Created by Kubam Ivo
#Date: 5/4/2019
#Course: Multivariate and Hierrachical Data
#Group: One 

########Sample Size Calculation#####################

#Setting working directory
setwd("D:\\Msc. Biostatistics\\Level One\\Second Semester\\Multivariate and Hierrachical Data\\Project Association")

#Importing Data set
flower <- read.table(file.choose(),header=T,sep = "")
head(flower)
dim(flower)
summary(flower) # This will give a mean value of 10 days for compound 1(water)


#Varying the sample size to get desired p values
#Change the value of N to desired number
#N.B will advice to select everything next to the end and run at once to see the power

#```{r echo = TRUE, eval = TRUE}
# 1. Choose one fixed N 
N <- 200

# 2. Select parameters
control = 10 #(Lamda. Value gotten from the summary statistics)
treated = 12 #(Effect size is 1. so add deta to lamda(10+11))
alpha = 0.05/14  # significance level divided by 14 to correct for the multiple comparism to be done(Bonferroni correction) 

# 3. Simulate huge number of experiments and test
numberSimulation <- 1000
pval <- numeric(numberSimulation) 

set.seed(1234) # set the seed for reproducible resuts
for (i in 1:numberSimulation){
  # we simulate from Poisson distribution
  controlGroup <- rpois(N, lambda = control)
  treatedGroup <- rpois(N, lambda = treated)
  simData <- data.frame(response = c(controlGroup, treatedGroup), treatment = rep(c(0,1), each = N))
  # we use a GLM model for Poisson regression to test effect of treatment
  pval[i] <- summary(glm(response ~ treatment, data = simData, family=poisson()))$coeff["treatment", "Pr(>|z|)"]
}

# 4. Estimate power
sum(pval < 0.05)/numberSimulation

#Creating a dataframe to hold values for the graph
#While fixing delta value at 1, sample_size was varied from 50 to 250 at intervals of 50.Then recording the powers
#Process was repeated for delta values 1.5 and 2
sample_size<-c(50,100,150,200,250,50,100,150,200,250,50,100,150,200,250)
power_values<-c(0.365,0.583,0.763,0.88,0.928,0.624,0.907,0.973,0.993,1,0.841,0.987,1,1,1)
delta_values<-c(1,1,1,1,1,1.5,1.5,1.5,1.5,1.5,2,2,2,2,2)
plot_data<-as.data.frame(cbind(sample_size,power_values,delta_values)) #merging all variables into one data frame


#Plot of sample size vs power for delta equals 1
plot(plot_data$power_values[plot_data$delta_values=="1"],plot_data$sample_size[plot_data$delta_values=="1"]
     ,type = "l",main = "Sample size vs Power at a fixed effect size of 1",xlab = "Power",
     ylab = "Sample_size")
#plot of sample size vs power for different deltas
plot(plot_data$power_values,plot_data$sample_size,type = "n",main = "Sample size vs Power at various effect sizes",xlab = "Power",
     ylab = "Sample_size")
lines(plot_data$power_values[plot_data$delta_values=="1"],
      plot_data$sample_size[plot_data$delta_values=="1"],col="red",lty=1)
lines(plot_data$power_values[plot_data$delta_values=="1.5"],
      plot_data$sample_size[plot_data$delta_values=="1.5"],col="blue",lty=2)
lines(plot_data$power_values[plot_data$delta_values=="2"],
      plot_data$sample_size[plot_data$delta_values=="2"],co="green",lty=3)
legend(0.4,250,legend = c("delta=1","delta=1.5","delta=2"),lty = 1:3,col = c("red","blue","green"))

##############Analysis######################

#Count data

#Importing Data Set
flower_count<-read.table("D:\\Msc. Biostatistics\\Level One\\Second Semester\\Multivariate and Hierrachical Data\\Project Association\\count_data_group1.csv",header = T,sep = ",")

#See sas version for other summary statistics
attach(flower_count)
boxplot(Days~ factor(Compound)factor(Garden),xlim=c(1,15))
?boxplot()


#Gaussian Data
#Importing Data Set
flower_gaussian<-read.table("D:\\Msc. Biostatistics\\Level One\\Second Semester\\Multivariate and Hierrachical Data\\Project Association\\gaussian_data_group1.csv",header = T,sep = ",")
dim(flower_gaussian)
head(flower_gaussian)
#removing flowers with imcomplete data
flower_gaussian<-na.omit(flower_gaussian)

xmatrix <-matrix(,nrow = 0,ncol=21)

for (x in 1:15){
  vector<-c()
  for (i in 2:22){
    filtered<-flower_gaussian[flower_gaussian$Compound==x,]
    vector[i-1] <- mean(filtered[,i])
  }
  xmatrix<-rbind(xmatrix,vector)
}
dim(xmatrix)
head(xmatrix)
timescale<-c(0:20)
timescale

plot(timescale,xmatrix[1,],type = "n",ylim = c(4,16))
#lines(timescale,xmatrix[1,])
for (i in 1:15){
  lines(timescale,xmatrix[i,])
}


