#install.packages("Hmisc")
library(Hmisc)
library(here)

#load CompleteData
completedata_filepath <- here("data", "CompleteData.csv")
CompleteData=read.table(completedata_filepath,sep=",",header=TRUE)

#create NAnumber function calculating number of NA's in a vector
NAnumber=function(data)
{
  NAnumber=sum(is.na(data))
  return(NAnumber)
}
#test vector
data=c(0,17)

#use impute() to impute missing values of vector
vector = c(1,2,3,1,NA,2,1,1,1,NA,3,3,2,3,3,4,5,NA)
impute(vector,median)

#create logical statement covering all positive values associated with overpayment and underpayment
evaluatepayment = function(netpayment)
{
  if(netpayment>0){return("overpayment")}else{
    if(netpayment==0){return("exact payment")}else{return("underpayment")}
  }
}
#test
netpayment=-5
evaluatepayment(netpayment)
netpayment=3
evaluatepayment(netpayment)
netpayment=0
evaluatepayment(netpayment)

#use a for loop to calculate ratios of HDL/Implied.LDL for different LDL.scenarios
HDL=200
LDL=200
LDL.scenario=seq(0,-0.75,-0.05)
for (LDL.percent.reductions in LDL.scenario)
{
  Implied.LDL=LDL*(1+LDL.percent.reductions)
  print(HDL/Implied.LDL)
}

#create function for above task without for loops but rather, vectors
HDL=200
LDL=200
LDL.percent.reductions=seq(0,-0.75,-0.05)
LDL.scenario2=function(LDL.percent.reductions)
{
  Implied.LDL=LDL*(1+LDL.percent.reductions)
  LDL.scenario2=(HDL/Implied.LDL)
  print(LDL.scenario2)
}
LDL.scenario2(LDL.percent.reductions)

#calculate standard deviation of the age among diabetic individuals
diabetic.patients=CompleteData[CompleteData$Diabetic==1,]
sd(diabetic.patients$Age)

#calculate number of patients with >=2 conditions among CAD, CVD, AAA, and PAD
num.conditions=as.numeric()
for (i in 1:2000)
{
  num.sum=sum(CompleteData[i,7:10])
  num.conditions=c(num.conditions,num.sum)
}
sum((num.conditions)>=2)

#use a while loop to create a program identifying the first case of over-80 smoker with CAD, CVD, and AAA
smoker80=CompleteData[CompleteData$Smoker==1 & CompleteData$Age>80,]
which.row=1
while(sum(smoker80[which.row,7:9])!=3)
{
  which.row=which.row+1
}
print(smoker80[which.row,1])

#identify systolic bp of 100th male
CompleteData.male=CompleteData[CompleteData$Male==1,]
male.100=c(CompleteData.male[100,])
print(male.100$Systolic)

#calculate mean hsCRP across patients
mean(exp(CompleteData$loghsCRP))