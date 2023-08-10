library(here)
completedata_filepath <- here("data", "CompleteData.csv")

#Q1 - NAnumber
NAnumber=function(data)
{
  NAnumber=sum(is.na(data))
  return(NAnumber)
}

#Example Vector for Q1
data=c(0,17)

#Q2 - impute function
vector = c(1,2,3,1,NA,2,1,1,1,NA,3,3,2,3,3,4,5,NA)
install.packages("Hmisc")
library(Hmisc)
impute(vector,median)

#Q3 - netpayment logical
evaluatepayment = function(netpayment)
{
  if(netpayment>0){return("overpayment")}else{
    if(netpayment==0){return("exact payment")}else{return("underpayment")}
  }
}

evaluatepayment(netpayment)
netpayment=-5
netpayment=3
netpayment=0 #should be "exact payment", fixed above

#Q4 - LDL.scenario
HDL=200
LDL=200
LDL.scenario=seq(0,-0.75,-0.05)
for (LDL.percent.reductions in LDL.scenario)
{
  Implied.LDL=LDL*(1+LDL.percent.reductions)
  print(HDL/Implied.LDL)
}

#Q5 - LDL.scenario2
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

#Q6 - standard deviation of age of individuals
CompleteData=read.table(completedata_filepath,sep=",",header=TRUE)
diabetic.patients=CompleteData[CompleteData$Diabetic==1,]
sd(diabetic.patients$Age)

#Q7 - 2+ conditions among CAD, CVD, AAA, PAD
num.conditions=as.numeric()
for (i in 1:2000)
{
  num.sum=sum(CompleteData[i,7:10])
  num.conditions=c(num.conditions,num.sum)
}
sum((num.conditions)>=2)

#Q8 - while loop
smoker80=CompleteData[CompleteData$Smoker==1 & CompleteData$Age>80,]
which.row=1
while(sum(smoker80[which.row,7:9])!=3)
{
  which.row=which.row+1
}
print(smoker80[which.row,1])

#Q9 - 100th male systolic
CompleteData.male=CompleteData[CompleteData$Male==1,]
male.100=c(CompleteData.male[100,])
print(male.100$Systolic)

#Q10 - hsCRP
mean(exp(CompleteData$loghsCRP))
