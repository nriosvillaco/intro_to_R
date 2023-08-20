#install.packages(c("vtable","moments"))
library(here)
library(vtable)
library(moments)
library(mice)
library(readxl)

#load CompleteData
completedata_filepath <- here("data", "CompleteData.csv")
CompleteData=read.table(completedata_filepath,sep=",",header=TRUE)

#load SMARTRISK function
SMARTRISK = function(Data)
{
  Linear.predictor = -0.0850*Data$Age + 0.00105*(Data$Age^2) +
    0.156*Data$Male + 0.262*Data$Smoker  + 0.00429*Data$Systolic +
    0.223*Data$Diabetic + 0.140*Data$CAD + 0.406*Data$CVD + 0.558*Data$AAA +
    0.283*Data$PAD + 0.0229*Data$Yrs - 0.426*Data$HDL + 0.0959*Data$TCHOL -
    0.0532*Data$eGFR + 0.000306*(Data$eGFR^2) + 0.139*Data$loghsCRP
  return((1-0.81066^exp(Linear.predictor + 2.099))*100)
}

#produce sumtable grouped by Race.Ethn
sumtable(CompleteData,group='Race.Ethn',group.test=FALSE)

#create histogram of 10-yr risk of recurrent CVD event for Non-H White; superimpose density plot of 10-yr risk for Non-H Black
par(mfrow=c(1,1))
hist(SMARTRISK(CompleteData[CompleteData$Race.Ethn=="Non-H White",]),xlab="10-yr risk of recurrent CVD event for Non-Hispanic White participants",main="",prob=TRUE)
lines(density(SMARTRISK(CompleteData[CompleteData$Race.Ethn=="Non-H Black",])))

#perform linear regression demonstrating association between risk of CVD event and systolic bp
summary(lm(SMARTRISK(CompleteData)~CompleteData$Systolic))

#create heatmap to visualize potential association between age and 10-yr risk
Risk = table(cut(SMARTRISK(CompleteData),seq(0,100,10)),cut(CompleteData$Age,seq(40,80,5)))
heatmap(Risk,Rowv=NA,Colv=NA,xlab="Age (years)",ylab="Risk (%)")

#use 2-sample t-test to compare avg risk for Hisp. individuals vs. Non-H White ind.
Hisp.risk=SMARTRISK(CompleteData[CompleteData$Race.Ethn=="Hispanic",])
NonH.White.risk=SMARTRISK(CompleteData[CompleteData$Race.Ethn=="Non-H White",])
t.test(Hisp.risk,NonH.White.risk)

#provide odds ratio of risk above 50% for smokers vs non-smokers
risk.above50 = 1*(SMARTRISK(CompleteData)>=50)
results = glm(risk.above50~CompleteData$Smoker,family=binomial(link="logit")); results
odds.ratio(results)

#use optimization to find systolic bp at which risk equals 50%
f=function(Systolic)
{
  Patient = data.frame(Age=70, Male=1, Smoker=1, Systolic, Diabetic=1, CAD=0, CVD=1, AAA=0, PAD=0, Yrs=20, HDL=2, TCHOL=5, eGFR=100, loghsCRP=3)
  Risk=SMARTRISK(Patient)
  (Risk-50)^2
}
optim(0,f,method="L-BFGS-B")

#calculate skewness of 10-year risk among all patients
skewness(SMARTRISK(CompleteData))

#provide visualization of benefits of reducing systolic bp
systolic.values=seq(101,200,1)
Patient1=CompleteData[1,]
RxSBP=c()
for (i in 101:200)
{
  Patient1$Systolic=i
  RxSBP=c(RxSBP,SMARTRISK(Patient1))
}
plot(systolic.values,RxSBP,xlab="Systolic Blood Pressure (mmHG)",ylab="10-year Risk of Recurrent CVD Event")

#perform multiple imputation to explore association between HDL (outcome) and yrs since 1st diagnosis (Yrs)
incompletedata_filepath <- here("data", "IncompleteData.xlsx")
IncompleteData=read_excel(incompletedata_filepath)
set.seed(2)
imputed = mice(IncompleteData,m=5) 
fit = with(data=imputed,lm(HDL~Yrs))
summary(pool(fit))
