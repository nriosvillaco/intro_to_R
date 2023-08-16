#install.packages(c("lattice","tidyverse")
library(lattice)
library(tidyverse)
library(here)
library(mice)
library(readxl)

#CompleteData
completedata_filepath <- here("data", "CompleteData.csv")
CompleteData=read.table(completedata_filepath, sep=",", header=TRUE)

#Q1 - histogram of systolic bp
hist(CompleteData$Systolic,xlab="Systolic blood pressure (mmHG)",ylab="Observed frequency",main="")

#Q2 - 100 mmHG vertical line
abline(v=100)

#Q3 - ggplot
df=data.frame(CompleteData$Systolic)
ggplot(df, aes(CompleteData$Systolic)) + geom_histogram(aes(y=..density..), colour="black",fill="white") + geom_density(alpha=.2, fill="blue")

#Q4 - boxplot of systolic differences by race/ethn
boxplot(Systolic~Race.Ethn,data=CompleteData,xlab="Race/Ethnicity",ylab="Systolic blood pressure (mmHG)")

#Q5 - association between smoking status and 10-yr risk
risk.greater20=as.numeric()
for (i in 1:2000)
{
  risk=SMARTRISK(CompleteData[i,])
  if(risk>20)
  {
    risk.greater20=c(risk.greater20,TRUE)
  }else{
    risk.greater20=c(risk.greater20,FALSE)
  }
}
chisq.test(CompleteData$Smoker,risk.greater20)

#Q6 - 95% CI for age, CAD==1
DataCAD1=CompleteData[CompleteData$CAD==1,]
meanData=mean(DataCAD1$Age)
t.test(DataCAD1$Age,mu=meanData)

#Q7 - modified code, visualize implications to a 45 year old patient's 10yr risk for different systolic bp values
SMARTRISK = function(Data)
{
  Linear.predictor = -0.0850*Data$Age + 0.00105*(Data$Age^2) + 0.156*Data$Male + 0.262*Data$Smoker  + 0.00429*Data$Systolic + 0.223*Data$Diabetic + 0.140*Data$CAD + 0.406*Data$CVD + 0.558*Data$AAA + 0.283*Data$PAD + 0.0229*Data$Yrs - 0.426*Data$HDL + 0.0959*Data$TCHOL - 0.0532*Data$eGFR + 0.000306*(Data$eGFR^2) + 0.139*Data$loghsCRP
  return((1-0.81066^exp(Linear.predictor + 2.099))*100)
}
Patients = data.frame(Age=45, Male=0, Smoker=1, Systolic=100:180, Diabetic=1, CAD=0, CVD=0, AAA=0, PAD=1, Yrs=12, HDL=2, TCHOL=4, eGFR=100, loghsCRP=1)
Risk.systolic100to180 = SMARTRISK(Patients)
plot(100:180,Risk.systolic100to180,xlab="Systolic blood pressure (mmHG)",ylab="10-year risk of recurrent CVD event (%)",main="Example Individual")

#Q8 - heatmap optimization
Age = 40:80
eGFR = seq(50,100,1)
f = function(Age,eGFR)
{
  Patient = data.frame(Age,Male=0, Smoker=1, Systolic=160, Diabetic=1, CAD=0, CVD=0, AAA=0, PAD=1, Yrs=12, HDL=2, TCHOL=4, eGFR, loghsCRP=1)
  SMARTRISK(Patient)
}
Risk = outer(Age,eGFR,f) # Calculates the function SMARTRISK over each possible Age & eGFR
levelplot(Risk,row.values=Age,column.values=eGFR,xlab="Age",ylab="eGFR",col.regions=rev(heat.colors(100)),colorkey=list(col=rev(heat.colors(100))))


#Q9 - scatterplot, systolic bp and 10yr risk among diabetic non-Hispanic Black males over 50 yrs old
Q9Data=CompleteData[CompleteData$Diabetic==1 & CompleteData$Race.Ethn=="Non-H Black" & CompleteData$Male==1 & CompleteData$Age>50,]
plot(Q9Data$Systolic,SMARTRISK(Q9Data), xlab="Systolic blood pressure (mmHG)",ylab="10-year risk of recurrent CVD event (%)",main="Diabetic Non-Hispanic Black Males Over 50 Years Old")


#Q10 - modified imputation, explore association between smoking status (outcome) and yrs since 1st diagnosis (Yrs)
incompletedata_filepath <- here("data", "IncompleteData.xlsx")
IncompleteData=read_excel(incompletedata_filepath)
set.seed(3)
imputed = mice(IncompleteData,m=5) 
fit = with(data=imputed,lm(Smoker~Yrs))
summary(pool(fit))
