#CompleteData
CompleteData=read.table("C:/Users/narv1/Documents/MHIA/HCIP 5376/Quizzes/CompleteData.csv",sep=",",header=TRUE)

#SMARTRISK function
SMARTRISK = function(Data)
{
  Linear.predictor = -0.0850*Data$Age + 0.00105*(Data$Age^2) + 
    0.156*Data$Male + 0.262*Data$Smoker  + 0.00429*Data$Systolic + 
    0.223*Data$Diabetic + 0.140*Data$CAD + 0.406*Data$CVD + 0.558*Data$AAA + 
    0.283*Data$PAD + 0.0229*Data$Yrs - 0.426*Data$HDL + 0.0959*Data$TCHOL - 
    0.0532*Data$eGFR + 0.000306*(Data$eGFR^2) + 0.139*Data$loghsCRP
  return((1-0.81066^exp(Linear.predictor + 2.099))*100)
}

#Q1 - avg risk for w/ & w/o diabetes
#w/ diabetes
mean(SMARTRISK(CompleteData[CompleteData$Diabetic==1,]))
#w/o diabetes
mean(SMARTRISK(CompleteData[CompleteData$Diabetic==0,]))

#Q2 - males who smoke vs. non-smoking females
#males who smoke
mean(SMARTRISK(CompleteData[CompleteData$Male==1 & CompleteData$Smoker==1,]))
#nonsmoking females
mean(SMARTRISK(CompleteData[CompleteData$Male==0 & CompleteData$Smoker==0,]))

#Q3 - Patient 8 systolic reductions
DataQ3=CompleteData
systolic.reduction = seq(0,-60,-1) # unit reductions in systolic bp
for (systolic.reduction in systolic.reduction)
{
  Implied.systolic = (CompleteData[8,"Systolic"])+(systolic.reduction)
  DataQ3[8,"Systolic"]=Implied.systolic
  print(c(Implied.systolic,SMARTRISK(DataQ3[8,])))
  #Return a vector with The SMARTRISK reduction (values going from 0 to 60) 
}

#Q4 - average ages, 80% risk
age.risk80up=as.numeric()
age.riskless80=as.numeric()
for (i in 1:2000)
{
  risk=SMARTRISK(CompleteData[i,])
  if(risk>=80)
  {
    age.risk80up=c(age.risk80up,CompleteData[i,"Age"])
  }else{
    if(risk<80)
    {
      age.riskless80=c(age.riskless80,CompleteData[i,"Age"])
    }
  }
}
# average age of patients with at least an 80% 10-year risk
mean(age.risk80up)
# average age of those with less than an 80% 10-year risk
mean(age.riskless80)

#Q5 - mean risks by race/ethnicity
r.ethn=unique(CompleteData$Race.Ethn)
risk.r.ethn=c()
for (i in 1:4)
{
  mean.risk=mean(SMARTRISK(CompleteData[CompleteData$Race.Ethn==r.ethn[i],]))
  risk.r.ethn=c(risk.r.ethn,r.ethn[i],mean.risk)
}
print(risk.r.ethn)

#Q6 - first Hispanic patient w/ tchol >6
Data.Hispanic=CompleteData[CompleteData$Race.Ethn=="Hispanic",]
which.row=1
while(Data.Hispanic[which.row,"TCHOL"]<=6)
{
  which.row=which.row+1
}
print(Data.Hispanic[which.row,"TheID"])

#Q7 - IDs of all Hispanic patients w/ TCHOL >6
Data.Hispanic=CompleteData[CompleteData$Race.Ethn=="Hispanic",]
Hispanic.TCHOL.above6=as.numeric()
for (i in 1:dim(Data.Hispanic)[1])
{
  if(Data.Hispanic[i,"TCHOL"]>6)
  {
    Hispanic.TCHOL.above6=c(Hispanic.TCHOL.above6,Data.Hispanic[i,"TheID"])
  }else{
    "TCHOL<=6"
  }
}
print(Hispanic.TCHOL.above6)

#Q8 - output.csv of smoking status and risk value
smoking.status=c()
risk.10yr=c()
for(i in 1:2000)
{
  smoking.status=c(smoking.status,CompleteData[i,"Smoker"])
  risk.10yr=c(risk.10yr,SMARTRISK(CompleteData[i,]))
}
output=data.frame(smoking.status,risk.10yr)
write.table(output,"C:/Users/narv1/Documents/MHIA/HCIP 5376/Quizzes/output.csv",sep=",",col.names=T,row.names=F)

#Q9 - num patients w/ TCHOL >6 OR (65+ yrs AND diabetic)
print(sum((CompleteData[,"TCHOL"]>6)|((CompleteData[,"Age"]>65)&(CompleteData[,"Diabetic"]==1))))

#Q10 - corrected risk for Yrs==0.5
risk.difference=as.numeric()
for (i in 1:2000)
{
  if(CompleteData[i,"Yrs"]==0.5)
  {
    original.risk=SMARTRISK(CompleteData[i,])
    EditedData=CompleteData
    EditedData[i,"Yrs"]=0
    new.risk=SMARTRISK(EditedData[i,])
    risk.difference=c(risk.difference,(original.risk-new.risk))
    }else{
    "Yrs!=0.5"
  }
}
max(risk.difference)
min(risk.difference)
median(risk.difference)