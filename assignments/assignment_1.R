#Q1 - create SMARTRISK function
SMARTRISK=function(Data)
{
  linear.predictor=function(Data)
  {
    linear.predictor=(-0.0850*Data[1])+(0.00105*(Data[1]^2))+(0.156*Data[2])+
      (0.262*Data[3])+(0.00429*Data[4])+(0.223*Data[5])+(0.140*Data[6])+
      (0.406*Data[7])+(0.558*Data[8])+(0.283*Data[9])+(0.0229*Data[10])-
      (0.426*Data[11])+(0.0959*Data[12])-(0.0532*Data[13])+
      (0.000306*((Data[13])^2))+(0.139*Data[14])
    return(linear.predictor) #return the SMARTRISK linear predictor as an output of the function
  }
SMARTRISK=(1-(0.81066^(exp(linear.predictor(Data)+2.099))))*100
return(SMARTRISK)
}

#Q2 - initialize PATIENT1 data frame
age=65
male=0
smoker=1
systolic=160
diabetic=1
CAD=0
CVD=0
AAA=0
PAD=1
yrs=12
HDL=2
TCHOL=4
eGFR=100
loghsCRP=1
PATIENT1=data.frame(age,male,smoker,systolic,diabetic,CAD,CVD,AAA,PAD,yrs,HDL,
                    TCHOL,eGFR,loghsCRP)

#Q3 - produce 10-yr SMART CVD event risk
SMARTRISK(PATIENT1)

#Q4 - PATIENT2 data frame
PATIENT2=PATIENT1
PATIENT2$male=1
SMARTRISK(PATIENT2)

#Q5 - difference in risk
SMARTRISK(PATIENT2)-SMARTRISK(PATIENT1)

#Q6 - logSMART
logSMART=function(Data)
{  
  SMARTRISK=function(Data)
  {
    linear.predictor=function(Data)
    {
      linear.predictor=(-0.0850*Data[1])+(0.00105*(Data[1]^2))+(0.156*Data[2])+
        (0.262*Data[3])+(0.00429*Data[4])+(0.223*Data[5])+(0.140*Data[6])+
        (0.406*Data[7])+(0.558*Data[8])+(0.283*Data[9])+(0.0229*Data[10])-
        (0.426*Data[11])+(0.0959*Data[12])-(0.0532*Data[13])+
        (0.000306*((Data[13])^2))+(0.139*Data[14])
      return(linear.predictor) #return the SMARTRISK linear predictor as an output of the function
  }
  SMARTRISK=(1-(0.81066^(exp(linear.predictor(Data)+2.099))))*100
    return(SMARTRISK)
  }
logSMART=log(SMARTRISK(Data))
return(logSMART)
}
logSMART(PATIENT1)

#Q7 - ratioSMART
ratioSMART=function(Data)  
{  
  logSMART=function(Data)
  {  
    SMARTRISK=function(Data)
    {
      linear.predictor=function(Data)
      {
        linear.predictor=(-0.0850*Data[1])+(0.00105*(Data[1]^2))+(0.156*Data[2])+
          (0.262*Data[3])+(0.00429*Data[4])+(0.223*Data[5])+(0.140*Data[6])+
          (0.406*Data[7])+(0.558*Data[8])+(0.283*Data[9])+(0.0229*Data[10])-
          (0.426*Data[11])+(0.0959*Data[12])-(0.0532*Data[13])+
          (0.000306*((Data[13])^2))+(0.139*Data[14])
        return(linear.predictor) #return the SMARTRISK linear predictor as an output of the function
      }
    SMARTRISK=(1-(0.81066^(exp(linear.predictor(Data)+2.099))))*100
    return(SMARTRISK)
    }
  logSMART=log(SMARTRISK(Data))
  return(logSMART)
  }
ratioSMART=logSMART(Data)/SMARTRISK(Data)
return(ratioSMART)
}
ratioSMART(PATIENT2)

#Q8 - SMARTRISK2
SMARTRISK2=function(Parameter,Data)
{
  linear.predictor=function(Parameter,Data)
  {
    linear.predictor=(Parameter[1]*Data[1])+(Parameter[2]*(Data[1]^2))+
      (Parameter[3]*Data[2])+(Parameter[4]*Data[3])+(Parameter[5]*Data[4])+
      (Parameter[6]*Data[5])+(Parameter[7]*Data[6])+(Parameter[8]*Data[7])+
      (Parameter[9]*Data[8])+(Parameter[10]*Data[9])+(Parameter[11]*Data[10])-
      (Parameter[12]*Data[11])+(Parameter[13]*Data[12])-(Parameter[14]*Data[13])+
      (Parameter[15]*((Data[13])^2))+(Parameter[16]*Data[14])
    return(linear.predictor) #return the SMARTRISK2 linear predictor as an output of the function
  }
  SMARTRISK2=(1-(0.81066^(exp(linear.predictor(Parameter,Data)+2.099))))*100
  return(SMARTRISK2)
}

#Q9 - initialize Parameters data frame, diff
age=-0.0850
age2=0.00105
male=0.156
smoker=0.262
systolic=0.00429
diabetic=0.223
CAD=0.140
CVD=0.406
AAA=0.558
PAD=0.283
yrs=0.0229
HDL=0.426
TCHOL=0.0959
eGFR=0.0532
eGFR2=0.000306
loghsCRP=0.139
Parameter=data.frame(age,age2,male,smoker,systolic,diabetic,CAD,CVD,AAA,PAD,yrs,HDL,
                    TCHOL,eGFR,eGFR2,loghsCRP)
SMARTRISK(PATIENT1)-SMARTRISK2(Parameter,PATIENT1)

#Q10 - geometric.mean
install.packages("psych")
library(psych)
x=as.numeric(c(SMARTRISK(PATIENT1),SMARTRISK(PATIENT2)))
geometric.mean(x)
