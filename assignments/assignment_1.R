#install.packages("psych")
library(psych)

#create function called SMARTRISK
SMARTRISK = function(Data)
{
  Linear.predictor = -0.0850*Data$Age + 0.00105*(Data$Age^2) +
    0.156*Data$Male + 0.262*Data$Smoker  + 0.00429*Data$Systolic +
    0.223*Data$Diabetic + 0.140*Data$CAD + 0.406*Data$CVD + 0.558*Data$AAA +
    0.283*Data$PAD + 0.0229*Data$Yrs - 0.426*Data$HDL + 0.0959*Data$TCHOL -
    0.0532*Data$eGFR + 0.000306*(Data$eGFR^2) + 0.139*Data$loghsCRP
  return((1-0.81066^exp(Linear.predictor + 2.099))*100)
}

#initialize PATIENT1 data frame
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

#produce 10-yr SMART CVD event risk using SMARTRISK
SMARTRISK(PATIENT1)

#produce 10-yr risk value using PATIENT2=PATIENT1 except is male
PATIENT2=PATIENT1
PATIENT2$male=1
SMARTRISK(PATIENT2)

#calculate difference in risk between two patients
SMARTRISK(PATIENT2)-SMARTRISK(PATIENT1)

#create function called logSMART to calculate log-risk
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

#create function called ratioSMART to calculate ratio of log-risk to risk
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

#creat function SMARTRISK2 with 2 arguments: df of parameters and df of patient characteristics
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

#initialize Parameters data frame, calculate different between SMARTRISK and SMARTRISK2
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

#calculate geometric mean of SMART risk for patients 1 and 2
x=as.numeric(c(SMARTRISK(PATIENT1),SMARTRISK(PATIENT2)))
geometric.mean(x)
