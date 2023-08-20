library(here)
library(ggplot2)

#load CompleteData
completedata_filepath <- here("data", "CompleteData.csv")
data=read.table(completedata_filepath,sep=",",header=TRUE)

#perform significance test for differences in mean value of loghCRP
NonH.Bl=data[data$Race.Ethn=="Non-H Black",]
Bl.100=NonH.Bl[1:100,]
Bl.loghsCRP=Bl.100$loghsCRP
NonH.Wh=data[data$Race.Ethn=="Non-H White",]
Wh.100=NonH.Wh[1:100,]
Wh.loghsCRP=Wh.100$loghsCRP
t.test(Bl.loghsCRP,Wh.loghsCRP)

#construct boxplot for individuals w/ diabetes in which logHSCRP is stratified by smoking status
data=read.table(completedata_filepath,sep=",",header=TRUE)
boxplot(loghsCRP~Diabetic, data=data, xlab="Smoking Status (0=no; 1=yes)",main="Log(hsCRP) by Smoking Status Among Individuals with Diabetes")

#run chi square test
data=read.table(completedata_filepath,sep=",",header=TRUE)
hsCRPdata=data
  #convert loghsCRP to hsCRP
hsCRPdata$loghsCRP=exp(hsCRPdata$loghsCRP)
  #chi-square test
below2=as.numeric()
for (i in 1:2000)
{
    if(hsCRPdata[i,"loghsCRP"]<2)
  {
    below2=c(below2,TRUE)
  }else{
    below2=c(below2,FALSE)
  }
}
chisq.test(hsCRPdata$Smoker,below2)

#(a) extract only smokers AND tchol > 6, (b) save extracted data into csv
data=read.table(completedata_filepath,sep=",",header=TRUE)
data2=data[data$TCHOL>6 & data$Smoker==1,]
length(data2$TheID)
SmokerHighTotalCholesterolData_filepath <- here("data", "SmokerHighTotalCholesterolData.csv")
write.table(data2,SmokerHighTotalCholesterolData_filepath,sep=",", col.names=T, row.names=F)

#calculate number of observations for which household.size = max value of variable
sum(A$household.size==max(A$household.size))

#explore associations through regression analyses
summary(lm(WL~PA))
summary(lm(MPAT~WL))

#create function to calculate % of individuals who are diabetic in any dataset
percentD=function(mydata)
{
  perc.diab=((sum(mydata$Diabetic==1))/(length(mydata$Diabetic)))*100
  return(perc.diab)
}
data.males=data[data$Male==1,]
percentD(data.males)

