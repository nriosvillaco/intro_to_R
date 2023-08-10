library(here)
completedata_filepath <- here("data", "CompleteData.csv")
data=read.table(completedata_filepath,sep=",",header=TRUE)

#Q1 - sig test for diff in mean value of loghCRP
NonH.Bl=data[data$Race.Ethn=="Non-H Black",]
Bl.100=NonH.Bl[1:100,]
Bl.loghsCRP=Bl.100$loghsCRP
NonH.Wh=data[data$Race.Ethn=="Non-H White",]
Wh.100=NonH.Wh[1:100,]
Wh.loghsCRP=Wh.100$loghsCRP
t.test(Bl.loghsCRP,Wh.loghsCRP)
  #blank 1 - two-sample t-test
  #blank 2 - 0.007142
  #blank 3 - IS

#Q2 - boxplot, individ. w/ diabetes, logHSCRP statified by smoking status
data=read.table(completedata_filepath,sep=",",header=TRUE)
boxplot(loghsCRP~Diabetic, data=data, xlab="Smoking Status (0=no; 1=yes)",main="Log(hsCRP) by Smoking Status Among Individuals with Diabetes")

#Q3 - chi square test
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

#Q4 - (a) extract only smokers AND tchol > 6, (b) save extracted data into csv
data=read.table(completedata_filepath,sep=",",header=TRUE)
data2=data[data$TCHOL>6 & data$Smoker==1,]
length(data2$TheID)
SmokerHighTotalCholesterolData_filepath <- here("data", "SmokerHighTotalCholesterolData.csv")
write.table(data2,SmokerHighTotalCholesterolData_filepath,sep=",", col.names=T, row.names=F)

#Q5 - debug code
index=1
b1=c(rep(1,500),rep(0,550))
for (TRUE in 1:100)
{
  if(testing(b1[index])==7.5){print(c("OK",index))}else{print(c("Not OK",index));break}
  index = index+3
}
print(index);

#Q6 - calculate # observations for which household.size = max value of variable
sum(A$household.size==max(A$household.size))

#Q7 - object type to allocate multiple variables of diff sizes/types
"only tuples and lists"

#Q8 - debug code, provide (a) line number, (b) error(s), (c) corrections needed
library(gplot2)
ggplot(data=A, aes(x=household.size, y=income, fill=household.size)) +
geom_bar(colour="green", fill="blue", size=1, stat="identity") 
xlab("Household Size (# of persons)") + 
yleb("Income (thousands of dollars)") +
ggtitle("Income by Household Size")
  #edited code should be:
library(ggplot2)
ggplot(A, aes(x=household.size, y=income)) +
  geom_bar(aes(colour="green", fill="blue", size=1, stat="identity")) +
  xlab("Household Size (# of persons)") + 
  ylab("Income (thousands of dollars)") +
  ggtitle("Income by Household Size")

#Q9 - explore associations through regression
summary(lm(WL~PA)) #a
summary(lm(MPAT~WL)) #b

#Q10 - select all that are correct
"Encapsulation is the same as polymorphism" = FALSE
"Classes must always contain a patient name" = FALSE
"Inheritance occurs when two variables have the same name" = FALSE
"SetClass is a variable type" = FALSE
"Variable names in R are case sensitive" = TRUE
"Function names in R are not case sensitive" = FALSE

#EC - create function to calculate % of ind. who are diabetic in any dataset
percentD=function(mydata)
{
  perc.diab=((sum(mydata$Diabetic==1))/(length(mydata$Diabetic)))*100
  return(perc.diab)
}
data.males=data[data$Male==1,]
percentD(data.males)

