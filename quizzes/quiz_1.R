#write code returning the following sequence of values in order (-0.6,-1.6,-2.6,-3.6,-4.6)
-(seq(0.6,4.6,1))

#calculate the square roots of the even integers from 0 to 20
sqrt(seq(0,20,2))

#initialize age and months.breastfed, then print value of exponentiation of age to power of months.breastfed
age=2
months.breastfed=3
print(age^months.breastfed)

#calculate total number of overweight (but not obese) individuals using BMI vector
#overweight is 25 to <30; obese is >=30
BMI = c(18.9, 35.4, 25, 17.5, 23.1, 29.9, 26, 21.3)
length(BMI)-sum((BMI)>=30)-sum((BMI)<25)

#compute proportion of overweight or obese patients for variable BMI of any size
sum((BMI)>=25)/length(BMI)

#calculate mean of positive values in A
A=c("1","2","3",0, "1","2","3","-2","-3")
A1=as.integer(A[c(A)>0])
mean(A1)

# initialize and print() a factor variable
status = factor(x=c("inpatient", "inpatient", "outpatient", "outpatient", "outpatient"), levels=c("inpatient", "outpatient", "TBD"))
print(status)

#use prod() to determine whether product of non-NA values in A is larger than sum of non-NA values in A
A=c(0.1,2,3,4,NA)
(prod(A,na.rm=TRUE))>(sum(A,na.rm=TRUE))

#initialize variables x,y,z and perform a calculation
X=2
Y=7
Z=10
(Z-sqrt(9.7))*((X/2.5)^(log(Y-6)-X^3))
#double check math
E1=Z-sqrt(9.7)   #6.885518
E2=X/2.5         #0.8
E3=log(Y-6)-X^3  #-8
E1*(E2^E3)

#define a class from two patient measurements; calculate how many units difference between each patient's systolic and diastolic BPs
P1=list(systolic=140, diastolic=100)
P2=list(systolic=160, diastolic=80)
class(c(P1,P2))
print(c(P1$systolic-P1$diastolic,P2$systolic-P2$diastolic))