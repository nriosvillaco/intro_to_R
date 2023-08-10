print("My first programming line")
print(2); print(3)
#variables
ID="PC24601"
#comparisons
a=2<3
b=2>3
print(a)
print(b)
#vectors
myvector1=c(1,2,3)
print(myvector1)
b = c(1,2,3) #(vector of pre-defined values)
A = 2+2 #(assignment after some mathematical operation)
b = c(1,2,3); A = b #(copy of another vector)
A = sqrt(b) #(some operation using another variable)
A = c(b,4) #(concatenation of values and/or variables)
A = c(sqrt(b),4) #(concatenation of operations and values)
C = c(A,b) # (concatenation of variables)

age=2
months.breastfed=3
print(age^months.breastfed)

sum(c(18.9, 35.4, 25, 17.5, 23.1, 29.9, 26, 21.3)<30)-sum(c(18.9, 35.4, 25, 17.5, 23.1, 29.9, 26, 21.3)>=25)

A=c(0.1,2,3,4,NA)
(prod(A,na.rm=TRUE))>(sum(A,na.rm=TRUE))

#Question 9
X=2
Y=7
Z=10
(Z-sqrt(9.7))*((X/2.5)^(log(Y-6)-X^3))
E1=Z-sqrt(9.7)   #6.885518
E2=X/2.5         #0.8
E3=log(Y-6)-X^3  #-8
E1*(E2^E3)

status = factor(x=c("inpatient", "inpatient", "outpatient", "outpatient", "outpatient"), levels=c("inpatient", "outpatient", "TBD"))
print(status)

Patient1= list(name="LaToya Jones",  DOB="09/01/2000", Insurance="BCBS",  LDL=82, Visits=c("01/22/2021", "04/07/2022"), AmountsPaid = c(50,  250, 1400))
class(Patient1) = "PatientData"
print(Patient1)

# question 6
A=c("1","2","3",0, "1","2","3","-2","-3")
A1=as.integer(A[c(A)>0])
mean(A1)

BMI = c(18.9, 35.4, 25, 17.5, 23.1, 29.9, 26, 21.3)
length(BMI)-sum((BMI)>=30)-sum((BMI)<25)

BMI = c(18.9, 35.4, 25, 17.5, 23.1, 29.9, 26, 21.3)
sum((BMI)>=25)/length(BMI)

#Question 10
P1=list(systolic=140, diastolic=100)
P2=list(systolic=160, diastolic=80)
class(c(P1,P2))
print(c(P1$systolic-P1$diastolic,P2$systolic-P2$diastolic))


