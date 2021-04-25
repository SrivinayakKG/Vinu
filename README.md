# Health Care Cost Analsis
To predict the hospital discharge cost. To analyze the hospital discharge cost TOTCHG as mentioned in the data set and the affect of all other factors which influence the overall hospital discharge cost. The Final analysis will reveal all the significant variable which play a pivotal role is attaining the Hospital Discharge Cost. The Dependent variable in this data set is ‘TOTCHG’ or the total Hospital discharge costs as it he variable whose values are to be predicted with the model. Since the dependent variable is of continues form we will use the Linear Regression Model in the project. All the other variables like ‘Age’, ‘Female’, ‘Los’ (Length of Stay), ‘Race’, ‘Aprdrg’ (All Patient Refined Diagnosis Related Groups) are the independent variables as they will influence the value of the dependent variable ‘TOTCHG’.
Model Interpretation of the First Model: P value for the variables AGE, LOS and APRDRG was found to be less than 0.05 so these values are said to be significant variables. Slope of AGE is derived as positive 112.79 which says that a unit increase in AGE will increase the TOTCH value by 112.79. Slope of LOS is derived as positive 711.8334 which says that a unit increase in LOS value will increase the TOTCH value by 711.833. Slope of APRDRG is found to be negative 8.622 which concludes that a unit increase in APRDRG value will decrease the TOTCH value by 8.622. Multiple R Squared Value is derived as 75.56% which is above the cut off rate of 70% which says that the model is efficient enough.
Model Interpretation of the Final Model: P value for the variables AGE, LOS and APRDRG was found to be less than 0.05 so these values are said to be significant variables. Slope of AGE is derived as positive 106.96 which says that a unit increase in AGE will increase the TOTCH value by 106.96. Slope of LOS is derived as positive 711.33 which says that a unit increase in LOS value will increase the TOTCH value by 711.33. Slope of APRDRG is found to be negative 8.81 which concludes that a unit increase in APRDRG value will decrease the TOTCH value by 8.81. Multiple R Squared Value is derived as 75.2% which is above the cut off rate of 70% which says that the model is efficient enough.
Interpretation of the final output and Overall Conclusion: As it is seen from the final output that the residual values are higher which implies that better and richer data is further required to ensure further accuracy of the model presented. Further analysis will also reveal that the factors AGE and LOS are the more significant factors for the company as a small increase or decrease in the value of these may cause a devastating effect over the Dependent variable TOTCHG. The company must also ensure that the factors AGE and LOS are monitored cautiously and further data is required to be collected with respect to these factors to ensure further accuracy to the model. The File output file embedded for further reference.
getwd()
#Step 1: install the package to read xl files
install.packages("readxl")
library("readxl")

#Step 2: upload the data import the data
data1 = read_excel("/home/vinusahoo_gmail/hospitalcosts.xlsx")
View(data1)

#Step 3: Data Expolration
head(data1)
tail(data1)
data2= head(data1,100)
data3= tail(data1,50)
nrow(data1)
ncol(data1)
dim(data1)
str(data1)
summary(data1$TOTCHG)
levels (data1$TOTCHG)
table (data1$TOTCHG)
mean(data1$TOTCHG)
median(data1$TOTCHG)
min(data1$TOTCHG)
max(data1$TOTCHG)
var(data1$TOTCHG)
sd(data1$TOTCHG)
range(data1$TOTCHG)
class(data1)

#Step 4 : Split the data into training and testing
install.packages("caTools")
library(caTools)
sample = sample.split(data1$TOTCHG,SplitRatio=0.80)
sample
train_data = subset(data1,sample==TRUE) 
test_data = subset(data1,sample==FALSE)

#Step 5 : Building model using train_data
model = lm(TOTCHG~.,data= train_data)
summary(model)

age = 5.05e-16
format(age,sci=FALSE)
los=2e-16
format(los,sci=FALSE)
aprdrg=2e-16
format(aprdrg,sci=FALSE)

final_model = lm(TOTCHG~ AGE + LOS + APRDRG, data = train_data)
summary(final_model)
predtest<- predict(final_model,test_data)
head(predtest)

predtest1<- data.frame(predtest)
final_data<- cbind(test_data,predtest1)
write.csv(final_data,"final_output.csv" )
