library(readxl)
dataset<-read_excel("E:/AIUB/9th semester/Data Science/Mid_Project/Dataset_MIdterm_sectoin(B).xlsx")
dataset

names(dataset)

colSums(is.na(dataset))


dataset$smoking_history[dataset$smoking_history=="No Info"] <- NA


most_frequency_value<- table(dataset$smoking_history)
most_frequency_value
sort_most_frequence_value<-sort(most_frequency_value,decreasing = TRUE) 
sort_most_frequence_value
mode_sort_most_frequency_value<-names(sort_most_frequence_value)[1]
mode_sort_most_frequency_value

dataset$smoking_history[is.na(dataset$smoking_history)]<-mode_sort_most_frequency_value

colSums(is.na(dataset))

 

most_frequency_value_gender<- table(dataset$gender)
print(most_frequency_value_gender)
sort_most_frequence_value_gender<-sort(most_frequency_value_gender,decreasing = TRUE) 
sort_most_frequence_value_gender
mode_sort_most_frequency_value_gender<-names(sort_most_frequence_value_gender)[1]
mode_sort_most_frequency_value_gender

dataset$gender[is.na(dataset$gender)]<-mode_sort_most_frequency_value_gender

colSums(is.na(dataset))



most_frequency_value_hypertension<- table(dataset$hypertension) 
most_frequency_value_hypertension

sort_most_frequence_value_hypertension<-sort(most_frequency_value_hypertension,decreasing = TRUE) 
sort_most_frequence_value_hypertension

mode_sort_most_frequency_value_hypertension<-as.numeric(names(sort_most_frequence_value_hypertension)[1])
mode_sort_most_frequency_value_hypertension
dataset$hypertension[is.na(dataset$hypertension)]<-mode_sort_most_frequency_value_hypertension
colSums(is.na(dataset))



dataset$age[is.na(dataset$age)]<- mean(dataset$age,na.rm = TRUE)


summary_statics<-summary(dataset[c('age','bmi','HbA1c_level','blood_glucose_level')])
summary_statics

dataset$bmi<- abs(dataset$bmi)





hist(dataset$age,main = "Age Distribution",xlab = 'Age',col = 'lightblue')
hist(dataset$bmi,main = "BMI Distribution",xlab = 'BMI',col = 'blue')
hist(dataset$HbA1c_level,main = "HbAlc_level Distribution",xlab = 'HbAlc_level',col = 'pink')
hist(dataset$blood_glucose_level,main = "Glucose level Distribution",xlab = 'Glucose_level',col = 'yellow')

 
barplot(table(dataset$heart_disease),main = "Heart Disease Distribution",xlab = "Heart_disease",ylab = 'count',col = "red")

barplot(table(dataset$gender),main = "Gender Distribution",xlab = 'gender',ylab = 'count',col = 'lightgreen')

barplot(table(dataset$hypertension),main = "Hypertension Distribution",xlab = 'hypertension',ylab = 'count',col = 'orange')

barplot(table(dataset$smoking_history),main = "Smoking History",xlab = 'smoking history',ylab = 'count',col = 'darkgreen')

barplot(table(dataset$diabetes),main = 'Diabetes',xlab = 'Diabetes',ylab = 'count',col = 'darkblue')


boxplot(dataset$age, main='Box Plot for AGE')

q1<-quantile(dataset$age,0.25)
q1
q3<-quantile(dataset$age,0.75)
q3
iqr<-q3-q1
iqr
outliers_age<- dataset$age < (q1-1.5*iqr)| dataset$age > (q3+1.5*iqr)
outliers_age
dataset$age <- ifelse(outliers_age,NA,dataset$age)
dataset$age[is.na(dataset$age)]<-mean(dataset$age,na.rm = TRUE)
boxplot(dataset$age, main='Box Plot for AGE')


boxplot(dataset$bmi, main='Box Plot for BMI')
q1<-quantile(dataset$bmi,0.25)
q1
q3<-quantile(dataset$bmi,0.75)
q3
iqr<-q3-q1
iqr
outliers_bmi<-dataset$bmi<(q1-1.5*iqr)| dataset$bmi>(q3+1.5*iqr)
dataset$bmi<-ifelse(outliers_bmi,NA,dataset$bmi)
dataset$bmi[is.na(dataset$bmi)]<-mean(dataset$bmi,na.rm = TRUE)

boxplot(dataset$bmi, main='Box Plot for BMI')


boxplot(dataset$HbA1c_level, main='Box Plot for HbAlc level')
q1<-quantile(dataset$HbA1c_level,0.25)
q1
q3<-quantile(dataset$HbA1c_level,0.75)
q3
iqr<-q3-q1
iqr
outliers_HbA1c_level<-dataset$HbA1c_level<(q1-1.5*iqr)| dataset$HbA1c_level>(q3+1.5*iqr)
dataset$HbA1c_level<-ifelse(outliers_HbA1c_level,NA,dataset$HbA1c_level)
dataset$HbA1c_level[is.na(dataset$HbA1c_level)]<-mean(dataset$HbA1c_level,na.rm = TRUE)
boxplot(dataset$HbA1c_level, main='Box Plot for HbAlc level')



boxplot(dataset$blood_glucose_level, main='Box Plot for Glucose level')
q1<-quantile(dataset$blood_glucose_level,0.25)
q1
q3<-quantile(dataset$blood_glucose_level,0.75)
q3
iqr<-q3-q1
iqr
outliers_blood_glucose_level<-dataset$blood_glucose_level<(q1-1.5*iqr)| dataset$blood_glucose_level>(q3+1.5*iqr)
dataset$blood_glucose_level<-ifelse(outliers_blood_glucose_level,NA,dataset$blood_glucose_level)
dataset$blood_glucose_level[is.na(dataset$blood_glucose_level)]<-mean(dataset$blood_glucose_level,na.rm = TRUE)


