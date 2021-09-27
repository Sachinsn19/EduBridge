      #Pima Indians Diabetes 

#  Who is Pima Indians ?
#"The Pima Indians ( "River People")are a group of Native Americans living in an area 
#consisting of what is now central and southern Arizona.

# Understanding the data
#The datasets consist of several medical predictor (independent) variables
#and one target (dependent) variable, Outcome. Independent variables include
#the number of pregnancies the patient has had, their BMI, insulin level, 
#age, and so on.


# Load required libraries
library(ggplot2)
library(ggthemes)
library(psych)
library(dplyr)
library(caret)
library(reshape2)
library(corrplot)



# Load the dataset
dataset=read.csv("C:/Users/Sachin/Programs_EduBridge/diabetes.csv")


# Print first 5 row
print(head(dataset,5))


# Print last 5 row
print(tail(dataset,5))


# To View the contents in the dataet
View(dataset)


# Print column names
print(names(dataset))


# Dimention of data
print(dim(dataset))


# Print Statistical summary
describe(dataset)


# Internal structure of data
print(str(dataset)) 


# Display columns  and display some portions of the data
#print(glimpse(dataset))


# Statistical values
print(is.na(dataset))
print(ncol(dataset))
print(nrow(dataset))
print(max(dataset$Outcome))
print(min(dataset$Outcome))
print(sort(dataset$Outcome))
print(which.max(dataset$Outcome))# Return the index of the first maximum value
print(which.min(dataset$Outcome))# Return the index of the first minimum value
print(mean(dataset$Outcome))
print(mean(dataset$Outcome,trim=0.10))
print(var(dataset$Outcome))
print(median(dataset$Outcome))
print(mad(dataset$Outcome))# mean absolute division
print(sd(dataset$Outcome))
print(range(dataset$Outcome))
print(quantile(dataset$Outcome))
print(IQR(dataset$Outcome))
print(t.test(dataset$Outcome))


# Data visualisation


# Create a 2 x 2 plotting matrix
par(mfrow = c(2, 2))


# The $ notation can be used to subset the variable you're interested in.

# Histogram of numerical data
print(hist(dataset$Pregnancies,col="red"))
print(hist(dataset$Age,col="blue3"))
print(hist(dataset$Glucose,col="cyan"))
print(hist(dataset$BMI,col="pink"))
#Age and number of times pregnant are not normal distributions as expected since the underlying population should not be 
#normally distributed either. This 392 observations are just a sample of the original population. On the other hand, the glucose 
#level and BMI seem to follow a normal distribution. When performing any analysis, it is always good to know what is the 
#distribution of the data so all the assumptions for different tests or models can be met."""


# Age distribution

age<-ggplot(dataset,aes(x=Age))+geom_histogram(binwidth=10,col="blue",fill="brown")+
  labs(title="Age column",x="Age","Count")
print(age)


#Pregnancy distribution
print(str(dataset$Pregnancies))
print(table(dataset$Pregnancies))# Create a table for pregnancies


dataset$Outcome <- as.factor(dataset$Outcome)
#All 8 independent variables are numeric. There are two outcomes, this data is good for classifciation. 
#Lets change Outcome to categorical Variable


pd<-ggplot(dataset,aes(x = Pregnancies)) +
  geom_histogram(binwidth = 0.5,aes(fill = Outcome,position = "dodge")) +
  ggtitle("Pregnancies Data Distribution") + ylab("OutCome Counts") +
  theme_light() +
  theme_update(plot.title = element_text(hjust = ))
print(pd)

#Pregnancies data is right skewed.

opm<-ggplot(data = dataset,aes(x = Outcome, y = Pregnancies)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(1,12,1),limits = c(0,12)) +
  ggtitle("Pregnancies boxplot") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() + 
  theme_update(plot.title = element_text(hjust = 0.5))
print(opm)
#Box plot shows, woman who had more pregnancies are more prone to diabetes.This may be important variable for model.

ogm<-ggplot(data = dataset,aes(x = Outcome, y = Glucose)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(80,200,10),limits = c(80,200)) +
  ggtitle("Glucose") +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() + 
  theme_update(plot.title = element_text(hjust = 0.5))
print(ogm)
#Diabetics woman have high Plasma glucose concentration. 
#On average this value is 140 for diabetics woman while this is quite low for non-diabetics.

#Blood Pressure
table(dataset$BloodPressure)


obm<-ggplot(data = dataset,aes(x = Outcome, y = BloodPressure)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(60,110,10),limits = c(60,110)) +
  ylab("Blood Pressure") +
  ggtitle("Blood Pressure Histogram") +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
print(obm)
#Diastolic blood pressure for diabetic woman is higher compare to non-diabetics.

#Triceps skin fold thickness
#Triceps skin-fold thickness normal value for female 23
table(dataset$SkinThickness)
# Let's replace zero with the median value.
dataset$SkinThickness <- ifelse(
  dataset$SkinThickness == 0 , 
  median(dataset$SkinThickness,na.rm = TRUE),
  dataset$SkinThickness)


osm<-ggplot(data = dataset,aes(x = Outcome, y = SkinThickness)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100)) +
  ylab("Triceps skin fold thickness") +
  ggtitle("Skin Thickness Histogram") +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
print(osm)
#Boxplot shows that diabetics woman normally has high skin thickness. 
#Red big dots are outlier but ignoring this outlier to consider the extreme case.


table(dataset$BMI)

obm<-ggplot(data = dataset,aes(x = Outcome, y = BMI)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(20,70,5),limits = c(20,70)) +
  ylab("BMI") +
  ggtitle("Body mass index Histogram") +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
print(obm)
#BMI for diabetics woman is high compare to non-diabetics.
#There are few outlier, let not treat them to consider the extreme cases of BMI.

#Diabetes pedigree function
odpf<-ggplot(data = dataset,aes(x = Outcome, y = DiabetesPedigreeFunction)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,2,0.2),limits = c(0,2)) +
  ylab("Diabetes Pedigree Function") +
  ggtitle("Diabetes Pedigree Function") +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
print(odpf)


#Check the balancing of data
table(dataset$Outcome)
prop.table(table(dataset$Outcome)) 
ggplot(dataset,aes(Outcome))+
  geom_bar(fill=c("red","green"))+
  geom_text(stat = "count",aes(label=stat(count),vjust=0.5))

# it seems to be unbalanced


# correlation matrix
cor_melt <- melt(cor(dataset[, 1:8]))
cor_melt <- cor_melt[which(cor_melt$value > 0.5 & cor_melt$value != 1), ]
cor_melt <- cor_melt[1:3, ]
print(cor_melt)
#correlation values higher than 0.5.


#Let's see the correlation between numerical variables. There are variables which are highly correlated.
#That is the case of Age for example.
correlat <- cor(dataset[, setdiff(names(dataset), 'Outcome')])
print(correlat)
print(corrplot(correlat,method="ellipse"))

#In this analysis, we used the diabetic patient health management follow-up data
#We have combined feature selection and imbalanced processing techniques.

