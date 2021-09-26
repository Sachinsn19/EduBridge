#Load required libraries
library(dplyr)
library(ggplot2)
library(openintro)
library(tidyverse)
library(scales)

#To find current working directory
getwd()

#To set required working directory
setwd("C:/Users/Sachin/Downloads")

#Read the file
d<-read.csv("ifood.csv")

#To print top 5 rows of the data
head(d)

#To print last 5 rows of the data
tail(d)

#To print column names
print(colnames(d))

#To show complete structure of data
print(str(d))

#To check is there any missing values in data
print(sum(is.na(d)))

#To find statistical information of data
print(summary(d))

#To show the entire data
View(d)

#To show names of food items
print(unique(d$name))

#To find the total time taken for each food items
name=d %>% group_by(name) %>% summarise(Total_Time=max(prep_time+cook_time),
                                        prep_time,cook_time, flavor_profile,course) %>% 
  arrange(desc(Total_Time))
View(name)

#    Diet
#=============


#To show the unique values in diet columns
print(unique(d$diet))

#To find the frequency of categorical values in diet
table(d$diet)

# To extract vegetarian foods and non vegetarian foods
veg<-subset(d,diet=='vegetarian')
View(veg)
non_veg<-subset(d,d$diet=="non vegetarian")
View(non_veg)

#To print names of vegetarian foods and non vegetarian foods
print(veg$name)
print(non_veg$name)

# Pie diagram showing total number of vegetarian and non vegetarian foods
pie(table(d$diet),col = c('#CE1212','#1F441E'),main = "Diet")



#To find the average time taken for preparation of vegetarian foods & non vegetarian foods
print(mean(veg$prep_time))
print(mean(non_veg$prep_time))

#boxplot showing preparation time for veg and non veg foods
v_p<-ggplot(d,aes(x=d$diet,y=prep_time))+geom_boxplot(fill="green3")
print(v_p)

#To find the average time taken for cooking of vegetarian foods & non vegetarian foods
print(mean(veg$cook_time))
print(mean(non_veg$cook_time))

v_c<-ggplot(d,aes(x=d$diet,y=cook_time))+geom_boxplot(fill="green2")
print(v_c)

#To find the frequency of flavor profile in vegetarian and non vegetarian
print(table(veg$flavor_profile))
print(table(non_veg$flavor_profile))

#barplot for showing flavours in each diet
barplot(table(veg$flavor_profile),col=c('brown','blue','yellow','red','green'),horiz = TRUE,main = "Flavors in Vegetarian foods")
barplot(table(non_veg$flavor_profile),col=c('yellow','red','green'),horiz = TRUE,main = "Flavors in Non Vegetarian foods")

#     Flavors
#===================



#To show the unique values in flavor profile
print(unique(d$flavor_profile))

#To find the frequency of each categorical values
table(d$flavor_profile)

#To extract the each flavor foods in the descending order of preparation time
sweet<-name %>% filter(flavor_profile=='sweet')%>% arrange(desc(prep_time))
View(sweet)
spicy<-name %>% filter(flavor_profile=='spicy')%>% arrange(desc(prep_time))
View(spicy)
bitter<-d %>% filter(flavor_profile=='bitter')%>% arrange(desc(prep_time))
View(bitter)
sour<-name %>% filter(flavor_profile=='sour')%>% arrange(desc(prep_time))
View(sour)
savour<-name %>% filter(flavor_profile=='savour')%>% arrange(desc(prep_time))
View(savour)

#To find the frequency of each course in each flavor foods
print(table(d$flavor_profile,d$course))

# ggplot showing course in each flavor
flavor_course=ggplot(d,aes(x=flavor_profile,y=course,fill=course))+geom_col()+
  ggtitle('Course in each Flavor')+coord_flip()+theme(legend.position = 'bottom',
                                                          axis.text.y =  element_text(size=6))
print(flavor_course)


#    course
#===============

#To print unique values in course
print(unique(d$course))

#To print the frequency of course in data
print(table(d$course))

#To find frequency of each flavor in each course
print(table(d$course,d$flavor_profile))

course_flavor=ggplot(d,aes(x=course,y=flavor_profile,fill=flavor_profile))+geom_col()+
  ggtitle('Flavors in each course')+coord_flip()+theme(legend.position = 'bottom',
                                                      axis.text.y =  element_text(size=6))
print(course_flavor)

#       State wise
#============================


#To find the names of each states
print(unique(d$state))


#To find the maximum number of diet in each states
Statewise_diet=d %>% group_by(state) %>% summarise(Diet=max(diet)) %>% 
  arrange(desc(Diet))
View(Statewise_diet)

State_diet=ggplot(d,aes(x=state,y=diet,fill=diet))+geom_col()+
  ggtitle('Statewise Diet Analysis')+coord_flip()+theme(legend.position = 'bottom',
                                                        axis.text.y =  element_text(size=6))
print(State_diet)

# To find the taste of each states
Statewise_flavor=d %>% group_by(state) %>% summarise(Flavor=max(flavor_profile)) %>% 
  arrange(desc(Flavor))
View(Statewise_flavor)

State_flavor=ggplot(d,aes(x=state,y=flavor_profile,fill=flavor_profile))+geom_col()+
  ggtitle('Statewise Flavor Analysis')+coord_flip()+theme(legend.position = 'bottom',
                                                          axis.text.y =  element_text(size=6))
print(State_flavor)

#course in each states
State_course=ggplot(d,aes(x=state,y=course,fill=course))+geom_col()+
  ggtitle('Statewise Course Analysis')+coord_flip()+theme(legend.position = 'bottom',
                                                          axis.text.y =  element_text(size=6))
print(State_course)



#       Region wise
#==========================


#To print the names of regions
print(unique(d$region))

#To extract region wise food information
Central<-subset(d, region=='Central')
View(Central)
East<-subset(d, region=='East')
View(East)
North<-subset(d, region=='North')
View(North)
North_East<-subset(d, region=='North East')
View(North_East)
West<-subset(d, region=='West')
View(West)
South<-subset(d,region=='South')
View(South)

#To find the states in each region
print(unique(Central$state))
print(unique(East$state))
print(unique(North$state))
print(unique(North_East$state))
print(unique(West$state))
print(unique(South$state))

#To find the flavor of each region
print(table(d$region,d$flavor_profile))

#To find the flavor in each course of each region
print(table(Central$course, Central$flavor_profile))
print(table(East$course, East$flavor_profile))
print(table(North$course, North$flavor_profile))
print(table(North_East$course, North_East$flavor_profile))
print(table(West$course, West$flavor_profile))
print(table(South$course, South$flavor_profile))

# To find the taste of each Region
Regionwise_flavor=d %>% group_by(region) %>% summarise(Flavor=max(flavor_profile)) %>% 
  arrange(desc(Flavor))
View(Regionwise_flavor)

# To show taste of each region
Region_flavor=ggplot(d,aes(x=region,y=flavor_profile,fill=flavor_profile))+geom_col()+
  ggtitle('Region wise Flavor Analysis')+coord_flip()+theme(legend.position = 'bottom',
                                                          axis.text.y =  element_text(size=6))
print(Region_flavor)

#To show course in each region
Region_course=ggplot(d,aes(x=region,y=course,fill=course))+geom_col()+
  ggtitle('Region wise Flavor Analysis')+coord_flip()+theme(legend.position = 'bottom',
                                                            axis.text.y =  element_text(size=6))
print(Region_course)

#    conclusion
#=======================
# From this data set, It's showing that there were 255 variety of food items in this data.
# Among this vegetarian food items were more. The food items in non veg were very less compared to vegetarian.
# There is no much difference between cooking and preparation time of vegetarian and non vegetarian foods.
# The food items were mainly sweet and spicy flavors. The flavors were different in different regions.
# The spicy food items were mainly from the states such as Chhattisgarh, Gujrat, Haryana, Nagaland, NCT of Delhi,Tripura.




