########################
####### MA 317 #########
##### Group Project ####
## Clenaning the Data ##
#######################

#loading libraries
library(ggplot2)
library(pastecs)
library(psych)

#set wd
wd=setwd("D:/Documentos/Essex/Modelling Experimental Data/group project")

#reading the data set
data=read.csv("LifeExpectancy1.csv", header=T)

#asigning different names
names(data)=c("country", "code", "life_exp", "electricity", "adj_income", "primary_school", "exp_primary_ed",
              "ppp", "mortality_water", "literacy_rate", "pop_growth", "pop_total", "primary_completion", "secondary_education_duration",
              "sec_ed_teachers", "health_exp", "health_exp_pc", "unemployment_total", "unemployment_youth",
              "rural_pop", "adolscent_fert", "GDP_pc", "mobile_coverage", "internet_users")

#Creating a list of variables to be transformed to log form     
lista=list("adj_income","primary_school","mortality_water","pop_total","secondary_education_duration",
           "GDP_pc", "mobile_coverage", "sec_ed_teachers","health_exp_pc")

#loop to iterate throught the list
for (l in lista){
  print(l)
  data[l]=log(data[l])
}

#save the data
write.csv(data, "LifeExpectancy1_log.csv", row.names = F)

