data=read.csv("LifeExpectancy1_log.csv", header=T)
head(data)

countries=data[1:217,]

#Data imputation using Mean
countries$adj_income[is.na(countries$adj_income)]<- mean(countries$adj_income, na.rm=TRUE)
countries$electricity[is.na(countries$electricity)]<- mean(countries$electricity, na.rm = TRUE)
countries$pop_growth[is.na(countries$pop_growth)]<- mean(countries$pop_growth, na.rm = TRUE)
countries$pop_total[is.na(countries$pop_total)]<- mean(countries$pop_total, na.rm = TRUE)
countries$primary_completion[is.na(countries$primary_completion)]<-mean(countries$primary_completion, na.rm = TRUE)
countries$secondary_education_duration[is.na(countries$secondary_education_duration)]<- mean(countries$secondary_education_duration, na.rm = TRUE)
countries$health_exp[is.na(countries$health_exp)]<-mean(countries$health_exp, na.rm = TRUE)
countries$rural_pop[is.na(countries$rural_pop)]<- mean(countries$rural_pop, na.rm = TRUE)
countries$mobile_coverage[is.na(countries$mobile_coverage)]<- mean(countries$mobile_coverage, na.rm = TRUE)
countries$internet_users[is.na(countries$internet_users)]<- mean(countries$internet_users, na.rm = TRUE)

#Median imputation for Primary school, due to presence of outliers
sum.primary=summary((countries$primary_school))[3]
sum.primary
countries$primary_school[is.na(countries$primary_school)]<- sum.primary

sum.gdp=summary((countries$GDP_pc))[3]
sum.gdp
countries$GDP_pc[is.na(countries$GDP_pc)]<- sum.gdp

sum.health_pc=summary((countries$health_exp_pc))[3]
sum.health_pc
countries$health_exp_pc[is.na(countries$health_exp_pc)]<- sum.health_pc

sum.mortarlity_water=summary((countries$mortality_water))[3]
sum.mortarlity_water
countries$mortality_water[is.na(countries$mortality_water)]<- sum.mortarlity_water

sum.adolscent_fert=summary((countries$adolscent_fert))[3]
sum.adolscent_fert
countries$adolscent_fert[is.na(countries$adolscent_fert)]<- sum.adolscent_fert

#Missing values Imputation using linear regression with plots
#Regression for expenditure
lm_exp<- (lm(countries$exp_primary_ed~countries$electricity+countries$adj_income+
               countries$primary_school+countries$mortality_water+countries$pop_growth+
               countries$pop_total+countries$primary_completion+
               countries$secondary_education_duration+countries$health_exp+
               countries$health_exp_pc+countries$rural_pop+countries$adolscent_fert+
               countries$GDP_pc+countries$mobile_coverage+countries$internet_users))
lm_exp
summary(lm_exp)
pred1 <- predict(lm_exp)

impute <- function (a, a.impute){ ifelse (is.na(a), a.impute, a)}
countries$exp_primary_ed <- impute(countries$exp_primary_ed,pred1)

#Regression for literacy rate,  with plots
#LITERACY RATE HAS A LOT OF MISSING VALUES, NEVERTHELESS, HERE IS THE LINEAR REGRESSION FOR IT
lm_litrate<-(lm(countries$literacy_rate~countries$electricity+countries$adj_income+
                  countries$primary_school+countries$mortality_water+countries$pop_growth+
                  countries$pop_total+countries$primary_completion+countries$secondary_education_duration+
                  countries$health_exp+countries$health_exp_pc+countries$rural_pop+countries$adolscent_fert+
                  countries$GDP_pc+countries$mobile_coverage+countries$internet_users))
pred2 <- predict(lm_litrate)
countries$literacy_rate <- impute(countries$literacy_rate,pred2)

#Regression for secondary education teachers with plots
lm_seced<-(lm(countries$sec_ed_teachers~countries$electricity+countries$adj_income+
                countries$primary_school+countries$mortality_water+countries$pop_growth+
                countries$pop_total+countries$primary_completion+countries$secondary_education_duration+
                countries$health_exp+countries$health_exp_pc+countries$rural_pop+
                countries$adolscent_fert+countries$GDP_pc+countries$mobile_coverage+
                countries$internet_users))
pred3 <- predict(lm_seced)
countries$sec_ed_teachers<-+ impute(countries$sec_ed_teachers,pred3)


#Regression for unemployment total with plots
lm_total.unemp<-(lm(countries$unemployment_total~countries$electricity+
                      countries$adj_income+countries$primary_school+countries$mortality_water+
                      countries$pop_growth+countries$pop_total+countries$primary_completion+
                      countries$secondary_education_duration+countries$health_exp+countries$health_exp_pc+
                      countries$rural_pop+countries$adolscent_fert+countries$GDP_pc+countries$mobile_coverage+
                      countries$internet_users))
pred4 <- predict(lm_total.unemp)
countries$unemployment_total <- impute(countries$unemployment_total,pred4)

#Regression for unemployment total with plots
lm_total.youth<-(lm(countries$unemployment_youth~countries$electricity+
                      countries$adj_income+countries$primary_school+countries$mortality_water+
                      countries$pop_growth+countries$pop_total+countries$primary_completion+
                      countries$secondary_education_duration+countries$health_exp+countries$health_exp_pc+
                      countries$rural_pop+countries$adolscent_fert+countries$GDP_pc+countries$mobile_coverage+
                      countries$internet_users))
pred5 <- predict(lm_total.youth)
countries$unemployment_youth <- impute(countries$unemployment_youth,pred5)

write.csv(countries, "imputation.csv", row.names = F)