wd=setwd("D:/Documentos/Essex/Modelling Experimental Data/group project")
#dir(wd)
countries=read.csv("imputation.csv", header=T)

#countries=data[1:217,]
tail(countries)

##########   1. Education:
############    - primary_school
############    - primary_completion
############    - adolscent_fert (even when is not an education variable there may be
############                      some correlation between primary completion and this one)
############    - secondary_education_duration
############    - sec_ed_teachers

#countries cor.test
cor.test(countries$primary_school, countries$primary_completion)  # p-value(1.531e-12) is <0.05, cor= --0.4560131 ,significantly correlated.
cor.test(countries$primary_school, countries$adolscent_fert)      # p-value(3.366e-09) is <0.05, cor= 0.3878015,significantly correlated.
cor.test(countries$primary_completion, countries$adolscent_fert)  # p-value(8.051e-14) is <0.05, cor= -0.4785612,significantly correlated.

cor.test(countries$secondary_education_duration,countries$sec_ed_teachers) #### p-value(0.06768) is >0.05, cor= 0.1242684 ####

#regression 
col_1 = summary(lm(formula = countries$primary_school ~ countries$primary_completion + countries$adolscent_fert))
VIF_1 = 1/(1-col_1$r.squared)   # VIF_1 = 1.32493

col_2 = summary(lm(formula = countries$secondary_education_duration~countries$sec_ed_teachers))
VIF_2 = 1/(1-col_2$r.squared)   # VIF_2 = 1.015685



########## 2. Economic indicators:
###########    - GDP_pc
###########    - rural population
###########    - health expenditure
###########    - health expenditure per capita
###########    - electricity
###########    - adj_income
###########    - unemployement total
###########    - mobile coverage
###########    - internet users

#countries cor.test
cor.test(countries$GDP_pc, countries$rural_pop)          # p-value(< 2.2e-16) is <0.05, cor= -0.6899855, they are significantly correlated.
cor.test(countries$GDP_pc, countries$health_exp)         #### p-value(0.08339) is >0.05, cor= 0.1178041 ###  
cor.test(countries$GDP_pc, countries$health_exp_pc)      # p-value(< 2.2e-16) is <0.05, cor= 0.886965, they are significantly correlated.
cor.test(countries$GDP_pc, countries$electricity)        # p-value(< 2.2e-16) is <0.05, cor= 0.7346948, they are significantly correlated.
cor.test(countries$GDP_pc, countries$adj_income)         # p-value(3.941e-16) is <0.05, cor= 0.5155133, they are significantly correlated.
cor.test(countries$GDP_pc, countries$unemployment_total) #### p-value(0.1283) is >0.05, cor= -0.1035577 ####
cor.test(countries$GDP_pc, countries$mobile_coverage)    # p-value(< 2.2e-16) is <0.05, cor= 0.5809759, they are significantly correlated.
cor.test(countries$GDP_pc, countries$internet_users)     # p-value(< 2.2e-16) is <0.05, cor= 0.8295114, they are significantly correlated.

#regression
col_3 = summary(lm(formula = countries$GDP_pc ~ countries$rural_pop + countries$health_exp +countries$health_exp_pc + 
                             countries$electricity + countries$adj_income + countries$unemployment_total + countries$mobile_coverage +
                             countries$internet_users))
VIF_3 = 1/(1-col_3$r.squared) # VIF_3 = 9.007608

col_GDP_rural = summary(lm(formula = countries$GDP_pc ~ countries$rural_pop))
VIF_GDP_rural = 1/(1-col_GDP_rural$r.squared)  # VIF_GDP_rural = 1.908689

col_GDP_health = summary(lm(formula =  countries$GDP_pc ~ countries$health_exp_pc))
VIF_GDP_health = 1/(1-col_GDP_health$r.squared)  # VIF_GDP_health = 4.688385

col_GDP_electricity = summary(lm(formula = countries$GDP_pc ~ countries$electricity))
VIF_GDP_electricity = 1/(1-col_GDP_electricity$r.squared)  # VIF_GDP_health = 2.172857

col_GDP_internet = summary(lm(formula = countries$GDP_pc ~ countries$internet_users))
VIF_GDP_internet = 1/(1-col_GDP_internet$r.squared)   # VIF_GDP_internet = 3.206045

summary(lm(countries$health_exp_pc~countries$health_exp+countries$pop_total))

scatter.smooth(countries$GDP_pc, countries$adj_income)

##########  3. Health indicators
############   - mortality_water
############   - health_exp
############   - health_exp_pc

#countries cor.test
cor.test(countries$mortality_water, countries$health_exp)     # p-value(9.172e-05) is < 0.05, cor= -0.262379, significantly correlated.
cor.test(countries$mortality_water, countries$health_exp_pc)  # p-value(< 2.2e-16) is < 0.05, cor= --0.8344875, significantly correlated.
cor.test(countries$health_exp, countries$health_exp_pc)       # p-value(3.94e-12) is < 0.05, cor= 0.4484107, significantly correlated.

#regression
col_4 = summary(lm(formula = countries$mortality_water ~ countries$health_exp + countries$health_exp_pc))
VIF_4 = 1/(1-col_4$r.squared) # VIF_4 = 1.599791

col_mortality_health = summary(lm(formula = countries$mortality_water ~ countries$health_exp_pc))
VIF_mortality_health = 1/(1-col_mortality_health$r.squared)  # VIF_mortality_health = 3.293475



##########  4. Communication services:
############    - Mobile_coverage
############    - Internet_users

#countries cor.test
cor.test(countries$mobile_coverage, countries$internet_users)  # p-value(2.2e-16) is < 0.05, cor= 0.5781635, significantly correlated.

#regression
col_5 = summary(lm(formula = countries$mobile_coverage ~ countries$internet_users))
VIF_5 = 1/(1-col_5$r.squared) #VIF_5 = 1.502117


