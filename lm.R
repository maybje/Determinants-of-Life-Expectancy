########################
####### MA 317 #########
##### Group Project ####
####### Task 4 #########
##### Linear Model#####
#######################7#cleaning ws

#Cleaning dataframe
rm(list=ls())

#loading libraries
library(data.table)
library(ggplot2)
library(ggpubr)
library(stargazer)
library(quantreg)
library(kableExtra)


#Setting wd
wd=setwd("D:/Documentos/Essex/Modelling Experimental Data/Group Project")
countries=read.csv("imputation.csv", header=T) #Read file
head(countries)   #check if it's the correct data
attach(countries) #attach variables to ws

#4. Suggest a model which explains life expectancies in 2016. Justify you answer. Can this model be used
#to predict life expectancies of other countries which have not provided in 2016 data on life expectancy?
#adding a variable GINI (Income inequality)
gini=fread("WDIData.csv", header=T)  #read data
gini=gini[gini$`Indicator Name`=="GINI index (World Bank estimate)",c(1,2,61)]  #keeping just year of interest

countries=merge(countries, gini[,c(1,3)], by.x="country", by.y="Country Name")  #merging with country data
names(countries)[names(countries)=="2016"]="gini"
attach(countries) #attach variables to ws
rm(gini)

#we analyse the gini data
summary(gini)

#perform regression imputation
model_gini=summary(lm(gini~countries$electricity+countries$adj_income+
                        countries$primary_school+countries$mortality_water+countries$pop_growth+
                        countries$pop_total+countries$primary_completion+countries$secondary_education_duration+
                        countries$health_exp+countries$health_exp_pc+countries$rural_pop+countries$adolscent_fert+
                        countries$GDP_pc+countries$mobile_coverage+countries$internet_users))
X=matrix(1,nrow(countries))

X=cbind(X, countries$electricity,countries$adj_income,
        countries$primary_school,countries$mortality_water,countries$pop_growth,
        countries$pop_total,countries$primary_completion,countries$secondary_education_duration,
        countries$health_exp,countries$health_exp_pc,countries$rural_pop,countries$adolscent_fert,
        countries$GDP_pc,countries$mobile_coverage,countries$internet_users)

countries$gini_i=X%*%as.matrix(model_gini$coefficients[,1])

#impute <- function (a, a.impute){ ifelse (is.na(a), a.impute, a)}
#countries$gini_i <- impute(countries$gini,model_gini)
attach(countries) #attach variables to ws

#summarizing the new imputed var
summary(countries$gini_i)
#comparing density function before &after imputation
countries$gini_i=ifelse(countries$gini_i<25,mean(countries$gini_i),countries$gini_i) #min value before imp
ggplot()+geom_line(data=countries, aes(gini), stat="density")+theme_bw()+
  geom_line(data=as.data.frame(gini_i), aes(gini_i), stat="density", color="red")

#plotting correlation with life expectancy
gp1 = ggplot(countries)+geom_point(aes(log(gini_i), life_exp), colour="red")+theme_bw()+
  geom_smooth(aes(log(gini_i), life_exp), method = "lm") + xlab("log(Gini)") + 
  ylab("Life expectancy")+stat_cor(aes(log(gini_i), life_exp,label = ..r.label..),method = "pearson", label.x = 3.97, label.y = 80)

gp1
ggsave("gini.pdf")
countries$gini=NULL
#Defining the saturated model with only those variables with high correlation (meausred
#by the correlation coefficient) to Life Expectancy 
    #Y: Life Expectancy
    #matrix X: Electricity, water, mortality,  population growth, adjusted income
    #primary completion rate, health expenditure pc
    #and squared (see scatterplot), rural population, adolescent fertilty rate, internet users,
    #mobile coverage

sub=countries[,-c(1,2,8,26)]
corr <- round(cor(na.omit(sub)), 1)
corr=melt(corr)

n=rev(list("Life Expectancy","Electricity","Adjusted Income", "Primary School", "Expenditure Primary","Water Mortality",
       "Literacy Rate", "Population Growth", "Total Population", "Primary Completion", "Secondary Education",
       "Secondary Teachers", "Health Exp.","Health Exp. pc", "Unemployment", "Youth Unemploymnent",
       "Rural Opulation", "Adolescent Fertility", "GDP pc", "Mobile Coverage", "Internet Users", "Gini"))

ggplot(data = corr, aes(x=Var1,ordered(Var2, levels =     rev(sort(unique(Var2)))), fill=value)) + 
  geom_tile()+ theme(axis.title.x = element_blank(),axis.text.y = element_text(vjust=0),
                     axis.title.y = element_blank(),axis.text.x=element_blank(), legend.title = element_blank()) +
  scale_fill_gradient2(low="red", mid="yellow", high="red", 
                       midpoint=0,labels=c("-1.0", "-0.5","0","0.5","1.0"),breaks=c(-1,-.5,0,.5,1),limits=c(-1,1))+
  scale_y_discrete(labels=n)
ggsave("matrix_corr.pdf")

#From section 1 we know there is a non-linear correlation between life expectancy and
# health exp pc in levels. We add an extra squared term to capture this relation.

countries$health_pc_levels=exp(health_exp_pc)/1000
attach(countries)

#We perform stepwise variable selection with both AIC and BIC
best1=step(lm(life_exp~electricity+mortality_water+pop_growth+primary_completion+loghealth_pc_levels+
             I(health_pc_levels^2)+
             rural_pop+adolscent_fert+internet_users+mobile_coverage+adj_income), direction = "both")

aic1=best1$anova$AIC[length(best1$anova$AIC)]
extractAIC(best1, scale=0, k=log(nrow(countries)))

best1_1=step(lm(life_exp~electricity+mortality_water+pop_growth+primary_completion+health_pc_levels+
                I(health_pc_levels^2)+
                rural_pop+adolscent_fert+internet_users+mobile_coverage+adj_income), direction = "both",
           k = log(nrow(countries)))

bic1=best1_1$anova$AIC[length(best1_1$anova$AIC)]

best2=step(lm(life_exp~electricity+pop_growth+primary_completion+health_pc_levels+
          I(health_pc_levels^2)+
          rural_pop+adolscent_fert+internet_users+mobile_coverage+adj_income), direction = "both")

aic2=best2$anova$AIC[length(best2$anova$AIC)]
extractAIC(best2, scale=0, k=log(nrow(countries)))

best2_2=step(lm(life_exp~electricity+pop_growth+primary_completion+health_pc_levels+
                I(health_pc_levels^2)+
                rural_pop+adolscent_fert+internet_users+mobile_coverage+adj_income), direction = "both",
           k = log(nrow(countries)))

bic2=best2_2$anova$AIC[length(best2_2$anova$AIC)]
extractAIC(best2_2, scale=0)
#######################
#Summarize best models
#####################

diagPlot<-function(model){
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()+theme(title = element_text(size=9))
  
  p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
  p2<-p2+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2<-p2+ggtitle("Normal Q-Q")+theme_bw()+theme(title = element_text(size=9))
  
  p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggtitle("Scale-Location")+theme_bw()+theme(title = element_text(size=9))
  p3
  return(ggarrange(p1, p2, p3, ncol=3, nrow=2))
}
ggplot(countries)+geom_line(aes(x=life_exp , y= stat(density)), stat = 'density')+theme_bw()
#############
#####Model 1
#############
summary(best1)

#diagnostic plots
diagPlot(best1)
ggsave("best1.pdf")

#############
#####Model 2
#############
summary(best1_1)

#diagnostic plots
diagPlot(best1_1)
ggsave("best1_1.pdf")

#############
#####Model 3
#############
summary(best2)

#diagnostic plots
diagPlot(best2)
ggsave("best2.pdf")

############
#####Model 4
#############
summary(best2_2)

#diagnostic plots
diagPlot(best2_2)
ggsave("best2_2.pdf")


#####################
#Model with GDP
#####################
#we will test a model using GDP as explanatory variable and
#a additional covariates, namely: GINI, and Continent indicator
continets=read.csv("continents.csv", header=T)  #load continents names
continents2=read.csv("WDICountry.csv", header=T)  #different classification
continents2=continents2[,c(1,8)] #select coiumn1 and column8 on the table

#rename variables
names(continets)[3]="code"
names(continets)[1]="cont"
names(continents2)[1]="code"

#adding continent columns to data frame
countries<-merge(countries,continets[,c(1,3)],by="code") #merge Continent
countries<-merge(countries,continents2,by="code")
attach(countries)


countries$cont <- as.character(countries$cont) #change factor to character
countries$Region <- as.character(countries$Region)  #change factor to character

#renaming continents
countries$Region[countries$Region=="Latin America & Caribbean"]="South America"
countries$cont <- ifelse(countries$cont =="Americas",countries$Region,countries$cont) #replace Americas to South and North Americas
countries$cont[countries$country=="Greenland"]="Europe"

#using "geographic north" criteria
centro=list("Guatemala", "Mexico", "Belize", "Honduras", "El Salvador", "Costa Rica", "Panama","Nicaragua")

for (c in centro){
  countries$cont[countries$country==c]="North America"
}

countries$Region=NULL   #deleting unused variables

#c=unique(cont)

#for (v in c) {
 # countries[,v]=0
  #countries[v]=ifelse(countries["cont"]==v,1,0)
#}

attach(countries)

####MODEL
model_gdp=step(lm(log(life_exp)~GDP_pc+gini_i+cont), direction = "both", k=log(length(na.omit(life_exp))))


#diagnostic
summary(model_gdp)

diagPlot(model_gdp)

ggsave("gdp.pdf")

#scatterplot by continent
ggplot(countries)+geom_point(aes(GDP_pc, life_exp, color=cont, size=exp(pop_total)^0.5))+theme_bw()+
  xlab("log(GDP pc)")+ylab("Life Expectancy")+
  guides(size = FALSE)+theme(legend.position = c(0.85,0.25),legend.title=element_blank(),legend.background=element_blank(),
                             legend.key=element_blank(),)+
  scale_colour_manual(values = c("#DBD522", "#FB0101", "#20CE00", "#00DCD3","#0013DC","#D600DC"))
#  geom_quantile(aes(gini_i, life_exp),quantiles = qs)

ggsave("scattercont.pdf")
###################################
##Table with all models
##################################
models=list(best1,best2,best2_2,model_gdp)
stargazer(models, align=T, keep.stat = c("n", "rsq", "adj.rsq", "aic","bic"),no.space=T,
          dep.var.labels = c("Life Expectancy", "log(Life Expectancy"), covariate.labels = 
            c("Electricity", "log(Water Mort.)", "Health Exp. pc", 
              "Health Exp. pc", "Adolscent Fertility", "Internet Users","Mobile Coverage", "GDP pc", "Gini Index",
              "Asia", "Europe", "North America", "Oceania", "South America", "Constant"))
    


#################
##Unseen Examples
#################
le_hat=countries[is.na(life_exp),]

le_hat$best1=predict(best1,le_hat)
le_hat$best2=predict(best2,le_hat)
le_hat$best2_2=predict(best2_2,le_hat)
le_hat$model_gdp=exp(predict(model_gdp,le_hat))



  

plot(le_hat$health_pc_levels,le_hat$best2)
ggplot(le_hat)+geom_density(aes(le_hat$GDP_pc))

par(mfrow=c(1,1))

########################
##quantile regression
#######################
q=countries[order(life_exp),]
qs=1:4/5
quant=rq(log(q$life_exp)~q$GDP_pc+q$gini_i+q$cont,tau=qs)

summary(quant)
plot(quant)

ggplot(countries)+geom_point(aes(