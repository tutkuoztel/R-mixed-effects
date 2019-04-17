########### DEBUGGED!!!!!!!!!!!!!!!!!! #########################
# dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
# no centering 
#datayý center ettiðimizde BIC AIC deðerleri deðiþmiyor.
# aic-biclerin ayný olmasý için REML seçeneðini untick etmek gerekiyor jamovide


###################### FIXED INTERCEPT SEÇENEÐÝNÝ KALDIRMAYI DENE JAMOVÝDE ##############################
###################### R USES DUMMY SCHEME AS DEFAULT BUT YOU SHOULD STILL FIX THE INTERCEPT PROBLEM ##########
setwd("C:\\Users\\tutku\\Desktop\\2. pilot data\\yepyeni analizler 750ler included 210319 BUNU KULLAN")
myData=read.csv("C:\\Users\\tutku\\Desktop\\2. pilot data\\yepyeni analizler 750ler included 210319 BUNU KULLAN\\jamovi_mixed effect\\zConf_ZSL_for_mixedEffects_EXP2_CV_280319.csv")

#REMLlogical scalar - Should the estimates be chosen to optimize the REML criterion (as opposed to the log-likelihood)?


library(tidyverse) #for data handling and plotting
library(lme4) #for the linear mixed effect models
library(lmerTest) #for significance tests
library(effects) #for constructing predictions from the effects [esp. to plot interactions]
library(sjPlot)
library(sjmisc)
# random intercept coefficientler ve intercept coeffler ayný deðil
myData <- myData%>%mutate(allReproAbs=abs(allRepro))
myDataTRANS <- myData%>%mutate(expCondCat=factor(expCond, levels=c(2,1), labels=c("nonsocial", "social")))

# all main effects
exp2modelConf.0 <- lmer(allConf~(1|IDs),data=myDataTRANS,REML=FALSE) #jamovi ile birebir ayný
exp2modelConf.1 <- lmer(allConf ~ allReproAbs +(1 | IDs), data=myDataTRANS, REML=FALSE) #doðru Jamovi'de zRepro'yu center etmezsek.
exp2modelConf.2 <- lmer(allConf ~ allReproAbs + expCondCat + (1 | IDs), data=myDataTRANS, REML=FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
exp2modelConf.3 <- lmer(allConf ~ allReproAbs + expCondCat + allCV+(1|IDs), data=myDataTRANS,REML = FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile



exp2modelConf.9 <- lmer(allConf~expCondCat+(1|IDs),data=myDataTRANS,REML=FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
exp2modelConf.11 <- lmer(allConf~allCV+(1|IDs),data=myDataTRANS,REML=FALSE) #no center deyince herþey ayný
# ~ dan sonra 1 koyunca biþey deðiþmiyor
#REMLlogical scalar - Should the estimates be chosen to optimize the REML criterion (as opposed to the log-likelihood)?

# interaction terms
exp2modelConf.4 <- lmer(allConf ~ expCondCat * allReproAbs + (1 | IDs), data=myDataTRANS, REML=FALSE)#dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
exp2modelconf.5 <- lmer(allConf ~ expCondCat * allReproAbs * allCV + (1| IDs) , data=myDataTRANS,REML= FALSE)#dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile


#all model comparison
anova(exp2modelConf.0,exp2modelConf.1,exp2modelConf.2,exp2modelConf.3,exp2modelConf.4,exp2modelConf.5,exp2modelConf.9,exp2modelConf.11)


############ SL MAIN EFFECTS 11.04.2019
exp2modelSL.0 <- lmer(allSL ~ (1 | IDs), data=myDataTRANS, REML=FALSE) #jamovi ile ayný
exp2modelSL.1 <- lmer(allSL ~ allRepro +(1 | IDs), data=myDataTRANS, REML=FALSE) # reproyu center etmediðimizde jamovi ile ayný
exp2modelSL.2 <- lmer(allSL ~ allRepro + expCondCat + (1 | IDs), data=myDataTRANS, REML=FALSE)#dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
exp2modelSL.3 <- lmer(allSL ~ allRepro + expCondCat + allCV + (1 | IDs), data=myDataTRANS, REML=FALSE)#dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile

exp2modelSL.4 <- lmer(allSL~expCondCat+(1|IDs),data=myDataTRANS,REML=FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
exp2modelSL.5 <- lmer(allSL~allCV+(1|IDs),data=myDataTRANS,REML=FALSE) #her þey ayný center etmediðimiz sürece

##SL INTERACTION TERMS
exp2modelSL.6 <- lmer(allSL ~ allRepro*expCondCat + (1 | IDs), data=myDataTRANS, REML=FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
exp2modelSL.7 <- lmer(allSL ~ allRepro*expCondCat*allCV + (1 | IDs), data=myDataTRANS, REML=FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile

plot_model(exp2modelSL.7, type = "pred", terms = c("allRepro","expCondCat"))

# model comparison
anova(exp2modelSL.0,exp2modelSL.1,exp2modelSL.2,exp2modelSL.3,exp2modelSL.4,exp2modelSL.5,exp2modelSL.6,exp2modelSL.7)



##################################################################################################




####### STUDY 3 11.04.2019. FIXED INTERCEPT TÝKÝ TÝKLÝ 16.04.19
rm(list=ls()) #clear  global environment
setwd("C:\\Users\\tutku\\Desktop\\exp3\\yepisyeniHipotezTesti_TEZICINBUDOSYA\\mixed effects")
dataExp3=read_csv("dataForMixedEffects_exp3.csv")


dataExp3TRANS <- dataExp3%>%mutate(allStimCaseCat=factor(allStimCase, levels=c(0,1), labels=c("nonsocial", "social")))

# all main effects
myModelExp3.0 <- lmer(rescaledConfS ~ (1|IDs), data = dataExp3TRANS,REML=FALSE) #birebir ayný jamovi ile
myModelExp3.1 <- lmer(rescaledConfS ~ allRepro + (1|IDs),data = dataExp3TRANS,REML=FALSE)#birebir ayný jamovi ile
myModelExp3.2 <- lmer(rescaledConfS ~ allStimCaseCat + (1|IDs),data = dataExp3TRANS,REML=FALSE)#dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
myModelExp3.3 <- lmer(rescaledConfS ~ CV + (1|IDs), data = dataExp3TRANS,REML=FALSE) # jamovi ile birebir ayný

# adding new main effects step by step

myModelExp3.4 <- lmer(rescaledConfS ~ allRepro +allStimCaseCat+ (1|IDs),data = dataExp3TRANS,REML=FALSE)#dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
myModelExp3.5 <- lmer(rescaledConfS ~ allRepro +allStimCaseCat+ CV+(1|IDs),data = dataExp3TRANS,REML=FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile

# interactions
myModelExp3.6 <- lmer(rescaledConfS ~  allRepro*allStimCaseCat +(1 | IDs),data = dataExp3TRANS,REML=FALSE)#dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile
myModelExp3.7 <- lmer(rescaledConfS ~  allStimCaseCat*allRepro*CV +(1 | IDs), data = dataExp3TRANS,REML=FALSE) #dummy yapýp levellarýyla oynayýnca ayný oldu jamovi ile


# model comparison
anova(myModelExp3.0,myModelExp3.1,myModelExp3.2,myModelExp3.3,myModelExp3.4,myModelExp3.5,myModelExp3.6,myModelExp3.7)



#######################################################################################################

########## STUDY 1 12.04.2019. PROBLEM SOLVED. 15.04.19
# R USES DUMMY AS DEFAULT FACTOR CODING SCHEME. AND TAKES THE REFERENCE LEVEL AS SOCIAL DIRECT.
# WHEN YOU PUT THE SOCIAL DIRECT AS THE FIRST LEVEL AND MAKE IT DUMMY IN JAMOVI, THEY ARE ALL THE SAME WITH R OUTPUT

setwd("C:\\Users\\tutku\\Desktop\\pilotData\\pilotdata 32 kiþilik\\jamovi\\newestMixedEffects\\csvs")

dataExp1=read.csv("allDataExp1_forR.csv")

dataExp1<- dataExp1%>%mutate(allReproAbs=abs(allRepro))
dataExp1TRANS <- dataExp1%>%mutate(allStimCaseCat=factor(allStimCase, levels=c(11,12,21,22), labels=c("social direct","social averted","nonsocial direct", "nonsocial averted")))


# all main effects
myModelExp1Conf.0 <- lmer(allConf~(1|IDs),data=dataExp1TRANS,REML=FALSE) #same as jamovi
myModelExp1Conf.1 <- lmer(allConf~allReproAbs + (1|IDs),data=dataExp1TRANS,REML=FALSE) #same as jamovi
myModelExp1Conf.2 <- lmer(allConf~allStimCaseCat+(1|IDs),data=dataExp1TRANS,REML=FALSE)#jamovide dummy yapýnca her þey ayný
myModelExp1Conf.3 <- lmer(allConf~CV+(1|IDs),data=dataExp1TRANS,REML=FALSE)#fixed intercept tikliyken herþey ayný
#plot_model(myModelExp1Conf.1, type = "pred")

#adding new main effects
myModelExp1Conf.4 <- lmer(allConf~+allReproAbs+allStimCaseCat+(1|IDs),data=dataExp1TRANS,REML=FALSE) #jamovi'de dummy yapýnca herþey ayný. bir de levellarla oynamak gerekiyor.center etmemek de lazým
myModelExp1Conf.5 <- lmer(allConf~+allReproAbs+allStimCaseCat+CV+(1|IDs),data=dataExp1TRANS,REML=FALSE) #jamovi'de dummy yapýnca herþey ayný. bir de levellarla oynamak gerekiyor.center etmemek de lazým

#adding interaction terms
myModelExp1Conf.7 <- lmer(allConf~allReproAbs*allStimCaseCat+(1|IDs),data=dataExp1TRANS,REML=FALSE) #birebir ayný dummy yapýnca ve no centering 
myModelExp1Conf.8 <-lmer(allConf~allReproAbs*allStimCaseCat*CV+(1|IDs),data=dataExp1TRANS,REML=FALSE)#birebir ayný dummy yapýnca ve no centering

#myModelExp1Conf.10<-lmer(allConf~allStimCaseCat*CV+(1|IDs),data=dataExp1TRANS,REML=FALSE)#bunda her þey ayný deðil


# model comparison

anova(myModelExp1Conf.0,myModelExp1Conf.1,myModelExp1Conf.2,myModelExp1Conf.3,myModelExp1Conf.4,myModelExp1Conf.5,myModelExp1Conf.7,myModelExp1Conf.8)


# SL
# all main effects
myModelExp1SL.0 <- lmer(allSL~(1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný
myModelExp1SL.1 <- lmer(allSL~allRepro+ (1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný
myModelExp1SL.2 <- lmer(allSL~CV+(1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný
myModelExp1SL.3 <- lmer(allSL~allStimCaseCat+(1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný

# adding new main effects step by step
myModelExp1SL.4 <- lmer(allSL~allRepro+allStimCaseCat+ (1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný
myModelExp1SL.5 <- lmer(allSL~allRepro+allStimCaseCat+CV +(1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný


#adding interaction terms
myModelExp1SL.6 <- lmer(allSL~allRepro*allStimCaseCat+ (1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný
myModelExp1SL.7 <- lmer(allSL~allRepro*allStimCaseCat*CV+ (1|IDs),data=dataExp1TRANS,REML=FALSE)#ayný

# model comparison
anova(myModelExp1SL.0,myModelExp1SL.1,myModelExp1SL.2,myModelExp1SL.3,myModelExp1SL.4,myModelExp1SL.5,myModelExp1SL.6,myModelExp1SL.7)