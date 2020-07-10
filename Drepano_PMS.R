
library(MASS)
library(survival)
library(MatchIt)
library(optmatch)
library(tidyverse)


Drepms<-read.csv("C:/Togo_recherche/drepano_PMS.csv", header=T,na.strings = c(""," ","NA"), stringsAsFactors = F)
colnames(Drepms)


Drepms1<-Drepms %>% 
  dplyr::select(HB,Age,Matrim,Educ,Gestite,Parite,CPN,Adm,Accht,STAT,HOSP)

#### PMS 

m.out<-matchit(STAT~HB+Age+Matrim+Educ+Gestite+Parite, data=Drepms1, method="optimal",distance="logit", ratio=1)
summary(m.out)
plot(m.out, type = "hist")


m.data=match.data(m.out)

ps<-glm(HB~Age+Matrim+Educ+Gestite+Parite, data=Drepms1,family=binomial())
summary(ps)
Drepms1$psvalue<-predict(ps,type="response")


d.fit<-glm(STAT~HB+Accht+CPN+Adm, data = Drepms1, family = binomial())
summary(d.fit)


## Next task: Reverse the SS and SC code to the original before the PMS
