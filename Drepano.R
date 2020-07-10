
library(tidyverse)
library(MASS)
library(survival)
library(MatchIt)
library(optmatch)
library(kableExtra)
library(qwraps2)
library(gtsummary)
library(epiDisplay)
source("C:/Togo_recherche/survplotR.txt")


Drepano<-read.csv("C:/Togo_recherche/drepanocytose.csv", header=T,na.strings = c(""," ","NA"), stringsAsFactors = F)
names(Drepano)
head(Drepano)
table(Drepano$HB)


#### PMS 

m.out<-matchit(treat~re74+re75+age+educ, data=pms2019, method="optimal",distance="logit", ratio=2)
summary(m.out)



#data wrangling and cleaning

Drepano2<-Drepano %>%
  select(HB,Age,Gestite,Parite,CPN,HospiZ,nHospiZ,Foldine,TPI,AgeZ,Etatarrive,STAT,CVO,URI,PULM,INF,ECL,STA) 

Drepano2$HB<-recode(Drepano2$HB, SS=0, SC=1)

  




############################################################# Survival analysis#######################################################
drepsurv<-read.csv("C:/Togo_recherche/Drepano_surv.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors=F)



drepsurv.fit<-survfit(Surv(HOSP, STAT)~1, data=drepsurv)
summary(drepsurv.fit)
survplot(drepsurv.fit)
plot(drepsurv.fit, lwd=3, col=c("red","black","blue"), lty=c(1:length(drepsurv.fit$strata)))



drepsurv.fit2<-survfit(Surv(HOSP, STAT)~HEP, data=drepsurv)
summary(drepsurv.fit2)
survplot(drepsurv.fit2,col=c("red","navyblue"),lwd=3)


drepsurv.fit3<-survfit(Surv(HOSP, STAT)~Accht, data=drepsurv)
summary(drepsurv.fit3)
survplot(drepsurv.fit3, xlab="Duree d'hospitalisation", ylab="Survie", main="Survie des drepanocytaires par type d'accouchement",
         col=c("red","black"),lwd=3)


drepsurv.fit4<-survfit(Surv(HOSP, STAT)~HB, data=drepsurv)
summary(drepsurv.fit4)
survplot(drepsurv.fit4)


drepsurv.fit5<-survfit(Surv(HOSP, STAT)~HB+Accht, data=drepsurv)
summary(drepsurv.fit5)
survplot(drepsurv.fit5,lwd=3)


drepsurv.fit6<-survfit(Surv(HOSP, STAT)~HB+Accht+HEP, data=drepsurv)
summary(drepsurv.fit6)
survplot(drepsurv.fit6)

##### Cox Hazard ########

drepcoxhz<-coxph(Surv(HOSP, STAT)~Accht, data=drepsurv)
summary(drepcoxhz)


coxph(Surv(HOSP, STAT)~Accht, data=drepsurv) %>% 
  gtsummary::tbl_regression(exp=T)




##### Table 1 #######################################


table(drepsurv$STAT)
tabpct(drepsurv$STAT, drepsurv$HB)

#Exposure vs outcome
tab1<-table(drepsurv$HB, drepsurv$STAT)
tab1<-tab1[c("0","1"), c("0", "1")]
tab1
a<-ggplot(data2, aes(anxiety, ..count..)) + geom_bar(aes(fill=hypertension), position="dodge")
a

#Exposure vs covariates

tab2<-table(data2$anxiety, data2$sex_2)
tab2<-tab2[c("Anxiety","No Anxiety"),]
tab2


#Exposure vs covariates
tab3<-table(data2$anxiety, data2$sex_2)
tab3<-tab3[c("Anxiety", "No Anxiety"),]
tab3


tab4<-table(data2$anxiety, data2$age_cat)
tab4<-tab4[c("Anxiety", "No Anxiety"),]
tab4


tab5<-table(data2$anxiety, data2$healthcarecost)
tab5<-tab5[c("Anxiety", "No Anxiety"),]
tab5


tab6<-table(data2$anxiety, data2$accesscare)
tab6<-tab6[c("Anxiety", "No Anxiety"),]
tab6


tab7<-table(data2$anxiety, data2$poverty)
tab7<-tab7[c("Anxiety", "No Anxiety"),]
tab7

#Covariates vs outcome

tab8<-table(data2$sex_2, data2$hypertension)
tab8<-tab8[c("Male", "Female"),c("Hypertension","No hypertension")]   #maybe female first!
tab8

tab9<-table(data2$age_cat, data2$hypertension)
tab9<-tab9[,c("Hypertension","No hypertension")]
tab9

tab10<-table(data2$healthcarecost, data2$hypertension)
tab10<-tab10[,c("Hypertension","No hypertension")]
tab10


tab11<-table(data2$accesscare, data2$hypertension)
tab11<-tab11[,c("Hypertension","No hypertension")]
tab11


tab12<-table(data2$poverty, data2$hypertension)
tab12<-tab12[,c("Hypertension","No hypertension")]
tab12


##Creating table 1
#OPTION1

tab1(data2$anxiety, main="Distribution of anxiety", col=c("mediumseagreen", "mediumspringgreen"))

tab1(data2$hypertension, main="Distribution of Hypertension", col=c("royalblue", "royalblue4"))
tabpct(data2$hypertension, data2$anxiety, graph=FALSE)


colnames(drepsurv)

#OPTION2
data2_summaries <-
  list(
    "HB, n(%)"=
      list("Hemoglobin SC" = ~ qwraps2::n_perc0(HB=="0"),
           "Hemoglobin SS" = ~ qwraps2::n_perc0(HB=="1")
           ),
    "Accht,n(%)"=
      list("Voie basse" = ~qwraps2::n_perc0(Accht=="0")),
    "HEP, n(%)"= 
      list("Heparin use" = ~ qwraps2::n_perc0(HEP=="1"), 
           "No Heaprin use" = ~ qwraps2::n_perc0(HEP=="0")
      ))


#vignette("summary-statistics", package = "qwraps2")
str(data2_summaries, max.level = 1) #these should all be a list!



table1 <- cbind(summary_table(drepsurv, data2_summaries),
                summary_table(dplyr::group_by(drepsurv, STAT), data2_summaries))

tab_1 <- kable(table1, booktabs=T, "html", 
               caption = "Characteristics of Total Sample Population and Sample Population Stratified by vital status",
               col.names = c("Total Population (n)", "Alive (n)", "Deceased (n)"),
               digits=1)%>%
  pack_rows("HB, n(%)", 1,2) %>%
  pack_rows("Accht, n(%)", 3,3) %>%
  pack_rows("HEP, n(%)", 4, 5)


tab_1 #note if you put this in rmarkdown place it as "latex" document (add latex to after booktabs=T)

