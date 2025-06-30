library (haven)
library(tidyverse)
library(dplyr)
library(stringr)
library (stargazer)
library(did)
library(sandwich)
library(lmtest)
library(stats)

install.packages("gtsummary")
install.packages("did")

#Data 
#BDfull<-read.csv("Documents/Memoire/BD/DataBase.csv")

  #2021 database 
  BD2021<- BDfull%>% filter(Year==2021)
  BD2021<- BD2021%>% filter(age>=18)
  BD2021<- BD2021%>% rename(group=building)
  BD2021 <- transform(BD2021, group = as.numeric(group))
  BD2021 <- transform(BD2021, Year = as.numeric(Year))
  
  
  #logsalary 
  BD2021<- BD2021%>% mutate(log_salary= log(salary+1))

  #Dummies
  BD2021<- BD2021%>% mutate(Dif= case_when(UEMs>=1~ 1,UEMs<1 ~ 0))
  BD2021<- BD2021%>%mutate(Tem= case_when(age%in%18:30~ 1,age%in%30:42 ~ 0))
  
  
  #women & men
  women<- BD2021%>% filter(sex==2)
  men<- BD2021%>% filter(sex==1)
  
  #regressions

  reg1 = lm(log_salary~ Dif*Tem, data = BD2021)
  summary(reg1)
  
    reg11 = lm(log_salary~ Dif*Tem, data = women)
    summary(reg11)
    reg12 = lm(log_salary~ Dif*Tem, data = men)
    summary(reg12)
  
    stargazer(reg1, reg11,reg12,title="Wages",column.labels=c("whole sample","women","men"), align=TRUE)
    
  
  reg2 = lm(schooling~ Dif*Tem, data = BD2021)
  summary(reg2)
  
    reg21 = lm(schooling~ Dif*Tem, data = women)
    summary(reg21)
    reg22 = lm(schooling~ Dif*Tem, data = men)
    summary(reg22)
    
    stargazer(reg2, reg21,reg22, align=TRUE, title = "Years of schooling", column.labels=c("whole sample","women","men"))

    
#correlation between error terms 
  
  acf(reg1$residuals, type = "correlation")
  acf(reg11$residuals, type = "correlation")
  acf(reg12$residuals, type = "correlation")
  #no significant correlation between error terms 
  
  acf(reg2$residuals, type = "correlation")
  acf(reg21$residuals, type = "correlation")
  acf(reg22$residuals, type = "correlation")
  #significant correlation between error terms 

  
#clustering error terms in years of schooling regresions  
  
  
  clreg2<- coeftest(reg2, vcov=vcovCL,cluster=~ Canton)
  summary(clreg2)
  
  clreg21<- coeftest(reg21, vcov=vcovCL,cluster=~ Canton)
  summary(clreg21)
  
  clreg22<- coeftest(reg22, vcov=vcovCL,cluster=~ Canton)
  summary(clreg22)
  
  stargazer(clreg22,clreg21,clreg22, align=TRUE, 
            title = "Years of schooling", column.labels=c("whole sample","women","men"),
            dep.var.labels = "schooling")

     
################################################################################   
  
  #taking start dates into account when treatment started

  BD2021<-BD2021%>%mutate(group= case_when(group==2009 ~ 1, group== 2010 ~ 2, group== 2012 ~ 4,
                                           group==2013 ~ 5, group== 2014 ~ 6, group== 2015 ~ 7,
                                           group==2016 ~ 8, group== 2017 ~ 9, group== 2018 ~ 10,
                                           group==2019 ~ 11, group== 2020 ~ 12))
  
  
  
  BD2021<- BD2021%>% mutate(Tem1= case_when(group==1 & age>30 ~ 0, group==1 & age<=30~ 1,
                                              group==2 & age>29 ~ 0, group==2 & age<=29~ 1,
                                              group==3 & age>28 ~ 0, group==3 & age<=28~ 1,
                                              group==4 & age>27 ~ 0, group==4 & age<=27~ 1,
                                              group==5 & age>26 ~ 0, group==5 & age<=26~ 1,
                                              group==6 & age>25 ~ 0, group==6 & age<=25~ 1,
                                              group==7 & age>24 ~ 0, group==7 & age<=24~ 1,
                                              group==8 & age>23 ~ 0, group==8 & age<=23~ 1,
                                              group==9 & age>22 ~ 0, group==9 & age<=22~ 1,
                                              group==10 & age>21 ~ 0, group==10 & age<=21~ 1,
                                              group==11 & age>20 ~ 0, group==11 & age<=20~ 1,
                                              group==12 & age>19 ~ 0, group==12 & age<=19~ 1 ))
                                              
 
  reg00 = lm(schooling~ Dif*Tem1, data = BD2021)
  summary(reg00) 
  #all coeficeints defined 
  
  reg01 = lm(log_salary~ Dif*Tem1, data = BD2021)
  summary(reg01)
  #Coefficients: (1 not defined because of singularities- i.e. correlation between independent variables)
  
  
  #Diff in diff multiple groups and time periods 
  
  DinD<- att_gt(yname = "log_salary",
                tname = "period",
                gname = "group",
                panel = FALSE,
                xformla = NULL,
                control_group = c("nevertreated", "notyettreated"),
                data = BD2021)
  
  summary(DinD)
  
################################################################################   
  #double differences table 
  
  #salary 
  #T=0
  results0<-BD2021 %>%filter(Tem==0)%>%
    group_by(Dif) %>%
    summarise(mean=mean(log_salary,na.rm = TRUE), 
              var=var(log_salary,na.rm = TRUE), 
              N=length(Dif), 
              na_sum = sum(is.na(log_salary))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results0 <- bind_rows(results0, results0[2,]-results0[1,])
  results0$group<- c("D=0", "D=1", "Difference")
  
  diff_se0 <- sqrt(results0$var[1]/results0$n[1] + results0$var[2]/results0$n[2])
  diff_se0
  
  #T=1
  results1<-BD2021 %>%filter(Tem==1)%>%
    group_by(Dif) %>%
    summarise(mean=mean(log_salary,na.rm = TRUE), 
              var=var(log_salary,na.rm = TRUE), 
              N=length(Dif), 
              na_sum = sum(is.na(log_salary))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results1 <- bind_rows(results1, results1[2,]-results1[1,])
  results1$group<- c("D=0", "D=1", "Difference")
  
  diff_se1 <- sqrt(results1$var[1]/results1$n[1] + results1$var[2]/results1$n[2])
  diff_se1
  
  
  Diff<-results1$mean[3]-results0$mean[3]
  Diff
  SE<-sqrt(results0$var[1]/results0$n[1] + results0$var[2]/results0$n[2]+ results1$var[1]/results1$n[1] + results1$var[2]/results1$n[2])
  SE
  
  
  #with
  results2<-BD2021 %>%filter(Dif==1)%>%
    group_by(Tem) %>%
    summarise(mean=mean(log_salary,na.rm = TRUE), 
              var=var(log_salary,na.rm = TRUE), 
              N=length(Tem), 
              na_sum = sum(is.na(log_salary))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results2 <- bind_rows(results2, results2[2,]-results2[1,])
  results2$group<- c("T=0", "T=1", "Difference")
  
  diff_se2 <- sqrt(results2$var[1]/results2$n[1] + results2$var[2]/results2$n[2])
  diff_se2
  
  #without
  results3<-BD2021 %>%filter(Dif==0)%>%
    group_by(Tem) %>%
    summarise(mean=mean(log_salary,na.rm = TRUE), 
              var=var(log_salary,na.rm = TRUE), 
              N=length(Tem), 
              na_sum = sum(is.na(log_salary))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results3 <- bind_rows(results3, results3[2,]-results3[1,])
  results3$group<- c("T=0", "T=1", "Difference")
  
  diff_se3 <- sqrt(results3$var[1]/results3$n[1] + results3$var[2]/results3$n[2])
  diff_se3
  
  #table
  
  results0<-matrix(c(results0$mean[1],results0$mean[2],results0$mean[3],
                     results0$se[1],results0$se[2],diff_se0),
                   ncol=3,byrow=TRUE)
  
  results1<-matrix(c(results1$mean[1],results1$mean[2],results1$mean[3],
                     results1$se[1],results1$se[2],diff_se1),
                   ncol=3,byrow=TRUE)
  
  results<-matrix(c(results3$mean[3],results2$mean[3], Diff,
                    diff_se3, diff_se2, SE),
                  ncol=3,byrow=TRUE)
  
  table1 <- rbind(results0,results1,results)
  colnames(table1) <- c("D=0","D=1","Difference")
  rownames(table1) <- c("T=0","no","T=1","noo","Difference","nooo")
  
  table1 <- as.data.frame(table1)
  stargazer(table1, summary=FALSE, column.sep.width = "50pt",
            title = "Wages",
            notes.append = FALSE, notes.align = "l",
            notes = "\\parbox[t]{12cm}{Note: The sample is made of the individuals who earn a wage. Standard errors are in parentheses}") 
  
  t.test(table1$Difference)
  
  
  #years of schooling 
  #T=0
  results0<-BD2021 %>%filter(Tem==0)%>%
    group_by(Dif) %>%
    summarise(mean=mean(schooling,na.rm = TRUE), 
              var=var(schooling,na.rm = TRUE), 
              N=length(Dif), 
              na_sum = sum(is.na(schooling))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results0 <- bind_rows(results0, results0[2,]-results0[1,])
  results0$group<- c("D=0", "D=1", "Difference")
  
  diff_se0 <- sqrt(results0$var[1]/results0$n[1] + results0$var[2]/results0$n[2])
  diff_se0
  
  #T=1
  results1<-BD2021 %>%filter(Tem==1)%>%
    group_by(Dif) %>%
    summarise(mean=mean(schooling,na.rm = TRUE), 
              var=var(schooling,na.rm = TRUE), 
              N=length(Dif), 
              na_sum = sum(is.na(schooling))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results1 <- bind_rows(results1, results1[2,]-results1[1,])
  results1$group<- c("D=0", "D=1", "Difference")
  
  diff_se1 <- sqrt(results1$var[1]/results1$n[1] + results1$var[2]/results1$n[2])
  diff_se1
  
  
  Diff<-results1$mean[3]-results0$mean[3]
  Diff
  SE<-sqrt(results0$var[1]/results0$n[1] + results0$var[2]/results0$n[2]+ results1$var[1]/results1$n[1] + results1$var[2]/results1$n[2])
  SE
  
  #with
  results2<-BD2021 %>%filter(Dif==1)%>%
    group_by(Tem) %>%
    summarise(mean=mean(schooling,na.rm = TRUE), 
              var=var(schooling,na.rm = TRUE), 
              N=length(Tem), 
              na_sum = sum(is.na(schooling))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results2 <- bind_rows(results2, results2[2,]-results2[1,])
  results2$group<- c("T=0", "T=1", "Difference")
  
  diff_se2 <- sqrt(results2$var[1]/results2$n[1] + results2$var[2]/results2$n[2])
  diff_se2
  
  #without
  results3<-BD2021 %>%filter(Dif==0)%>%
    group_by(Tem) %>%
    summarise(mean=mean(schooling,na.rm = TRUE), 
              var=var(schooling,na.rm = TRUE), 
              N=length(Tem), 
              na_sum = sum(is.na(schooling))) %>%
    mutate(n = N - na_sum) %>% 
    mutate(se = sqrt(var/N))
  
  results3 <- bind_rows(results3, results3[2,]-results3[1,])
  results3$group<- c("T=0", "T=1", "Difference")
  
  diff_se3 <- sqrt(results3$var[1]/results3$n[1] + results3$var[2]/results3$n[2])
  diff_se3
  
  #table
  
  results0<-matrix(c(results0$mean[1],results0$mean[2],results0$mean[3],
                     results0$se[1],results0$se[2],diff_se0),
                   ncol=3,byrow=TRUE)
  
  results1<-matrix(c(results1$mean[1],results1$mean[2],results1$mean[3],
                     results1$se[1],results1$se[2],diff_se1),
                   ncol=3,byrow=TRUE)
  
  results<-matrix(c(results3$mean[3],results2$mean[3], Diff,
                    diff_se3, diff_se2, SE),
                  ncol=3,byrow=TRUE)
  
  table2 <- rbind(results0,results1,results)
  colnames(table2) <- c("D=0","D=1","Difference")
  rownames(table2) <- c("T=0","no","T=1","noo","Difference","nooo")
  
  table2 <- as.data.frame(table2)
  
  stargazer(table2, summary=FALSE, column.sep.width = "50pt",
            title="Years of schooling",
            notes.append = FALSE, notes.align = "l",
            notes = "\\parbox[t]{12cm}{Note: The sample is made of the individuals who earn a wage. Standard errors are in parentheses}")
  
  
  t.test(table2$Difference)    
 
 
  
  
  
  
  
  
  

  
  
  


  





