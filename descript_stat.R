library (haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library (stargazer)

#Data 
#BDfull<-read.csv("Documents/Memoire/BD/DataBase.csv")

#theme set
theme_set(theme_minimal())
theme_update(legend.title = element_blank(),
             text = element_text(family = "AppleGothic")) 

#most populated regions
BiggerCantons <- c("Quito", "Guayaquil", "Cuenca")

#Average years of schooling by regions 

BDcantons<-BDfull%>% group_by(Canton, Year) %>%summarise(mean_schooling = mean(schooling, na.rm = TRUE))
BDcantons <- transform(BDcantons , Year = as.numeric(Year))


ggplot(BDcantons , aes(x=Year, y=mean_schooling, group=Canton)) +
  geom_line(color="grey80", size=0.08)  +
  geom_line(data = BDcantons %>% filter(Canton == BiggerCantons),
            size = 0.5,
            aes(color = Canton))+
  labs(title = "Years of Schooling by Canton",
       subtitle = "Since 2000",
       caption = "Data source: Encuesta de Empleo, Desempleo y Subempleo ENEMDU (2000-2021)")+
  ylab("Average Years of Schooling")+
  geom_vline( aes(xintercept = 2007), color = "grey40", linetype = "dotted",size = .5)+
  geom_text(aes(x=2008, label="2007 Reform", y=2), angle=0,size= 3,color="grey40")
  
#average salary per regions 

BDwage<-BDfull%>% filter(age>18)
BDwage<- BDwage%>% group_by(Canton, Year) %>%summarise(mean_salary = mean(salary, na.rm = TRUE))
BDwage <- transform(BDwage , Year = as.numeric(Year))

ggplot(BDwage, aes(x=Year, y=mean_salary, group=Canton)) +
  geom_line(color="grey80", size=0.08)  +
  geom_line(data = BDwage %>% filter(Canton == BiggerCantons),
            size = 0.5,
            aes(color = Canton))+
  labs(title = "Average Wage by Canton",
       subtitle = "Since 2000",
       caption = "Data source: Encuesta de Empleo, Desempleo y Subempleo ENEMDU (2000-2021)")+
  ylab("Average Wage")+
  geom_vline( aes(xintercept = 2007), color = "grey40", linetype = "dotted",size = .5)+
  geom_text(aes(x=2008, label="2007 Reform", y=1200), angle=0,size= 3,color="grey40")



##############################################################################

#Average years of schooling by regions with and without UEMs
BDUEMedu<- BDfull%>% filter(age>18)
BDUEMedu<- BDUEMedu%>% select(Year, Canton, schooling, UEMs)%>% mutate(YesNo= case_when(UEMs>=1 ~ "With", UEMs<1~ "Without"))
BDUEMedu<- BDUEMedu%>% group_by(YesNo, Year) %>%summarise(mean_schooling = mean(schooling, na.rm = TRUE))
BDUEMedu <- transform(BDUEMedu, Year = as.numeric(Year))

ggplot(BDUEMedu, aes(x=Year, y=mean_schooling, group=YesNo, color=YesNo)) +
  geom_line(size=0.5) +
  scale_y_continuous(limits=c(0, 15)) +
  labs(title = "Average Years of Schooling by Regions with and without UEMs",
       subtitle = "Since 2000",
       caption = "Data source: Encuesta de Empleo, Desempleo y Subempleo ENEMDU (2000-2021)")+
  ylab("Average Years of Schooling")+
  geom_vline( aes(xintercept = 2007), color = "grey40", linetype = "dotted",size = .5)+
  geom_text(aes(x=2008, label="2007 Reform", y=7), angle=0,size= 3,color="grey40")



#Average Wage by regions with and without UEMs 
BDUEMwage<- BDfull%>% filter(age>18)
BDUEMwage<- BDUEMwage%>% select(Year, Canton, salary, UEMs)%>% mutate(YesNo= case_when(UEMs>=1 ~ "With", UEMs<1~ "Without"))
BDUEMwage<- BDUEMwage%>% group_by(YesNo, Year) %>%summarise(mean_salary = mean(salary, na.rm = TRUE))
BDUEMwage <- transform(BDUEMwage, Year = as.numeric(Year))

ggplot(BDUEMwage , aes(x=Year, y=mean_salary, group=YesNo, color=YesNo)) +
  geom_line(size=0.5) +
  labs(title = "Average Wage by Regions with and without UEMs",
       subtitle = "Since 2000",
       caption = "Data source: Encuesta de Empleo, Desempleo y Subempleo ENEMDU (2000-2021)")+
  ylab("Average Wage")+
  geom_vline( aes(xintercept = 2007), color = "grey40", linetype = "dotted",size = .5)+
  geom_text(aes(x=2008, label="2007 Reform", y=7), angle=0,size= 3,color="grey40")



##############################################################################

#table UEMs

UEMS<-matrix(c(2009, 0, 2, 2, 21, 2,
               2010, 1, 2, 3, 14, 3,
               2012, 2, 0, 2, 57, 2,
               2013, 5, 1, 6, 51, 6,
               2014, 16, 1, 17, 115, 14, 
               2015, 5, 4, 9, 39, 7,
               2016, 1, 1, 2, 3, 2, 
               2017, 13, 18, 31, 100, 19, 
               2018, 5, 8, 13, 61, 7,
               2019, 3, 27, 30, 264,12,
               2020,1, 0, 1, 2, 1,
               "Total",51, 65,116, 727, 75), ncol=6,byrow=TRUE)

colnames(UEMS) <- c("Year","Minor UEMs","Mayor UEMs","Total UEMs", "Total n. of schools","N. of treated Cantons")
stargazer(UEMS, summary=FALSE, title="Number of UEMs built by year and type, and the total number of comparable educational units",
          column.sep.width = "0pt",
          notes.append = FALSE, notes.align = "l",
          notes = "\\parbox[t]{14cm}{Source:administrative database of the ministry of health}",
          table.placement = "h") 








