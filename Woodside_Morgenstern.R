


crime_path <- "~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYPD_7_Major_Felony_Incidents.csv"
pop_path<-"~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYC_Blocks_2010CensusData_Plus_Precincts.csv"
setwd("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/Harlem_Portfolio")
source("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/HWE_Fuctions/HWE_functions.R")

library(readr)
crime_dat <- read_csv(crime_path)
pop_dat <- read_csv(pop_path)

library(dplyr)
pop_dat_lk <-
  pop_dat %>% 
  group_by(precinct) %>% 
  summarise(sum(SUMLEV))

# str(crime_dat)
# glimpse(crime_dat)
# range(crime_dat$`Occurrence Date`,na.rm=T)

# Harlem properties fall mostly in 30 and 33rd precinct
# (exception of 174 W 137th St, which falls in 32nd precinct)


library(dplyr)
hardat <- 
  crime_dat %>% 
  filter(Precinct%in%c(30,33))

library(ggplot2)
library(ggthemes)

hardat %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)) %>% 
  group_by(Precinct,`CompStat Year`) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`CompStat Year`,y=count))+
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(1500,2100))+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  labs(title = "Crimes in Washington Heights"
       ,subtitle="Crime has declined 23% since 2006 in the 30th and 33rd Precincts"
       ,caption="Source: NYPD"
       ,x=NULL
       ,y="Count of Felonies")


hardat %>% 
  filter(`CompStat Year`%in%c(2006,2015)) %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)) %>% 
  group_by(`CompStat Year`) %>% 
  summarise(count=n()) %>%
  mutate(Perc_Change = (count - lag(count,1) )/ lag(count,1)) %>% 
  knitr::kable()



hardat %>% 
  ggplot(aes(x=`CompStat Month`))+
  geom_bar(stat="count")+
  facet_wrap(~`CompStat Year`)
  
hardat %>% 
  group_by(`CompStat Year`,`CompStat Month`) %>% 
  summarise(count=n()) %>%
  ungroup() %>% 
  group_by(`CompStat Month`) %>% 
  summarise(av=mean(count,na.rm=T)) %>% 
  ggplot(aes(x=`CompStat Month`,y=av))+geom_bar(stat="identity")



lvls <- 
  hardat %>% 
  group_by(`CompStat Year`,Offense) %>% 
  summarise(count=n()) %>% 
  filter(`CompStat Year`==2015) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  select(Offense)


hardat %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)
         ,Offense = factor(Offense, levels = as.character(lvls$Offense))) %>% 
  group_by(`CompStat Year`,`Offense Classification`,Offense) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`CompStat Year`,y=count,group=`Offense`,color=`Offense`))+
  geom_line(size=2)+
  scale_color_tableau()+
  theme_hwe()+
  labs(title = "Major Crimes in Washington Heights"
       ,caption = "Source: NYPD"
       ,x = NULL
       ,y = NULL)


crime_dat %>% 
  group_by(`CompStat Year`)
  ggplot(aes(x=`CompStat Year`))

all_harlem<-
  crime_dat %>% 
  mutate(color = ifelse(Precinct%in%c(30,33),1,0)) %>% 
  group_by(`CompStat Year`,Borough,color) %>% 
  arrange(color) %>% 
  filter(!is.na(Borough)
         ,!is.nan(Borough)) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`CompStat Year`,y=count,fill=color))+geom_bar(stat="identity")+facet_wrap(~Borough)







