
# EDA script for crime data in Sunnyside / Woodside


crime_path <- "~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYPD_7_Major_Felony_Incidents.csv"
pop_path<-"~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYC_Blocks_2010CensusData_Plus_Precincts.csv"
setwd("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/Harlem_Portfolio")
source("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/HWE_Fuctions/HWE_functions.R")

library(readr)
crime_dat <- read_csv(crime_path)
pop_dat <- read_csv(pop_path)

dups <- names(pop_dat)[!duplicated(names(pop_dat))]

pop_dat<-pop_dat[,dups]

library(dplyr)
library(magrittr)
pop_dat_lk <-
  pop_dat %>% 
  group_by(precinct) %>% 
  summarise(sum(SUMLEV))

# str(crime_dat)
# glimpse(crime_dat)
# range(crime_dat$`Occurrence Date`,na.rm=T)

# Sunnyside and Woodside fall mostly in 108 (LIC/Woodside) and 114th (Astoria/Sunnyside)
# http://www.nyc.gov/html/nypd/html/precinct_maps/precinct_finder.shtml



# Extract lat/lon from Location 1 variable
crime_dat %<>% 
  mutate(lat = strsplit(`Location 1`,split=",")[[1]][1]
            ,lat = gsub("[(]","",lat)
            ,lat = gsub(" ","",lat)
            ,lon = strsplit(`Location 1`,split=",")[[1]][2]
            ,lon = gsub("[)]","",lon)
            ,lon = gsub(" ","",lon)
            )


focus_data <- 
  crime_dat %>% 
  filter(Precinct%in%c(108,114))

library(ggplot2)
library(ggthemes)

focus_data %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)) %>% 
  group_by(Precinct,`CompStat Year`) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`CompStat Year`,y=count))+
  geom_bar(stat="identity")+
  #coord_cartesian(ylim=c(1500,2100))+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  facet_wrap(~Precinct)
  
focus_data %>% 
  filter(`CompStat Year`%in%c(2006,2015)) %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)) %>% 
  group_by(`CompStat Year`) %>% 
  summarise(count=n()) %>%
  mutate(Perc_Change = (count - lag(count,1) )/ lag(count,1)) %>% 
  knitr::kable()



focus_data %>% 
  ggplot(aes(x=`CompStat Month`))+
  geom_bar(stat="count")+
  facet_wrap(~`CompStat Year`)
  
focus_data %>% 
  group_by(`CompStat Year`,`CompStat Month`) %>% 
  summarise(count=n()) %>%
  ungroup() %>% 
  group_by(`CompStat Month`) %>% 
  summarise(av=mean(count,na.rm=T)) %>% 
  ggplot(aes(x=`CompStat Month`,y=av))+geom_bar(stat="identity")



lvls <- 
  focus_data %>% 
  group_by(`CompStat Year`,Offense) %>% 
  summarise(count=n()) %>% 
  filter(`CompStat Year`==2015) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  select(Offense)


focus_data %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)
         ,Offense = factor(Offense, levels = as.character(lvls$Offense))) %>% 
  group_by(Precinct,`CompStat Year`,`Offense Classification`,Offense) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`CompStat Year`,y=count,group=`Offense`,color=`Offense`))+
  geom_line(size=2)+
  facet_wrap(~Precinct)+
  scale_color_tableau()+
  theme_hwe()+
  labs(title = "Major Crimes in Sunnyside/Woodside"
       ,caption = "Source: NYPD"
       ,x = NULL
       ,y = NULL)




  crime_dat %>% 
  mutate(color = ifelse(Precinct%in%c(108,114),"Sunnydise/Woodside","Other")
         ) %>% 
  group_by(`CompStat Year`,Borough,color) %>% 
  arrange(color) %>% 
  filter(!is.na(Borough)
         ,!is.nan(Borough)) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`CompStat Year`,y=count,fill=color))+geom_bar(stat="identity")+facet_wrap(~Borough)







