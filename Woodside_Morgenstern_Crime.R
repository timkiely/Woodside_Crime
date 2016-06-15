
# GENERATING CHARTS FOR CRIME STATISTICS FOR THE HARLEM PORTFOLIO

master.plot.list<-list()

crime_path <- "~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYPD_7_Major_Felony_Incidents.csv"
pop_path<-"~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYC_Blocks_2010CensusData_Plus_Precincts.csv"
setwd("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/Harlem_Portfolio")
source("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/HWE_Fuctions/HWE_functions.R")

library(readr)
if(!exists("crime_dat")){
  crime_dat <- read_csv(crime_path)
}

# Population data
if(!exists("pop_dat")){
  pop_dat <- read_csv(pop_path)
  pop_dat[,2]<-NULL
  pop_dat$SHAPE_Area<-NULL
  pop_dat$SHAPE_Leng<-NULL
  pop_dat$import_notes<-NULL
}

precincts <- pop_dat %>% select(precinct,P0010001) %>%  group_by(precinct) %>% summarise(population = sum(P0010001))

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


# ALL NYC, crime by incident year
master.plot.list$harlem_crime_plot_1<-
crime_dat %>% 
  filter(`Occurrence Year`>2006) %>% 
  group_by(`Occurrence Year`,Borough,Offense) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`Occurrence Year`,y=count,group=Offense,fill=Offense))+
  #geom_line()+
  geom_bar(stat="identity")+
  facet_wrap(~Borough)


# H Heights, just 2012 - 2015
master.plot.list$harlem_crime_plot_2<-
hardat %>% 
  filter(`Occurrence Year`>2011) %>% 
  #filter(!Offense%in%c("GRAND LARCENY","BURGLARY")) %>% 
  #filter(!Offense%in%c("GRAND LARCENY")) %>% 
  mutate(`Occurrence Year` = factor(`Occurrence Year`)) %>% 
  group_by(Precinct,`Occurrence Year`,Offense) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`Occurrence Year`,y=count))+
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(1400,1900))+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  labs(title = "Crimes in Hamilton Heights"
       ,subtitle="Crime has declined 23% since 2006 in the 30th and 33rd Precincts"
       ,caption="Source: NYPD"
       ,x=NULL
       ,y="Count of Felonies")




# H Heights vs Williamsburg, total
master.plot.list$harlem_crime_plot_3<-
  crime_dat %>% 
  filter(`Occurrence Year`>2005) %>% 
  mutate(Area = ifelse(Precinct==90, "Williamsburg","Other")
         ,Area = ifelse(Precinct==30, "Hamilton Heights",Area)
  ) %>% 
  filter(Area!="Other") %>% 
  mutate(`Occurrence Year` = factor(`Occurrence Year`)) %>% 
  group_by(Area,Precinct,`Occurrence Year`) %>% 
  summarise(count=n()) %>% 
  left_join(precincts,by=c("Precinct"="precinct")) %>%
  mutate(population_div_1000 = population/1000
         ,"Crime Per 1000" = count/population_div_1000) %>% 
  ggplot(aes(x=`Occurrence Year`,y=count,fill=Area))+
  geom_bar(stat="identity")+
  #coord_cartesian(ylim=c(10,18))+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  theme(legend.position="none")+
  scale_fill_tableau('tableau20')+
  facet_wrap(~Area)+
  labs(title = "Hamilton Heights vs Williamsburg"
       ,subtitle="Count of major crimes"
       ,caption="\nSource: NYPD \nhttps://data.cityofnewyork.us/Public-Safety/NYPD-7-Major-Felony-Incidents/hyij-8hr7"
       ,x=NULL
       ,y=NULL)
  
  
# MONEY SLIDE
# H Heights vs Williamsburg, per 1,000 residents
master.plot.list$harlem_crime_plot_4<-  
  crime_dat %>% 
  filter(`Occurrence Year`>2005) %>% 
  mutate(Area = ifelse(Precinct==90, "Williamsburg","Other")
         ,Area = ifelse(Precinct==30, "Hamilton Heights",Area)
         ) %>% 
  filter(Area!="Other") %>% 
  mutate(`Occurrence Year` = factor(`Occurrence Year`)) %>% 
  group_by(Area,Precinct,`Occurrence Year`) %>% 
  summarise(count=n()) %>% 
  left_join(precincts,by=c("Precinct"="precinct")) %>%
  mutate(population_div_1000 = population/1000
         ,"Crime Per 1000" = count/population_div_1000) %>% 
  ggplot(aes(x=`Occurrence Year`,y=`Crime Per 1000`,fill=Area))+
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(10,18))+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  theme(legend.position="none")+
  scale_fill_tableau('tableau20')+
  facet_wrap(~Area)+
  labs(title = "Hamilton Heights vs Williamsburg"
       ,subtitle="Crimes per 1,000 residents"
       ,caption="\nSource: NYPD \nhttps://data.cityofnewyork.us/Public-Safety/NYPD-7-Major-Felony-Incidents/hyij-8hr7"
       ,x=NULL
       ,y=NULL)


master.plot.list$harlem_crime_plot_5<- 
  crime_dat %>% 
  filter(`Occurrence Year`%in%c(2006,2015)) %>% 
  mutate(Area = ifelse(Precinct==90, "Williamsburg","Other")
         ,Area = ifelse(Precinct==30, "Hamilton Heights",Area)
  ) %>% 
  filter(Area!="Other") %>% 
  mutate(`Occurrence Year` = factor(`Occurrence Year`)) %>% 
  group_by(Area,Precinct,`Occurrence Year`) %>% 
  summarise(count=n()) %>% 
  left_join(precincts,by=c("Precinct"="precinct")) %>%
  mutate(population_div_1000 = population/1000
         ,"Crime Per 1000" = count/population_div_1000) %>% 
  mutate(Perc_Change = (`Crime Per 1000` - lag(`Crime Per 1000`,1) )/ lag(`Crime Per 1000`,1)) %>% 
  knitr::kable()



lvls <- 
  hardat %>% 
  group_by(`CompStat Year`,Offense) %>% 
  summarise(count=n()) %>% 
  filter(`CompStat Year`==2015) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  select(Offense)

master.plot.list$harlem_crime_plot_6<- 
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



master.plot.list$harlem_crime_plot_7<- 
crime_dat %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)) %>% 
  mutate(Borough = factor(Borough, levels = c("BROOKLYN","MANHATTAN","QUEENS","BRONX","STATEN ISLAND"))) %>% 
  mutate(Area = ifelse(Precinct%in%c(30,33),"Hamilton Heights","Other")
         ,Area = factor(Area, levels=c("Other","Hamilton Heights"))) %>% 
  group_by(`CompStat Year`,Borough,Area) %>% 
  na.omit() %>% 
  filter(Borough!="(null)") %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=`CompStat Year`,y=count,fill=Area))+
  #geom_line(aes(group=Borough,color=Borough))+
  geom_bar(stat="identity")+
  facet_wrap(~Borough,nrow=1)+
  theme_hwe()+
  theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0))+
  scale_fill_tableau()+
  labs(title="Hamilton Heights as Percent of Manhattan"
       ,subtitle = NULL
       ,caption = NULL
       ,x = NULL
       ,y = NULL)




# Hamilton hegh
master.plot.list$harlem_crime_plot_8<-
crime_dat %>% 
  mutate(Hamilton_Heights = ifelse(Precinct%in%c(30,33),"Heights","Rest of Manhattan")) %>% 
  #filter(Borough=="MANHATTAN") %>% 
  group_by(`CompStat Year`,Hamilton_Heights) %>%
  summarise(count=n()) %>% 
  ungroup() %>% 
  #select(-Borough) %>% 
  tidyr::spread(Hamilton_Heights,count) %>% 
  mutate(Heights_as_percent = scales::percent(Heights/`Rest of Manhattan`)) %>% knitr::kable()



setwd("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/Harlem_Portfolio")
save(master.plot.list,file="Master_Plot_List.RData")

