
# GENERATING CHARTS FOR CRIME STATISTICS FOR SUNNYSIDE / WOODSIDE CRIME


# initialize a list that we will fill with plots
master.plot.list<-list()

setwd("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/Woodside_Morgenstern/Woodside_Morgenstern")
repo_path <- "~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/Woodside_Morgenstern/Woodside_Morgenstern"
crime_path <- "~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYPD_7_Major_Felony_Incidents.csv"
pop_path<-"~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/Crime/NYC_Blocks_2010CensusData_Plus_Precincts.csv"
source("~/Dropbox (hodgeswardelliott)/Team_NYC/Tim_Kiely/HWE_Fuctions/HWE_functions.R")

library(readr)
if(!exists("crime_dat")){
  crime_dat <- read_csv(crime_path)
}

# Population data, removing duplicate columns

if(!exists("pop_dat")){
  dups <- names(pop_dat)[!duplicated(names(pop_dat))]
  pop_dat<-pop_dat[,dups]
}

precincts <- pop_dat %>% select(precinct,P0010001) %>%  group_by(precinct) %>% summarise(population = sum(P0010001))

# str(crime_dat)
# glimpse(crime_dat)
# range(crime_dat$`Occurrence Date`,na.rm=T)

# Harlem properties fall mostly in 30 and 33rd precinct
# (exception of 174 W 137th St, which falls in 32nd precinct)


library(dplyr)
focus_data <- 
  crime_dat %>% 
  filter(Precinct%in%c(108,114))

library(ggplot2)
library(ggthemes)


# ALL NYC, crime by incident year
master.plot.list$crime_plot_1<-
crime_dat %>% 
  filter(`Occurrence Year`>2006) %>% 
  group_by(`Occurrence Year`,Borough,Offense) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`Occurrence Year`,y=count,group=Offense,fill=Offense))+
  #geom_line()+
  geom_bar(stat="identity")+
  facet_wrap(~Borough)


# count of crimes
master.plot.list$crime_plot_2<-
focus_data %>% 
  filter(`Occurrence Year`>2005) %>% 
  #filter(!Offense%in%c("GRAND LARCENY","BURGLARY")) %>% 
  #filter(!Offense%in%c("GRAND LARCENY")) %>% 
  mutate(`Occurrence Year` = factor(`Occurrence Year`)) %>% 
  group_by(Precinct,`Occurrence Year`,Offense) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`Occurrence Year`,y=count))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  facet_wrap(~Precinct)+
  labs(title = "Crimes in Sunnyside/Woodside"
       #,subtitle="Crime has declined 23% since 2006 in the 30th and 33rd Precincts"
       ,caption="Source: NYPD"
       ,x=NULL
       ,y="Count of Felonies")



# crimes per 1000 residents
master.plot.list$crime_plot_2a<-  
  focus_data %>% 
  filter(`Occurrence Year`>2005) %>% 
  mutate(`Occurrence Year` = factor(`Occurrence Year`)) %>% 
  group_by(Precinct,`Occurrence Year`) %>% 
  summarise(count=n()) %>% 
  left_join(precincts,by=c("Precinct"="precinct")) %>%
  ungroup() %>% 
  mutate(population_div_1000 = population/1000
         ,"Crime Per 1000" = count/population_div_1000
         ,Precinct = factor(Precinct)) %>% 
  ggplot(aes(x=`Occurrence Year`,y=`Crime Per 1000`,fill=Precinct,group=Precinct))+
  geom_bar(stat="identity",position="dodge")+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  theme(legend.position="right"
        ,axis.text.x=element_text(angle=90))+
  scale_fill_tableau('tableau10')+
  #facet_wrap(~Precinct,ncol=2)+
  labs(title = "Sunnyside & Woodside"
       ,subtitle="Crimes per 1,000 residents"
       ,caption="\nSource: NYPD \nhttps://data.cityofnewyork.us/Public-Safety/NYPD-7-Major-Felony-Incidents/hyij-8hr7"
       ,x=NULL
       ,y=NULL)



# H Heights vs Williamsburg, total
master.plot.list$crime_plot_3<-
  crime_dat %>% 
  filter(`Occurrence Year`>2005) %>% 
  mutate(Area = ifelse(Precinct==90, "Williamsburg","Other")
         ,Area = ifelse(Precinct%in%c(108,114), "Sunnyside/Woodside",Area)
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
  labs(title = "Sunnyside/Woodside vs Williamsburg"
       ,subtitle="Count of major crimes"
       ,caption="\nSource: NYPD \nhttps://data.cityofnewyork.us/Public-Safety/NYPD-7-Major-Felony-Incidents/hyij-8hr7"
       ,x=NULL
       ,y=NULL)
  
  
# MONEY SLIDE
# H Heights vs Williamsburg, per 1,000 residents
master.plot.list$crime_plot_4<-  
  crime_dat %>% 
  filter(`Occurrence Year`>2005) %>% 
  mutate(Area = ifelse(Precinct==90, "Williamsburg","Other")
         ,Area = ifelse(Precinct%in%c(108,114), "Sunnyside/Woodside",Area)
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
  #coord_cartesian(ylim=c(10,18))+
  scale_y_continuous(labels=scales::comma)+
  theme_hwe()+
  theme(legend.position="none")+
  scale_fill_tableau('tableau20')+
  facet_wrap(~Area)+
  labs(title = "Sunnyside/Woodside vs Williamsburg"
       ,subtitle="Crimes per 1,000 residents"
       ,caption="\nSource: NYPD \nhttps://data.cityofnewyork.us/Public-Safety/NYPD-7-Major-Felony-Incidents/hyij-8hr7"
       ,x=NULL
       ,y=NULL)
   


lvls <- 
  focus_data %>% 
  group_by(`CompStat Year`,Offense) %>% 
  summarise(count=n()) %>% 
  filter(`CompStat Year`==2015) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  select(Offense)

master.plot.list$crime_plot_6<- 
focus_data %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)
         ,Offense = factor(Offense, levels = as.character(lvls$Offense))) %>% 
  group_by(`CompStat Year`,`Offense Classification`,Offense) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=`CompStat Year`,y=count,group=`Offense`,color=`Offense`))+
  geom_line(size=2)+
  scale_color_tableau()+
  theme_hwe()+
  labs(title = "Major Crimes in Sunnyside/Woodside"
       ,caption = "Source: NYPD"
       ,x = NULL
       ,y = NULL)



master.plot.list$crime_plot_7<- 
crime_dat %>% 
  mutate(`CompStat Year` = factor(`CompStat Year`)) %>% 
  mutate(Borough = factor(Borough, levels = c("BROOKLYN","MANHATTAN","QUEENS","BRONX","STATEN ISLAND"))) %>% 
  mutate(Area = ifelse(Precinct%in%c(108,114), "Sunnyside/Woodside","Other")
         ) %>% 
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
  labs(title="As Percent of Borough"
       ,subtitle = NULL
       ,caption = NULL
       ,x = NULL
       ,y = NULL)




save(master.plot.list,file=paste0(repo_path,"/","Master_Plot_List.RData"))

