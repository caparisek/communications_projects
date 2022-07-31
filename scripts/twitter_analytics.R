#explore twitter analytics 

library(tidyverse)
library(fBasics)

twitter<-read_csv("data/TwitterAnalytics.csv")

twitter2<-twitter %>% 
  dplyr::filter(!is.na(Mentions))

fBasics::basicStats(twitter2$NewFollowers)

twitter2 %>% 
  ggplot()+
  scale_x_date(date_labels ="%m/%Y", date_breaks="1 month")+
  geom_bar(aes(x=Date_ForMonthOf, y=TotalFollowers),stat="identity")+
  #geom_bar(aes(x=Date_ForMonthOf, y=NewFollowers),stat="identity")+
  #geom_bar(aes(x=Date_ForMonthOf, y=Mentions),stat="identity")+
  #geom_bar(aes(x=Date_ForMonthOf, y=ProfileVisits),stat="identity")+
  #geom_bar(aes(x=Date_ForMonthOf, y=`Tweet Impressions`),stat="identity")+
  #geom_bar(aes(x=Date_ForMonthOf, y=Tweets),stat="identity")+
  theme_minimal()+
  theme(axis.text.x =element_text(size=15,angle=90))




