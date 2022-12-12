
# Script to compare previous list of CWS_PEOPLE to new roster. ID who from the new roster is/isn't already on the website. Helpful to ID who is alum & to locate based on role. 


#1st - replaces spaces with underscores (easiest w "Find&Replace" in excel)

library(tidyverse)
CWS_2021<-read_csv("data/cws/CWS People/2021 Fall - CWS People List from Debbie.csv")
CWS_2021_unknowns<-read_csv("data/cws/CWS People/2021 Fall - CWS People List from Debbie - who had no email or association somehow.csv")
CWS_2022<-read_csv("data/cws/CWS People/2022 Fall - UCP-272 Employee Roster Report_CWS_from Christopher C..csv")

colnames(CWS_2021)
colnames(CWS_2021_unknowns)
colnames(CWS_2022)

unique(CWS_2021$Employee_Name)
unique(CWS_2021_unknowns$Employee_Name)
unique(CWS_2022$Employee_Name)

CWS_2022_new<-CWS_2022 %>% separate(Employee_Name, into = c("Last", "First"), sep = ",")
CWS_2022_new$Employee_Name <- paste(CWS_2022_new$First,CWS_2022_new$Last)
CWS_2022_new<-CWS_2022_new %>% 
  select(c(-First,-Last)) %>% 
  mutate(YEAR = 2022)

?semi_join
?anti_join
#most of 2021 list includes researchers/faculty. most of 2022 list is Student/Researcher-Scholar.
colnames(CWS_2021)
CWS_2021_new<-CWS_2021 %>% 
  #dplyr::filter(!is.na(Student) | !is.na(Researcher_or_Scholar)) %>% 
  mutate(YEAR = 2021)

ANTI_JOIN<- anti_join(CWS_2021_new,CWS_2022_new, by="Employee_Name") #that's weird 

FULL_JOIN<- full_join(CWS_2021_new,CWS_2022_new, by="Employee_Name") 

write_csv(FULL_JOIN,"data output/CWS_People_2021-22.csv")

#did a lot of manual work to connect and check the two. but hopefully in the next file the name-matches will be consistent. 




