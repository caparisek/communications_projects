
library(tidyverse)

#Subscribed (keep)
#Unsubscribed (dont keep, and make sure they aren't in future dfs)
#Cleaned (bounced stuff)



# March 2021 Splash -------------------------------------------------------
RossV_subscribed<-read_csv("data/members_export_b6300f8dad-ROSS list/subscribed_members_export_b6300f8dad.csv",
                           col_types =cols("Is.Student"=col_character(),
                                           "Is.Early.Career"=col_character(),
                                           "Address"=col_character(),
                                           "Phone_Number"=col_double(),
                                           "CONFIRM_IP" =col_character()))
#RossV_cleanedmembers<-read_csv("data/members_export_b6300f8dad-ROSS list/cleaned_members_export_b6300f8dad.csv")
#RossV_unsubscribed<-read_csv("data/members_export_b6300f8dad-ROSS list/unsubscribed_members_export_b6300f8dad.csv")

Shelli_subscribed<-read_csv("data/members_export_8c457ffe03-SHELLI list/subscribed_members_export_8c457ffe03.csv",
                            col_types =cols("Address"=col_character(),
                                            "Phone_Number"=col_double(),
                                            "CONFIRM_IP" =col_character()))
#Shelli_cleanedmembers<-read_csv("data/members_export_8c457ffe03-SHELLI list/cleaned_members_export_8c457ffe03.csv")
#Shelli_unsubscribed<-read_csv("data/members_export_8c457ffe03-SHELLI list/unsubscribed_members_export_8c457ffe03.csv")

x<-RossV_subscribed %>% group_by(Email_Address) %>% tally()
x<-Shelli_subscribed %>% group_by(Email_Address) %>% tally() 

colnames(RossV_subscribed)
colnames(Shelli_subscribed) #member ratings are diff bw the two and so rbind gives some people duplicates

Shelli_subscribed$Member.Category<-NA
Shelli_subscribed$Is.Early.Career<-NA
Shelli_subscribed$Is.Student<-NA

bind_ross_shelli<-rbind(RossV_subscribed,Shelli_subscribed, by="Email_Address")

x<-bind_ross_shelli %>% group_by(Email_Address) %>% tally() 

bind_discrete <- bind_ross_shelli[!duplicated(bind_ross_shelli$Email_Address),] # make 1 row per thing

#write_csv(bind_discrete, "data output/SFS_mailchimp_merge_March2021.csv")




# October 2021 Splash -----------------------------------------------------
Aug2021_subscribed<-read_csv("data/members_export_46798ccd70_Aug2021/subscribed_members_export_46798ccd70.csv")
#Aug2021_cleanedmembers<-read_csv("data/members_export_46798ccd70_Aug2021/cleaned_members_export_46798ccd70.csv")
#Aug2021_unsubscribed<-read_csv("data/members_export_46798ccd70_Aug2021/unsubscribed_members_export_46798ccd70.csv")

Aug2021_subscribed$Member.Category <- NA
Aug2021_subscribed$Is.Early.Career <- NA
Aug2021_subscribed$Is.Student <- NA


bind_March_Aug_2021<-rbind(bind_discrete,Aug2021_subscribed, by="Email_Address")

x<-bind_March_Aug_2021 %>% group_by(Email_Address) %>% tally() 

bind_discrete_OCT <- bind_March_Aug_2021[!duplicated(bind_March_Aug_2021$Email_Address),] # make 1 row per thing

write_csv(bind_discrete_OCT, "data output/SFS_mailchimp_merge_Oct2021.csv")
#made manual fix to Oct2021 csv to delete one "Email_Email" useless row and fix the 0/1 Yes/No categories. 








