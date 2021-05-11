
library(tidyverse)

RossV_cleanedmembers<-read_csv("data/members_export_b6300f8dad-ROSS list/cleaned_members_export_b6300f8dad.csv")
RossV_subscribed<-read_csv("data/members_export_b6300f8dad-ROSS list/subscribed_members_export_b6300f8dad.csv")
RossV_unsubscribed<-read_csv("data/members_export_b6300f8dad-ROSS list/unsubscribed_members_export_b6300f8dad.csv")

Shelli_cleanedmembers<-read_csv("data/members_export_8c457ffe03-SHELLI list/cleaned_members_export_8c457ffe03.csv")
Shelli_subscribed<-read_csv("data/members_export_8c457ffe03-SHELLI list/subscribed_members_export_8c457ffe03.csv")
Shelli_unsubscribed<-read_csv("data/members_export_8c457ffe03-SHELLI list/unsubscribed_members_export_8c457ffe03.csv")

unique(RossV_cleanedmembers$`Email Address`)
unique(RossV_subscribed$`Email Address`)
unique(RossV_unsubscribed$`Email Address`)

unique(Shelli_cleanedmembers$`Email Address`)
unique(Shelli_subscribed$`Email Address`)
unique(Shelli_unsubscribed$`Email Address`)



join_Ross<-anti_join(RossV_subscribed,RossV_cleanedmembers, by="Email Address")

join_Shelli<-anti_join(Shelli_subscribed,Shelli_cleanedmembers, by="Email Address")

join<-left_join(join_Ross,join_Shelli,by="Email Address")

unique(join$`Email Address`)

#ReduceDups <- join[!duplicated(join$"Email Address"),] # make 1 row per reservoir


colnames(join)




write_csv(join, "data output/SFS_mailchimp_merge_March2021.csv")
