# Script for 2023 Dec Annual Report 


library(tidyverse)
library(fBasics)
options(scipen=999) #removes sci-notation



# Twitter -----------------------------------------------------------------

Twitter<-read_csv("data/cws/Annual Report/2023 Dec Annual Report/2023-12-00-AnnualReportStats-Twitter.csv")

colnames(Twitter)

#turn date column into date "format" 
Twitter_dated<-Twitter %>% 
  dplyr::mutate(Year = substr(Twitter_Analytics, 1, 4)) %>%   #read char 
  dplyr::mutate(Month = substr(Twitter_Analytics, 6, 7))   #read char 

Twitter_dated$Month_Year <- paste(Twitter_dated$Month, Twitter_dated$Year, sep="/")

Twitter_dated$Date <- as.Date(paste0("01/", Twitter_dated$Month_Year), format = "%d/%m/%Y")





# # of Tweets 
colnames(Twitter_dated)
Twitter_dated$NA_indicator <- is.na(Twitter_dated$Num_of_Tweets)
ggplot(Twitter_dated, aes(x = Date, y = Num_of_Tweets)) +
  #geom_ribbon(data = subset(Twitter_dated, !NA_indicator),
  #            aes(ymin = 0, ymax = Num_of_Tweets, fill = "black"), alpha = 0.2) +
  #geom_ribbon(data = subset(Twitter_dated, NA_indicator),
  #            aes(ymin = 0, ymax = 40, fill = "gray"), alpha = 0.2) +
  geom_line(aes(color = factor(!NA_indicator))) +
  scale_color_manual(values = c("black", "gray")) +
  labs(x = "Date",
       y = "Num_of_Tweets") +
  theme_minimal()


# Tweet Impressions 
colnames(Twitter_dated)
fBasics::basicStats(Twitter_dated$Tweet_Impressions)
Twitter_dated$NA_indicator <- is.na(Twitter_dated$Tweet_Impressions)
ggplot(Twitter_dated, aes(x = Date, y = Tweet_Impressions)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line()+
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "Twitter Tweet Impressions (Monthly)") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,200000, by=10000))+
  scale_x_date(breaks = seq(min(Twitter_dated$Date), max(Twitter_dated$Date), by = "6 months"),
               date_labels = "%b %Y")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14))




# Profile Visits
colnames(Twitter_dated)
fBasics::basicStats(Twitter_dated$Profile_Visits)
Twitter_dated$NA_indicator <- is.na(Twitter_dated$Profile_Visits)
ggplot(Twitter_dated, aes(x = Date, y = Profile_Visits)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line(aes(color = factor(!NA_indicator))) +
  geom_smooth(method="lm",se=FALSE)+
  scale_color_manual(values = c("cornflowerblue", "cornflowerblue")) +
  labs(x = "Date",
       y = "Profile Visits") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,8000, by=1000))+
  scale_x_date(breaks = seq(min(Twitter_dated$Date), max(Twitter_dated$Date), by = "3 months"),
               date_labels = "%b %Y",
               limits=c(as.Date("2019-07-01"), max(Twitter_dated$Date)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        legend.position = "none")

unique(Twitter_dated$Date)
x<-Twitter_dated %>%
  dplyr::filter(Date >= "2023-07-01")
fBasics::basicStats(x$Profile_Visits)
  

# Total Followers
colnames(Twitter_dated)
fBasics::basicStats(Twitter_dated$Total_Followers)
Twitter_dated$NA_indicator <- is.na(Twitter_dated$Total_Followers)
ggplot(Twitter_dated, aes(x = Date, y = Total_Followers)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  #geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "Twitter Total Followers") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,8500, by=1000),
                     limits=c(5000,8500))+
  scale_x_date(breaks = seq(min(Twitter_dated$Date), max(Twitter_dated$Date), by = "6 months"),
               date_labels = "%b %Y",
               limits=c(as.Date("2017-06-01"), max(Twitter_dated$Date)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))





# Facebook ----------------------------------------------------------------


Facebook<-read_csv("data/cws/Annual Report/2023 Dec Annual Report/2023-12-00-AnnualReportStats-Facebook.csv")

colnames(Facebook)

#turn the date column into date "format" 
FacebookDate<-Facebook %>% 
  dplyr::mutate(Year = substr(Date, 1, 4)) %>%   #read char 
  dplyr::mutate(Month = substr(Date, 6, 7))   #read char 

FacebookDate$Month_Year <- paste(FacebookDate$Month, FacebookDate$Year, sep="/")
FacebookDate$Date <- as.Date(paste0("01/", FacebookDate$Month_Year), format = "%d/%m/%Y")


# Page Reach
colnames(FacebookDate)
fBasics::basicStats(FacebookDate$Page_Reach)
ggplot(FacebookDate, aes(x = Date, y = Page_Reach)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "Facebook Page Reach (Monthly)") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,3050, by=1000),
                     limits=c(0,3050))+
  scale_x_date(breaks = seq(min(FacebookDate$Date), max(FacebookDate$Date), by = "3 months"),
               date_labels = "%b %Y",
               limits=c(as.Date("2020-07-01"), max(FacebookDate$Date)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))


# Page Visits 
colnames(FacebookDate)
fBasics::basicStats(FacebookDate$Page_Visits)
ggplot(FacebookDate, aes(x = Date, y = Page_Visits)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "Facebook Page Visits (Monthly)") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,200, by=20),
                     limits=c(0,200))+
  scale_x_date(breaks = seq(min(FacebookDate$Date), max(FacebookDate$Date), by = "3 months"),
               date_labels = "%b %Y",
               limits=c(as.Date("2020-12-01"), max(FacebookDate$Date)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))


# Total_Followers 
colnames(FacebookDate)
fBasics::basicStats(FacebookDate$Total_Followers)
ggplot(FacebookDate, aes(x = Date, y = Total_Followers)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "Facebook Total Follower Count") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,4000, by=500),
                     limits=c(0,4000))+
  scale_x_date(breaks = seq(min(FacebookDate$Date), max(FacebookDate$Date), by = "3 months"),
               date_labels = "%b %Y",
               limits=c(as.Date("2021-11-01"), max(FacebookDate$Date)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))


# Total_Likes 
colnames(FacebookDate)
fBasics::basicStats(FacebookDate$Total_Likes)
ggplot(FacebookDate, aes(x = Date, y = Total_Likes)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "Facebook Total Follower Count") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,4000, by=500),
                     limits=c(0,4000))+
  #scale_x_date(breaks = seq(min(FacebookDate$Date), max(FacebookDate$Date), by = "3 months"),
  #             date_labels = "%b %Y",
  #             limits=c(as.Date("2021-11-01"), max(FacebookDate$Date)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))



# Blog --------------------------------------------------------------------

Blog1<-read_csv("data/cws/Annual Report/2023 Dec Annual Report/2023-12-00-AnnualReportStats-Blog.csv")


#Total Likes
colnames(Blog1)
fBasics::basicStats(Blog1$Total_likes)
ggplot(Blog1, aes(x = CA_Waterblog_YEAR, y = Total_likes)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "California WaterBlog Total Likes") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,60, by=10),
                     limits=c(0,60))+
  scale_x_continuous(breaks = seq(min(Blog1$CA_Waterblog_YEAR), max(Blog1$CA_Waterblog_YEAR), by=1),
                     limits=c(2011, max(Blog1$CA_Waterblog_YEAR)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))

#Total Visitors 
colnames(Blog1)
fBasics::basicStats(Blog1$Total_Visitors)
ggplot(Blog1, aes(x = CA_Waterblog_YEAR, y = Total_Visitors)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "California WaterBlog Total Visitors") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,220000, by=20000),
                     limits=c(0,220000))+
  scale_x_continuous(breaks = seq(min(Blog1$CA_Waterblog_YEAR), max(Blog1$CA_Waterblog_YEAR), by=1),
                     limits=c(2012, max(Blog1$CA_Waterblog_YEAR)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))



#Total Visitors 
colnames(Blog1)
fBasics::basicStats(Blog1$Total_Views...14)
ggplot(Blog1, aes(x = CA_Waterblog_YEAR, y = Total_Views...14)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "California WaterBlog Total Views (Annually)") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,330000, by=30000),
                     limits=c(0,330000))+
  scale_x_continuous(breaks = seq(min(Blog1$CA_Waterblog_YEAR), max(Blog1$CA_Waterblog_YEAR), by=1),
                     limits=c(2011, max(Blog1$CA_Waterblog_YEAR)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))








Blog2<-read_csv("data/cws/Annual Report/2023 Dec Annual Report/2023-12-00-WordpressTotalViews.csv")

colnames(Blog2)
unique(Blog2$Month)

TotalViews <- Blog2 %>% 
  dplyr::filter(!Month==c("Total_Views","Cumulative_Views"))


# Create date column that's plotable 
TotalViews$month_num <- match(tolower(TotalViews$Month), tolower(month.abb))
TotalViews$Date <- as.Date(paste(TotalViews$month_num, TotalViews$Year, "01", sep = "/"), format = "%m/%Y/%d")



#Total Views by month 
colnames(TotalViews)
fBasics::basicStats(TotalViews$Total_Views)
ggplot(TotalViews, aes(x = Date, y = Total_Views)) +
  geom_point(size=3,alpha=0.5,color="cornflowerblue")+
  geom_line() +
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Date",
       y = "California WaterBlog Total Views (Monthly)") +
  scale_y_continuous(labels = scales::comma_format(), breaks=seq(0,110000, by=10000),
                     limits=c(0,110000))+
  scale_x_date(
    breaks = seq(min(TotalViews$Date, na.rm = TRUE), 
                 max(TotalViews$Date, na.rm = TRUE), 
                 by = "12 months"),
    date_labels = "%b %Y",
    limits = c(as.Date("2010-01-01"), max(TotalViews$Date, na.rm = TRUE)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        title = element_text(size=13))






