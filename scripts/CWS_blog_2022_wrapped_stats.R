


library(tidyverse)

Blog<-read_csv("data/cws/CaliforniaWaterBlog/2022-12-22 - WaterBlogAuthors2022.csv")

colnames(Blog)
x<-Blog %>% group_by(Author) %>% tally() %>% ungroup()
x<-Blog %>% group_by(BlogTitle) %>% tally() %>% ungroup()
