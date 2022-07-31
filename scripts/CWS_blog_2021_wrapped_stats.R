#cws 2021 wrapped blog stats (some)



library(tidyverse)

cwsauthor<-read_csv("data/cws/2021-authors-californiawaterblog.com_posts_year_01_01_2021_12_31_2021.csv")

#a<- cwsauthor %>% group_by(Authors) %>% tally(sort=TRUE) # tally check

extract<-cwsauthor %>%  
  mutate(Name_Plain = (sub(" et al.*", "", cwsauthor$Authors)))# Extract characters before pattern

a<- extract %>% 
  group_by(Name_Plain) %>% 
  tally(sort=TRUE)) # tally check

b<-extract %>% 
  group_by(Name_Plain) %>% 
  mutate(n= n()) %>% 
  select("Name_Plain","n") %>% 
  distinct(Name_Plain, .keep_all=TRUE)


write_csv(b,"data output/CWS_2021_wrapped_blog_author_tally.csv")


cwsblog<-read_csv("data/cws/2021-blogs-californiawaterblog.com_posts_year_01_01_2021_12_31_2021.csv")

c<- cwsblog %>% 
  group_by(First_Author) %>% 
  tally(sort=TRUE) # tally check


write_csv(c,"data output/CWS_2021_wrapped_blog_FIRSTauthor_tally.csv")
  