
# 1) Download CSV of all-time-views of WaterBlog stats from WordPress
# 2) CSV includes: "blog title" "views" and "URL" (but without headers; need to assign headers)
# 3) Check CSV for issues. i.e., upon export, "WaterBlog2021Wrapped" is consistently (and incorrectly) joined with "SometimesStudyingTheVariationIsTheInterestingThing" in one row; need to split these back onto 2 rows.

library(tidyverse)
library(rvest)
library(htm2txt)
library(stringr)

#read-in

blog<-read_csv("data/cws/2022-10-22 - All_Time_View_Stats_californiawaterblog.com_posts_day_10_22_2022_10_22_2022.csv")

starwars <- rvest::read_html("https://californiawaterblog.com/2022/10/16/being-patient-and-persistent-with-nature/")
starwars

#starwars <- rvest::read_html(blog$URL[1])
#starwars

url <- 'https://californiawaterblog.com/2022/10/16/being-patient-and-persistent-with-nature/'
text <- htm2txt::gettxt(url)
text<-as.data.frame(text)
text
author<-as.data.frame(stringr::word(text, 2, sep="By"))

# Try2 --------------------------------------------------------------------


library(rvest)
library(dplyr)


url <- 'https://californiawaterblog.com/2022/10/16/being-patient-and-persistent-with-nature/'
blog %>%
  mutate(text_found = purrr::map_lgl(url, ~  .x %>% read_html %>% 
                                       html_text() %>%  grepl('By', .)),
         Good_URL = !text_found)



blog %>% 
  mutate(text_found = purrr::map_lgl(url, ~  .x %>% 
                                       read_html %>% 
                                       html_text() %>%  
                                       grepl('By', .)),
         Good_URL = !text_found) 



# https://stackoverflow.com/questions/44218814/extract-data-from-text-files-using-for-loop
# https://stackoverflow.com/questions/54672672/r-loop-over-each-url-scrape-parse-extract-nodes-and-put-into-a-data-frame





