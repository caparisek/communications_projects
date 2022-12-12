
# 1) Download CSV of all-time-views of WaterBlog stats from WordPress
# 2) CSV includes: "blog title" "views" and "URL" (but without headers; need to assign headers)
# 3) Check CSV for issues. i.e., upon export, "WaterBlog2021Wrapped" is consistently (and incorrectly) joined with "SometimesStudyingTheVariationIsTheInterestingThing" in one row; need to split these back onto 2 rows.

library(tidyverse)
library(rvest)
library(htm2txt)
library(stringr)
library(xml2)

blog1<-read_csv("data/cws/2022-10-22 - All_Time_View_Stats_californiawaterblog.com_posts_day_10_22_2022_10_22_2022.csv")
colnames(blog1)

#add dates to all blogs using URL string
blog2<-blog1 %>% 
  dplyr::mutate(date = substr(blog1$URL, 33, 42))

#turn the date column into date "format" 
blog2$date <- as.POSIXct( blog2$date, format="%Y/%m/%d" )
print( blog2 )
print( class(blog2$date) )

#create column for author-name
blog3<-blog2 



#extract author in 2 different ways. end up taking body-text after BY. (prob same accuracy both ways.) 
for(i in 1:nrow(blog3)){
  # Read in the URL
  page <- read_html(blog3$URL[i])
  # See if there is text to be extracted from this xpath and the content attribute
  author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
    html_attr("content")
  if(is.na(author_text)){
    author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
      html_text() 
  } 
  blog3$Author[i] <- author_text
}

sum(is.na(blog3$Author))

blog3[2,5]

nchar(blog3[2,5], type = "chars", allowNA = FALSE, keepNA = NA)

#
# this does similar/same to for-loop above. 
#
for(i in 1:nrow(blog3)){
  # Read in the URL
  page <- read_html(blog3$URL[i])
  # See if there is text to be extracted from this xpath and the content attribute
  author_text2 <- as.data.frame(stringr::word(page, 2, sep="By"))
}














# Trying Something 1 --------------------------------------------------------------------

starwars <- rvest::read_html("https://californiawaterblog.com/2022/10/16/being-patient-and-persistent-with-nature/")
starwars
url <- 'https://californiawaterblog.com/2022/10/16/being-patient-and-persistent-with-nature/'
text <- htm2txt::gettxt(url)
text<-as.data.frame(text)
text
author<-as.data.frame(stringr::word(text, 2, sep="By"))

# Trying Something 2 --------------------------------------------------------------------

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




# Outtakes ----------------------------------------------------------------

# Advice from Liza
# (this works to grab 1st 3 words (in some cases) BUT author list is longer than 3 words (in most cases))
# so I did not use this
#
#   for(i in 1:nrow(blog)){
#     # Read in the URL
#     page <- read_html(blog$URL[i])
#     # See if there is text to be extracted from this xpath and the content attribute
#     author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
#       html_attr("content") %>% 
#       # If so, this says take the text from content and then three words( (\\w+\\s){3} )
#       # that come after some version of By ( [Bb]y:?\\s? means one of By:, by:, By, and by )
#       # and the (?<=...) part is a 'lookaround' to tell stringr to look for the text after the BY part
#       str_extract(., "(?<=[Bb]y:?\\s?)(\\w+\\s){3}")
#     # But, if that xpath worked but it is in the text and not in that content attribute, try to just
#     # extract the text using this alternative 
#     if(is.na(author_text)){
#       author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
#         html_text() %>% 
#         # Same regex expression to grab 3 words
#         str_extract(., '(?<=[Bb]y:?\\s?)(\\w+\\s){3}')
#     } 
#     blog$Author[i] <- author_text
#   }
#
#snippet of the above; still takes just 3 words: 
#for(i in 1:nrow(blog3)){
#  # Read in the URL
#  page <- read_html(blog3$URL[i])
#  # See if there is text to be extracted from this xpath and the content attribute
#  author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
#    html_attr("content") %>% 
#    str_extract(., '(?<=[Bb]y:?\\s?)(\\w+\\s){3}') #takes just 3 words
#  blog3$Author[i] <- author_text
#}
#sum(is.na(blog3$Author))



