
# 1) Download CSV of all-time-views of WaterBlog stats from WordPress
# 2) CSV includes: "blog title" "views" and "URL" (but without headers; need to assign headers)
# 3) Check CSV for issues. i.e., upon export, "WaterBlog2021Wrapped" is consistently (and incorrectly) joined with "SometimesStudyingTheVariationIsTheInterestingThing" in one row; need to split these back onto 2 rows.

library(tidyverse)
library(rvest)
library(htm2txt)
library(stringr)
library(xml2)

#read in data downloaded from WordPress stats page
blog1<-read_csv("data/cws/2022-12-22 - All_Time_View_Stats_californiawaterblog.com_posts_day_12_22_2022_12_22_2022.csv")

#add dates to all blogs using URL string
blog2<-blog1 %>% 
  dplyr::mutate(date = substr(blog1$URL, 33, 42))

#turn the date column into date "format" 
blog2$date <- as.POSIXct( blog2$date, format="%Y/%m/%d" )
print(blog2)
class(blog2$date)



x<-blog2 %>% 
  dplyr::filter(date>"2021-12-31")
  

blog2<-x

# wordcloud ---------------------------------------------------------------
# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(tidytext)#https://community.rstudio.com/t/removing-stopwords/122929/13




# Convert the text to lower case
blog2$BlogTitle <- tolower(blog2$BlogTitle)
# Remove punctuations
blog2$BlogTitle<-gsub("[[:punct:]]", " ", as.matrix(blog2$BlogTitle))
# Remove numbers
blog2$BlogTitle<-gsub("[[:digit:]]+", " ", as.matrix(blog2$BlogTitle))
#use tidytext to unnest tokens into individual works 
blog3<-blog2 %>%
  tidytext::unnest_tokens(word, BlogTitle) %>% 
  anti_join(tidytext::get_stopwords(language = "en",source = "snowball")) #remove stopwords
# Remove whitespace
blog3$word<-gsub("[ ]", "", as.matrix(blog3$word))



dtm <- TermDocumentMatrix(blog3$word) #idk
m <- as.matrix(dtm) #turn into matrix
v <- sort(rowSums(m),decreasing=TRUE) #row sums
d <- data.frame(word = names(v),freq=v) 
#set.seed(1234)
wordcloud::wordcloud(words = d$word, 
                     freq = d$freq, 
                     min.freq = 1,
                     max.words=900, 
                     random.order=FALSE,
                     random.color=TRUE, 
                     rot.per=0.35, 
                     colors=brewer.pal(6, "Dark2")) #cant ggsave

?wordcloud2
set.seed(2)
wordcloud2::wordcloud2(data=d, 
                       size = 1, 
                       minSize = 0, 
                       gridSize =  1,
                       fontFamily = 'Helvetica', 
                       fontWeight = 'normal',
                       color = brewer.pal(8, "Dark2"), 
                       backgroundColor = "white",
                       minRotation = -pi/4, 
                       maxRotation = pi/4, 
                       shuffle = FALSE,
                       rotateRatio = 0.4, 
                       shape = 'circle', 
                       ellipticity = 0.75,
                       widgetsize = NULL, 
                       figPath = NULL, 
                       hoverFunction = NULL)

# library(htmlwidgets)                
# ?saveW
# saveWidget(x, file="mywordcloud.html")
# library(webshot)
# ??webshot
# webshot(
#   url = "mywordcloud.html",
#   file = "figures/myFigure.jpeg", 
#   delay = 6, 
#   vwidth = 500, 
#   vheight = 500,
#   selector = '#canvas')
# 

?wordcloud
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=TRUE)


#kinda worked; not a fan 
#dtm <- TermDocumentMatrix(blog2) #idk
#m <- as.matrix(dtm) #turn into matrix
#v <- sort(rowSums(m),decreasing=TRUE) #row sums
#d <- data.frame(word = names(v),freq=v) 
#set.seed(1234)
#wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#          max.words=900, random.order=FALSE, rot.per=0.35, 
#          colors=brewer.pal(8, "Dark2"))







# create column for author-name -------------------------------------------

#extract author in 2 different ways. end up taking body-text after BY. (prob same accuracy both ways.) 
for(i in 1:nrow(blog3)){
  # Read in the URL
  page <- read_html(blog3$URL[i])
  # See if there is text to be extracted from this xpath and the content attribute
  author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
    html_attr("content")
  if(is.na(author_text)){
    author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
      html_text() } 
  blog3$Author[i] <- author_text}


#snippet of the above; still takes just 3 words: 
for(i in 1:nrow(blog3)){
  # Read in the URL
  page <- read_html(blog3$URL[i])
  # See if there is text to be extracted from this xpath and the content attribute
  author_text <- xml_find_all(page, '/html/head/meta[4]') %>% 
    html_attr("content") %>% 
    str_extract(., '(?<=[Bb]y:?\\s?)(\\w+\\s){3}') #takes just 3 words
  blog3$Author[i] <- author_text
}



# this does similar/same to for-loop above. 
for(i in 1:nrow(blog3)){
  # Read in the URL
  page <- read_html(blog3$URL[i])
  # See if there is text to be extracted from this xpath and the content attribute
  author_text2 <- as.data.frame(stringr::word(page, 2, sep="By"))
  }



#note the issue that it took only some words
sum(is.na(blog3$Author))
blog3[2,5]
nchar(blog3[2,5], type = "chars", allowNA = FALSE, keepNA = NA)











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



