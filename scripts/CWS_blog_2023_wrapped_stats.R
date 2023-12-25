#2023 Dec - Wrapped Stats (3rd time doinga blog like this; styled more similarly to time #1) 

library(tidyverse)
library(fBasics)
options(scipen=999) #removes sci-notation






# 1) Extract Data ---------------------------------------------------------

# Go to: Wordpress > Stats > “Year” > "View all posts" > scroll to bottom; click 'export as CSV'. 
# This will download a .csv of "total views" for each blog from 1/1/YEAR to 12/31/YEAR  
# (note that "View All Posts" shows ~this~ year is highlighted in blue in the margin)
# Put this .csv into the R Projects 'Data' folder

# ! Note !  ---------------------------------------------------------------
# - Note that this CSV is not All-Blogs-Ever, as zeros not shown. e.g. in 2023 it pulled 498 of 564 blogs. So, a diff method would be needed if you want this. Potentially doable down the road.

# 2) Data Cleaning --------------------------------------------------------

# Most steps will be done in R, except 2 parts: 

# A. Every year there are always 2 blogs that mush into a single row. Locate this row and separate it out manually onto 2 unique rows.

# B. Need to manually extract AUTHOR list (see #3 below)
  # Insert COLUMN to make COLUMN A the author column (don't add headers)
  # SORT by COLUMN D (Z-A); this sorts by date within URL
  # Only do the current year for Author-Wrapped info. 
      # Paste names from WP to the Column
      # Tidy this. 
          # e.g., we want: Name, Name, Name 
          # Not: Name, Name, and Name /OR/ Name, Name and Name ) 


# Columns -----------------------------------------------------------------

# Columns that come w Wordpress export (without explicit header)
#### TITLE (blog title)
#### VIEWS (total views bw 1/1YEAR-12/31/YEAR)
#### URL (the URL)

# Columns we want to add: 

#### AUTHORS (you need to extract manually from WP to CSV)
              #Suggest just doing full author string (Name, Name, Name) 
              #& we will use R to tidy up and separate this further. 

#### DATE (we'll extract it from URL column)
#### DATECOMPARE (the date we compare DATE column to: 12/31/YEAR)
#### DAYS_TO_DEC31 (function to calc diff bw 2 date columns)
#### WEEKS_TO_DEC31 (similar to above method)
#### AVG-DAY  ( VIEWS / DAYS_TO_DEC31  ) 
#### AVG-WEEK ( VIEWS / WEEKS_TO_DEC31 )







# 2) Read In Data ---------------------------------------------------------

# Read the CSV file without column names & assign column types
a_data <- read_csv("data/cws/CaliforniaWaterBlog/2023-12-25 - californiawaterblog.com_posts_year_01_01_2023_12_31_2023.csv", 
                 col_names = FALSE, 
                 col_types = cols(
                   col_index1 = col_double(),    # Specify column type for the first column
                   col_index2 = col_character(),  # Specify column type for the second column
                   col_index3 = col_character()))   # Specify column type for the third column

colnames(a_data)

# Issue 1. no column names 
# Issue 2. not enough info (cols = blog title, total views, url); what about data, authors, and scaled stats?



# Data Tidying ------------------------------------------------------------

# Add column names
colnames(a_data) <- c("AUTHORS_ALL", "TITLE", "VIEWS", "URL") 
colnames(a_data)

# Extract dates to all blogs using URL string
b_data<-a_data %>% 
  dplyr::mutate(DATE = substr(a_data$URL, 33, 42)) %>% 
  dplyr::filter(!TITLE=="Home page / Archives"
                & !TITLE=="About") #remove this line; it's not real

# Turn the date column into date "format" 
b_data$DATE <- as.POSIXct( b_data$DATE, format="%Y/%m/%d")


# Filter to keep desireable year range
c_data<-b_data %>% 
  dplyr::filter(DATE > "2022-12-31")

# Use tidyr and dplyr functions to transform the data
d_data <- c_data %>%
  mutate(AUTHORS_UNIQUE = str_replace_all(as.character(AUTHORS_ALL), "\\s+", " "),  # Remove extra spaces
         AUTHORS_UNIQUE = str_split(AUTHORS_UNIQUE, ","), #Split at the comma
         AUTHORS_UNIQUE = map(AUTHORS_UNIQUE, str_trim)) %>% # Trim leading and trailing spaces
         unnest(AUTHORS_UNIQUE) %>%
           mutate(AUTHORS_UNIQUE = trimws(AUTHORS_UNIQUE)) 

unique(d_data$AUTHORS_UNIQUE)
x<-d_data %>% 
  distinct(AUTHORS_UNIQUE, .keep_all = FALSE) %>%  # .keep_all = TRUE keeps all columns
  arrange(AUTHORS_UNIQUE)
print(x, n=Inf)


# Clean up instances where middle initial sometimes used, sometimes not. 
# No need to make it consistent; just fix so 1 person isn't 2 uniques.
e_data<-d_data %>% 
  mutate(AUTHORS_NODUP = case_when(
    AUTHORS_UNIQUE == "Andrew L. Rypel" ~ "Andrew Rypel",
    AUTHORS_UNIQUE == "Christine A. Parisek" ~ "Christine Parisek",
    AUTHORS_UNIQUE == "Karrigan Börk" ~ "Karrigan Bork",
    AUTHORS_UNIQUE == "Peter B. Moyle" ~ "Peter Moyle",
    AUTHORS_UNIQUE == "Sophie R. Sanchez" ~ "Sophie Sanchez",
    TRUE ~ AUTHORS_UNIQUE))  # Keep the original value if no condition is met





# Stats -------------------------------------------------------------------

unique_posts <- e_data %>% 
  group_by(TITLE) %>% 
  tally() %>% 
  ungroup()

unique_authors <- e_data %>% 
  group_by(AUTHORS_NODUP) %>% 
  tally() %>% 
  ungroup()

fBasics::basicStats(e_data$VIEWS)




# wordcloud ---------------------------------------------------------------
# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# install.packages("wordcloud")
# install.packages("wordcloud2")
# install.packages("RColorBrewer")
# install.packages("tm")
# install.packages("tidytext")

library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(tidytext)#https://community.rstudio.com/t/removing-stopwords/122929/13




# Convert the text to lower case
e_data$TITLE <- tolower(e_data$TITLE)

# Remove punctuations
e_data$TITLE<-gsub("[[:punct:]]", " ", as.matrix(e_data$TITLE))

# Remove numbers
e_data$TITLE<-gsub("[[:digit:]]+", " ", as.matrix(e_data$TITLE))

#use tidytext to unnest tokens into individual works 
f_data<-e_data %>%
  tidytext::unnest_tokens(word, TITLE) %>% 
  anti_join(tidytext::get_stopwords(language = "en",source = "snowball")) #remove stopwords

# Remove whitespace
f_data$word<-gsub("[ ]", "", as.matrix(f_data$word))



dtm <- TermDocumentMatrix(f_data$word) #idk
m <- as.matrix(dtm) #turn into matrix
v <- sort(rowSums(m),decreasing=TRUE) #row sums
d <- data.frame(word = names(v),freq=v) 


?wordcloud
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=TRUE)

set.seed(1234)
wordcloud::wordcloud(words = d$word, 
                     freq = d$freq, 
                     min.freq = 2,
                     max.words=3000, 
                     random.order=FALSE,
                     random.color=FALSE, 
                     rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2")) #cant ggsave



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










