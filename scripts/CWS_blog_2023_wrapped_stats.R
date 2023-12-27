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
#### YEAR_END (the date we compare DATE column to: 12/31/YEAR)
#### DAYS_TO_YEAR_END (function to calc diff bw 2 date columns)
#### WEEKS_TO_YEAR_END (similar to above method)
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
# Issue 3. some blogs are reposts; should they count toward "uniques"? currently they do. 


# Data Tidying ------------------------------------------------------------

# Add column names
colnames(a_data) <- c("AUTHORS_ALL", "TITLE", "VIEWS", "URL") 
colnames(a_data)

# Extract dates to all blogs using URL string
b_data<-a_data %>% 
  dplyr::mutate(DATE = substr(a_data$URL, 33, 42)) %>% 
  dplyr::filter(!TITLE=="Home page / Archives"
                & !TITLE=="About") #remove this line; it's not real




# Dates and Rates ---------------------------------------------------------

# Add the end-date column
b_data$YEAR_END <- "2023/12/31" # character column to compare dates to

# Turn the date column into date "format" 
b_data$DATE <- as.POSIXct( b_data$DATE, format="%Y/%m/%d")
b_data$YEAR_END <- as.POSIXct( b_data$YEAR_END, format="%Y/%m/%d")


# Calculate days until Dec31
b_data$DAYS_UNTIL_YEAR_END <- as.numeric(difftime(b_data$YEAR_END, b_data$DATE, units = "days"))

# Calculate weeks until Dec31
b_data$WEEKS_UNTIL_YEAR_END <- as.numeric(difftime(b_data$YEAR_END, b_data$DATE, units = "weeks"))

# Calculate average views per day for each blog (views/age)
c_data<-b_data %>% 
  # Calculate average views per day for each blog (views/age) « bit skewed
  mutate(AVG_VIEWS_PER_DAY  = (VIEWS / DAYS_UNTIL_YEAR_END)) %>% 
  mutate(AVG_VIEWS_PER_WEEK = (VIEWS / WEEKS_UNTIL_YEAR_END)) %>% 
  # Calculate the normalized views based on the age of the blog
  mutate(NORM_VIEWS_DAYS =  (VIEWS / (365 - DAYS_UNTIL_YEAR_END))) %>% 
  mutate(NORM_VIEWS_WEEKS = (VIEWS * (52 - WEEKS_UNTIL_YEAR_END))) %>% 
  dplyr::filter(DATE > "2022-12-31")

write_csv(c_data, "data output/2023_CWS_Wrapped_BlogStats.csv")







# Clock chart -------------------------------------------------------------

library(zoo)

# Create the circular chart
c_data %>% 
  ggplot(aes(x = as.numeric(DATE), y = (log(VIEWS)))) +
  #geom_line(aes(group = 1), color = "blue") +
  #geom_point(color = "cornflowerblue", size = 2, alpha=0.8) +
  geom_bar(width = 0.9, position = "fill") +
  scale_x_continuous(labels = function(x) format(as.Date(x, origin = "2023-01-01"), "%b %d"),
                     breaks = seq(min(c_data$DATE), max(c_data$DATE), by = "1 month")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        axis.text.y = element_blank())+
  coord_polar(start = 0, clip="off")


fBasics::basicStats(c_data$VIEWS)
p<-c_data %>% 
  ggplot(aes(x = as.numeric(DATE), y = (log(VIEWS)))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "cornflowerblue",alpha=0.4)+
  geom_hline(yintercept = log(1211), linetype = "dotted", color = "blue4", size = 0.5) +
  geom_segment(aes(x = as.numeric(DATE), xend = as.numeric(DATE), 
                   y = 0, yend = (log(VIEWS))), 
               color = "cornflowerblue", size = 2) +
  geom_point(color = "blue", size = 7, alpha = 0.9) +
  scale_x_continuous(labels = function(x) format(as.Date(x, origin = "2023-01-01"), "%b"),
                     breaks = seq(min(c_data$DATE), max(c_data$DATE), by = "1 month")) +
  geom_hline(yintercept = log(1211), linetype = "dashed", color = "blue4", size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0,face="bold",size=15,color="blue4"),
        #axis.text.x = element_blank(),
        #plot.background = element_rect(fill="white"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        #panel.grid.major=element_blank(),
        panel.grid.major = element_line(color = "cornflowerblue", linetype="dotted"),
        #panel.grid.minor=element_blank(),
        panel.grid.minor = element_line(color = "cornflowerblue", linetype = "dotted"),
        plot.margin = margin(0, 0, 0, 0, unit = "cm"))+
  coord_polar(start = 0)

# Save the ggplot as a PNG file with DPI set to 600
ggsave("figures/clock_plot.png", plot = p, width = 8, height = 6, units = "in", dpi = 600)




# Author info -------------------------------------------------------------

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




# Filter Year -------------------------------------------------------------

f_data<-e_data %>% 
  dplyr::filter(DATE > "2022-12-31")

# Stats -------------------------------------------------------------------

unique_posts <- f_data %>% 
  group_by(TITLE) %>% 
  tally() %>% 
  ungroup()

unique_authors <- f_data %>% 
  group_by(AUTHORS_NODUP) %>% 
  tally() %>% 
  ungroup()

fBasics::basicStats(f_data$VIEWS)




# wordcloud ---------------------------------------------------------------
# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(tidytext)
library(SnowballC)

# Preprocess the text data
titles <- e_data$TITLE
titles <- tolower(titles)
titles <- removePunctuation(titles)
titles <- removeNumbers(titles)
titles <- removeWords(titles, stopwords("en"))
titles <- stripWhitespace(titles)
titles <- titles[titles != ""] #Filter out empty strings or whitespace
text <- unlist(strsplit(titles, " ")) #Combine the processed text into a single document
text <- text[text != ""] #Remove any remaining empty strings
word_freq_df <- data.frame(word = text, stringsAsFactors = FALSE) #Convert the 'text' vector into a data frame
word_freq_table <- table(word_freq_df$word) #Create a tally of word frequencies
word_freq_df <- data.frame(word = names(word_freq_table), frequency = as.numeric(word_freq_table)) #Table>DF

textdata<-word_freq_df %>% 
  dplyr::filter(!word=="s") #tidy up more

display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=TRUE)

set.seed(1234) #set.seed or it will keep changing

wordcloud::wordcloud(words = textdata$word, 
                     freq = textdata$freq, 
                     min.freq = 1,
                     max.words=3000, 
                     random.order=FALSE,
                     random.color=FALSE, 
                     rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2")) #cant ggsave

wordcloud::wordcloud(words = textdata$word, 
                     freq = textdata$freq, 
                     min.freq = 1,
                     max.words=3000, 
                     random.order=FALSE,
                     random.color=FALSE, 
                     rot.per=0.32, 
                     colors=brewer.pal(8, "Dark2")) #cant ggsave


citation("wordcloud")


wordcloud2::wordcloud2(data=textdata, 
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



