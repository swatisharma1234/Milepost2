# Milepost2
# Code to do Sentiment Analysis from the file "1001_nights_analects.csv" which contains text from
# book "THOUSAND NIGHTS AND A NIGHT" and "Analects by Confucius"
# Downloading the data from GitHub, saving a copy in working directory and reading in R

download.file("https://raw.githubusercontent.com/skhan890/GIT-tutorial/master/1001_nights_analects.csv",destfile= "1001_nights_analects.csv", mode = "wb")
nights_analects<- read.csv("1001_nights_analects.csv", header = TRUE, stringsAsFactors = FALSE)

# Loading the required packages
library(tidytext)
library(tidyverse)
library(ggplot2)

# Looking at the number of lines in each book
nights_analects %>% 
  count(title, type)

# Un-nesting the lines in both the books into words

tidy_nights_analects <- nights_analects %>%
  group_by(title) %>% 
  mutate(line=row_number()) %>% 
  unnest_tokens(word,text) %>% 
  ungroup()
tidy_nights_analects
view(tidy_nights_analects)

# Counting and sorting the words in the dataset
tidy_nights_analects_sorted <- tidy_nights_analects %>% 
  count(word, sort=TRUE)
View(tidy_nights_analects_sorted)

# Adding sentiment for the dataset vy title and type using "bing"
sentinment_nights_analects <- tidy_nights_analects %>% 
  inner_join(get_sentiments("bing"), c=("word"))%>%
  count(title, type, sentiment)
View(sentinment_nights_analects)

# Adding sentiment for the words in the dataset vy title and type using "bing"
word_nights_analects<- tidy_nights_analects %>% 
  inner_join(get_sentiments("bing"), c=("word"))%>%
  count(word, sentiment, sort=TRUE)
View(word_nights_analects)

# Looking at Top 10 words for both positive and negative sentiments in the dataset
top_word <-word_nights_analects %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word=reorder(word,n)) 
View(top_word)

# Plotting the graph
plot1<- ggplot(top_word, aes(x=word,y=n, fill=sentiment))+
  geom_col(show.legend = FALSE)+labs(title="Sentiment Analysis for books The Thousand Nights And A Night and Analects by Confucius", x="Word", y="Number of occurences")+
  #improve display
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()
plot1
