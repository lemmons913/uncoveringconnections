#Laina Emmons 7/15/2020
#sort emails w/ data by each party to determine info surrounding each

setwd("/Users/lainaemmons/Documents/GitHub/uncoveringconnections")
getwd()

partisan_data <- read.csv("data/immigration_with_partisanship.csv")
partisan_data$Date <- as.Date(partisan_data$Date, "%m/%d/%y")
partisan_data = subset(partisan_data, select = -c(Numeric.counter.for.order.in.THOMAS.data.from.93rd...110th.Congresses,
                                                  From, ICPSR.number..according.to.Poole.and.Rosenthal, Subject, 
                                                  Congress.number, Year.at.start.of.Congress, Congressional.district.number)) 

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(stringr)
library(tm)
library(widyr)
library(sentimentr)
library(igraph)
library(ggraph)

#load stopwords
data("stop_words")
immigration_stopwords <- read.csv("data/immigration_stopwords.csv")

### DEMS ###
dem_data <- partisan_data %>% filter(X1_if_dem == 1)

dem_words <- dem_data %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  anti_join(immigration_stopwords)
dem_words

count_dem_words <- dem_words %>%
  count(word, sort = TRUE)
count_dem_words

#top dem word graph
count_dem_words %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Word: Democrats")

joined_dem_words <- count_dem_words %>%
  inner_join(dem_words)
joined_dem_words

###sentiment analysis
dem_sentiment <- dem_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = FALSE) %>%
  arrange(desc(value))
dem_sentiment

dem_sentiment %>%  
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  filter(!word == c("care", "support")) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment Value of Democrats's Top Words", caption = 
         "Contribution = number of word instances * sentiment value scaled from -5 to 5", 
       y = "contribution")

###bigrams
dem_bigrams <- dem_data %>%
  unnest_tokens(bigram, Message, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#filter stopwords
dem_bigrams_filtered <- dem_bigrams %>%
  transmute(dem_bigrams, word1 = gsub('[0-9]', '', dem_bigrams$word1)) %>%
  filter(!word1 == ',') %>%
  filter(!word1 == '.')
dem_bigrams_filtered <- dem_bigrams_filtered %>%
  transmute(dem_bigrams_filtered, word2 = gsub('[0-9]', '', dem_bigrams_filtered$word2)) %>%
  filter(!word2 == ',') %>%
  filter(!word2 == '.')
dem_bigrams_filtered <- dem_bigrams_filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% immigration_stopwords$word) %>%
  filter(!word2 %in% immigration_stopwords$word)
dem_bigrams_filtered 
 
dem_bigrams_counted <- dem_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(!word1 == "administration's") %>%
  filter(!word2 == "administration's") %>%
  filter(!word1 == "") %>%
  filter(!word2 == "")
dem_bigrams_counted 

#changeable graph to display words pre/proceeding others
#change filtering word1 & word2; change the word being filtered 
dem_bigrams_counted %>%
  filter(word1 == "border") %>%
  filter(n > 5) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(word2, n, fill = word2)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free_y") + 
  coord_flip() +
  labs(title = "Words Following '___': Democrats")

dem_bigrams <- dem_bigrams_counted %>%
  unite(bigram, word1, word2, sep = " ")
dem_bigrams

dem_bigrams %>%
  top_n(15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Bigrams: Democrats")

#word web
#bigram word web
dem_web <- dem_bigrams_counted %>%
  filter(n > 60) %>%
  graph_from_data_frame()
dem_web

set.seed(2017)
dem_web %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(edge_color = state_abbr), show.legend = TRUE,
                 label_dodge = TRUE) +
  geom_node_point(color = "red") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, 
                 repel = TRUE) +
  theme_void() +
  labs(title = "  Democrat's Bigram Word Map", 
       subtitle = "  Line color = State")  

### reps ###
rep_data <- partisan_data %>% filter(X1_if_dem == 0)

rep_words <- rep_data %>%
  unnest_tokens(word, Message) %>%
  anti_join(stop_words) %>%
  anti_join(immigration_stopwords)
rep_words         

count_rep_words <- rep_words %>%
  count(word, sort = TRUE)
count_rep_words

count_rep_words %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Word: Republicans")

#sentiment
rep_sentiment <- rep_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE)
rep_sentiment

rep_sentiment %>%  
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  filter(!word == "support") %>%
  filter(!word == "care") %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment Value of Republican's Top Words", caption = 
         "Contribution = number of word instances * sentiment value scaled from -5 to 5", 
       y = "contribution")

#bigrams
rep_bigrams <- rep_data %>%
  unnest_tokens(bigram, Message, token = "ngrams", n = 2)
rep_bigrams <- rep_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

rep_bigrams_filtered <- rep_bigrams %>%
  transmute(rep_bigrams, word1 = gsub('[0-9]', '', rep_bigrams$word1)) %>%
  filter(!word1 == ',') %>%
  filter(!word1 == '.')
rep_bigrams_filtered <- rep_bigrams_filtered %>%
  transmute(rep_bigrams_filtered, word2 = gsub('[0-9]', '', rep_bigrams_filtered$word2)) %>%
  filter(!word2 == ',') %>%
  filter(!word2 == '.')
rep_bigrams_filtered <- rep_bigrams_filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% immigration_stopwords$word) %>%
  filter(!word2 %in% immigration_stopwords$word)
rep_bigrams_filtered 

rep_bigrams_counted <- rep_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(!word1 == "") %>%
  filter(!word2 == "") %>%
  inner_join(rep_bigrams_filtered)
rep_bigrams_counted

rep_bigrams_counted %>%
  filter(word1 == "border") %>%
  filter(n > 20) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(word2, n, fill = word2)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free_y") +
  coord_flip() +
  labs(title = "Words Following 'Border': Republicans")

#word web
rep_web <- rep_bigrams_counted %>%
  filter(n > 80) %>%
  graph_from_data_frame()
rep_web

set.seed(2016)
rep_web %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(edge_color = state_abbr), show.legend = TRUE,
                 label_dodge = TRUE) +
  geom_node_point(color = "red") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void() +
  labs(title = "  Republican's Bigram Word Map",
       subtitle = "  Line color = State")

### tanks ###
tank_data <- read.csv("data/immigration_emails_april2020.csv")
tank_data = subset(tank_data, select = -c(raw_text, raw_text_no_links, url_links,
                                          email, heading, immigration_term1,
                                          immigration_term2, author, group_founding))

tank_words <- tank_data %>%
  unnest_tokens(word, text_no_punct) %>%
  anti_join(stop_words) %>%
  anti_join(immigration_stopwords)
tank_words

count_tank_words <- tank_words %>%
  count(word, sort = TRUE)
count_tank_words

count_tank_words %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most Frequently Used Word: Think Tanks")

#sentiment
tank_sentiment <- tank_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE)
tank_sentiment

tank_sentiment %>%  
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  filter(!word == "illegal") %>%
  filter(!word == "support") %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment Value of Think Tanks's Top Words", caption = 
         "Contribution = number of word instances * sentiment value scaled from -5 to 5", 
       y = "contribution")

#bigrams
tank_bigrams <- tank_data %>%
  unnest_tokens(bigram, text_no_punct, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")
tank_bigrams

tank_bigrams_filtered <- tank_bigrams %>%
  transmute(tank_bigrams, word1 = gsub('[0-9]', '', tank_bigrams$word1)) 
tank_bigrams_filtered <- tank_bigrams_filtered %>%
  transmute(tank_bigrams_filtered, word2 = gsub('[0-9]', '', tank_bigrams_filtered$word2)) 
tank_bigrams_filtered <- tank_bigrams_filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% immigration_stopwords$word) %>%
  filter(!word2 %in% immigration_stopwords$word)

count_tank_bigrams <- tank_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(!word1 == "") %>%
  filter(!word2 == "") %>%
  inner_join(tank_bigrams_filtered) 
count_tank_bigrams

count_tank_bigrams %>%
  filter(word1 == "illegal") %>%
  filter(!word2 == "immigration") %>%
  top_n(15) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(word2, n, fill = word2)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free_y") +
  coord_flip() +
  labs(title = "Words Following 'Illegal': Think Tanks")

tank_bigrams <- count_tank_bigrams %>%
  unite(bigram, word1, word2, sep = " ")
tank_bigrams

tank_bigrams %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() + 
  labs(title = "Most Frequently Used Bigrams: Think Tanks") 

#word web
tank_web <- count_tank_bigrams %>%
  top_n(150) %>%
  graph_from_data_frame()
tank_web

set.seed(2017)
tank_web %>%
  ggraph(layout = "nicely") +
  geom_edge_link(aes(color = "blue"), show.legend = FALSE, label_dodge = TRUE) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void() +
  labs(title = "  Think Tank's Bigram Word Map")


