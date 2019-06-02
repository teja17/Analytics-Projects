getwd()
setwd("C:\\Users\\Tejaswini\\Desktop\\Advanced Business Analytics\\Assignment")

#install.packages("data.table", repos = "https://cran.r-project.org")

# Load the data set
library(data.table)
data <- fread("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

#number of columns
colnames <- colnames(data)
colnames

#number of rows
nrow(data)
head(data)

#tokenize q_content column data
#install.packages("tidytext", repos = "https://cran.r-project.org")
#install.packages("dplyr", repos = "https://cran.r-project.org")
library(dplyr)
text_df <- data_frame(line = 1:8360, text = data$q_content)
head(text_df)

#install.packages("backports")
library(tidytext)
#tokenize column "q_content" by each word
tidy_text = text_df %>%
  unnest_tokens(word, text)

#tidy_text
#remove stop words
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)
#count the number of tokens
tidy_text %>%
  count(word, sort = TRUE) 
#create visualization using ggplot2
library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#install.packages("SnowballC", repos = "https://cran.r-project.org")
# stem q_content using SnowballC
library(SnowballC)
tidy_text <- data %>%
  unnest_tokens(word, q_content) %>%
  mutate(word = wordStem(word)) 

#remove stop words
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)
#count the number of tokens
tidy_text %>%
  count(word, sort = TRUE) 
#create visualization using ggplot
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Load wordcloud package
library(wordcloud)
tidy_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#Create a color-coded word cloud based on sentiment
#install.packages("reshape2", repos = "https://cran.r-project.org")
library(reshape2)
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

###Computing word count for column answers
text_df <- data_frame(line = 1:8360, text = data$answers)
head(text_df)
#tokenize column "answers" by each word
tidy_text = text_df %>%
  unnest_tokens(word, text)

#tidy_text
#remove stop words
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)
#count the number of tokens
tidy_text %>%
  count(word, sort = TRUE) 
#create visualization using ggplot2
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()
# stemming using SnowballC
tidy_text <- data %>%
  unnest_tokens(word, answers) %>%
  mutate(word = wordStem(word)) 
# remove stop words
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)
# count the number of tokens
tidy_text %>%
  count(word, sort = TRUE) 
#create visualization using ggplot
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#create word cloud for 200 words
tidy_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#Create a color-coded word cloud based on sentiment
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

####Topic Modelling
#install required packages
#install.packages("RTextTools", repos = "https://cran.r-project.org")
#install.packages("tm", repos = "https://cran.r-project.org")
#install.packages("topicmodels", repos = "https://cran.r-project.org")
#install.packages("slam", repos = "https://cran.r-project.org")

#Load packages
library(RTextTools)
library(tm)
library(topicmodels)
library(slam)

#topic modeling for k=5 q_content column

data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.
lda_td <- tidy(lda) #converting to tidy format

#create bar plot for top 10 tokens
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#topic modeling for k=2 q_content column
lda_2 <- LDA(dtm.new, k = 2) # k is the number of topics to be found.
lda_td2 <- tidy(lda_2) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_2 <- lda_td2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#topic modeling for k=3 q_content column
lda_3 <- LDA(dtm.new, k = 3) # k is the number of topics to be found.
lda_td3 <- tidy(lda_3) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_3 <- lda_td3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_3 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#topic modeling for k=4 q_content column
lda_4 <- LDA(dtm.new, k = 4) # k is the number of topics to be found.
lda_td4 <- tidy(lda_4) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_4 <- lda_td4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_4 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#topic modeling for k=10 q_content column
lda_10 <- LDA(dtm.new, k = 10) # k is the number of topics to be found.
lda_td10 <- tidy(lda_10) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_10 <- lda_td10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_10 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##to perform topic-modeling on answers column
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm, 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda_a_10 <- LDA(dtm.new, k = 10) # k is the number of topics to be found

#topic modeling for k=10 (answers column)
lda_tda10 <- tidy(lda_a_10) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_a_10 <- lda_tda10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_a_10 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#topic modeling for k=2 (answers column)
lda_a_2 <- LDA(dtm.new, k = 2) # k is the number of topics to be found.
lda_tda2 <- tidy(lda_a_2) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_a_2 <- lda_tda2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_a_2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#topic modeling for k=8 (answers column)
lda_a_8 <- LDA(dtm.new, k = 8) # k is the number of topics to be found.
lda_tda8 <- tidy(lda_a_8) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_a_8 <- lda_tda8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_a_8 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#topic modeling for k=11 (answers column)
lda_a_11 <- LDA(dtm.new, k = 11) # k is the number of topics to be found.
lda_tda11 <- tidy(lda_a_11) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_a_11 <- lda_tda11 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_a_11 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#topic modeling for k=14 (answers column)
lda_a_14 <- LDA(dtm.new, k = 14) # k is the number of topics to be found.
lda_tda14 <- tidy(lda_a_14) #converting to tidy format
#create bar plot for top 10 tokens
top_terms_a_14 <- lda_tda14 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_a_14 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

