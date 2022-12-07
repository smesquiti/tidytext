#4 Relationships between words: n-grams and correlations

library(pacman)
pacman::p_load(tidyverse, tidytext,janeaustenr, igraph, ggraph, widyr, job, install = T)

#let's start to look at ngrams

#Tokenize by n-gram 

austen_bigrams <- austen_books() %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%  #saying we want to tokenize bigrams
  filter(!is.na(bigram)) #filtering out stuff that isnt a bi-gram

#now let's count up the bigrams 

austen_bigrams  %>% #gives us the most common bigrams 
  count(bigram, sort = T)

#we meed to separate them out now 
bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c('word1','word2'), sep = ' ') #this is built into tidyR. we are specifiying how we want this separated 

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% #taking out stop words
  filter(!word2 %in% stop_words$word)#taking out stop words

#get our new bigram counts 
bigram_counts <- bigrams_filtered %>% 
  count(word1,word2,sort = T)
bigram_counts
#We can see that names (whether first and last or with a salutation) are the most common pairs in Jane Austen books.

#In other analyses, we may want to work with the recombined words. tidyr’s unite() function is the inverse of separate(), 
#and lets us recombine the columns into one.

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")#recombine them 

bigrams_united


#let's look at trigrams 

austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  filter(!is.na(trigram)) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
#and now we can see how it is all structured 

#we can also use this approach to see things like what are the most common streets mentioned in jane's work
bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = T)

#A bigram can also be treated as a term in a document in the same way that we treated individual words. 
#For example, we can look at the tf-idf (Chapter 3) of bigrams across Austen novels.
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

#visualize it 
bigram_tf_idf %>%
  group_by(book) %>% #group by book 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) + #need to change word to bigram, trigram, etc.
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") + #facet wrap by book 
  labs(x = "tf-idf", y = NULL)


#using bigrams to do sentiment analysis 

#this gives us context when doing sentiment analysis (e.g., the work like can be used for reference or emotion)
bigrams_separated %>% 
  filter(word1 == 'not') %>% #we want to see how often *not*(word1) happy comes up
  count(word1, word2, sort = T)

#let's use the AFINN lexicon to get the sentiment 
AFINN <- get_sentiments("afinn")
AFINN

#we can now look at the words that were preceded by not and were associated with sentiment 
not_words <- bigrams_separated %>% 
  filter(word1 =='not') %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, value, sort = T)
not_words
#For example, the most common sentiment-associated word to follow “not” was “like”, which would normally have a (positive) score of 2.

#It’s worth asking which words contributed the most in the “wrong” direction. 
#To compute that, we can multiply their value by the number of times they appear 
#(so that a word with a value of +3 occurring 10 times has as much impact as a word with a sentiment value of +1 occurring 30 times).

not_words %>%
  mutate(contribution = n * value) %>% #multiply their value by number of occurances 
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = F) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

#“Not” isn’t the only term that provides some context for the following word. 
#We could pick four common words (or more) that negate the subsequent term, 
#and use the same joining and counting approach to examine all of them at once.

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>% #taking out word1 in the bigram if the first word is in our list of negation words
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
negated_words

#we can then visualize this for each negation word 
negated_words%>%
  mutate(contribution = n * value) %>% #multiply their value by number of occurances 
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = F) +
  facet_wrap(~word1, ncol = 2, nrow = 2, scales = "free") + #facet wrap by book 
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"negation term\"")

#visualizing word networks

#network analysis
#One way to create an igraph object from tidy data is the graph_from_data_frame() function, 
#which takes a data frame of edges with columns for “from”, “to”, and edge attributes (in this case n
bigram_counts

#now only select relatively common co-occurences 
bigram_graph <- bigram_counts %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

bigram_graph

#use ggraph to visualize this 
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


#let's polish this up some 
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#let's do this with other texts 

#build functions 

#count bigrams 
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% # tokenize 
    separate(bigram, c("word1", "word2"), sep = " ") %>% #separate out 
    filter(!word1 %in% stop_words$word, #stop words 
           !word2 %in% stop_words$word) %>% #stop words 
    count(word1, word2, sort = TRUE) #count 
}

#visualize bigrams 

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "dodgerblue4", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


#let's try this with the king james version of the bible 
kjv <- gutenberg_download(10)

kjv_bigrams <- kjv %>% 
  count_bigrams()

#filter out rare occurences 
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams() #graph it

#count and correlate between sections 

#let's see what words appear in each of the sections 
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) #keep if the word is not a stop word in our stop word list
austen_section_words

#One useful function from widyr is the pairwise_count() function. 
#The prefix pairwise_ means it will result in one row for each pair of words in the word variable. 
#This lets us count common pairs of words co-appearing within the same section:
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs

#Notice that while the input had one row for each pair of a document (a 10-line section) and a word, 
#the output has one row for each pair of words. 
#This is also a tidy format, but of a very different structure that we can use to answer new questions.


#now run a correlation 
#but first we need to filter 
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>% #filter 
  pairwise_cor(word, section, sort = TRUE) #pairwise correlation 
word_cors

#This output format is helpful for exploration. For example, we could find the words most correlated with a word like “pounds”
#using a filter operation.

word_cors %>%
  filter(item1 == "pounds")

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>% #want to see words that co-occur with this list
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>% #top 6 words
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

#Just as we used ggraph to visualize bigrams, we can use it to visualize the correlations and clusters of 
#words that were found by the widyr package

word_cors %>%
  filter(correlation > .15) %>% #filter out week correlations
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


#now apply this to the CEO dataset 
df <- read.csv("Tech_CEOS copy.csv") 
#get rid of the LIWC output :(
df <- df[, c(1:6)] #we trimmed it down 

#tokenize the data 
df_bigrams <- df %>% 
  unnest_tokens(bigram, Text, token = 'ngrams', n = 2) %>% 
  filter(!is.na(bigram))

#count up the bigrams 
df_bigrams %>%
  count(bigram, sort = T)

#separate the data 
df_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")


#filter out stop words 
df_filtered <- df_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#get new bigram counts 
df_filtered %>% 
  count(word1, word2, sort = T)


#let's unite them back together 
df_united <- df_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#we can also look at trigrams, even though this isn't as useful 
df %>%
  unnest_tokens(trigram, Text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

#let's filter to see what words co-occur with cloud

df_filtered %>% 
  filter(word2 == "cloud") %>% 
  count(Speaker, word1, sort = T) #sort by speaker

#we can also look at tf-df or the uniqueness of words 
df_tf_idf <- df_united %>%
  filter(Speaker == 'Tim Cook'| Speaker == "Jeff Bezos"| Speaker == " Jack Dorsey"|Speaker == "Elon Musk") %>% 
  count(Speaker, bigram) %>%
  bind_tf_idf(bigram, Speaker, n) %>% #sorting by speaker
  arrange(desc(tf_idf))
df_tf_idf


#let's graph this 
df_tf_idf %>%
  group_by(Speaker) %>% #group by Speaker 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Speaker)) + #need to change word to bigram, trigram, etc.
  geom_col(show.legend = FALSE) +
  facet_wrap(~Speaker, scales = "free") + #facet wrap by Speaker 
  labs(x = "tf-idf", y = NULL)

#let's map this to sentiment analysis now 
df_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

#load in sentiments 
AFINN <- get_sentiments("afinn")
AFINN

#looking at sentiments that don't include not
not_words <-  df_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)
not_words


#lets graph this 
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")


#incorporate more words we wanna see that might influence sentiment 
negation_words <- c("not", "no", "never", "without")

negated_words <- df_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

#negated_words
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word1, scales = "free") + #facet wrap by Speaker 
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")


#lets do some network analysis now 

#we need to restructure the data first

df_bigrams_count <- df_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- df_bigrams_count %>%
  filter(n > 20) %>%
  graph_from_data_frame()



set.seed(2017)

#rough network analysis 
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

#more refined
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
#################################
#just out of curiosity we are going to do this with the manuscript data
setwd("~/Desktop/CEO Project") #set WD to where our files live 

data  <- read.csv(file = "Big_CEO.csv", stringsAsFactors = FALSE)

#data$month_year <- format(as.Date(data$Date), "%Y-%m") ###extracting month and year to build fiscal quarter graphs, need a new variable bc if not it'll give us issues

data <- data["2019-03-01"<= data$Date & data$Date <= "2021-04-01",] #subsetting covid dates 
data <- data %>% filter(WC<=5400) %>% #filter out based on our exclusion criteria
  filter(WC>=25)

df <- data[, c(1:9)] #we trimmed it down 

#tokenize the data 
df_bigrams <- df %>% 
  unnest_tokens(bigram, aggregated_text, token = 'ngrams', n = 2) %>% 
  filter(!is.na(bigram))

#count up the bigrams, which will take awhile with the CEO dataset. consider running this as a background job 
job::job({
bigrams_separated <- df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
})

###take out stop words
job::job({
  bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
})

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# filter for only relatively common combinations. bigger is better here
bigram_graph <- bigram_counts %>%
  filter(n > 400) %>%
  graph_from_data_frame()


#start building
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

#more refinedgraph
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
=======
pacman::p_load(tidyverse, tidytext,janeaustenr, igraph,ggraph,  install = T)
>>>>>>> e285558d9a6a0a325dc50f3498d55b321dd245bf
