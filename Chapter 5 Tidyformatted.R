library(pacman)
pacman::p_load(tidyverse, tidytext,janeaustenr, tm, topicmodels,quanteda, tm.plugin.webmining, purrr, install = T)

#Chapter 5: Converting to and from non-tidy formats 

#Document-term matrix 
#each from represents one doc
#each column represents one term 
#each value (usually) contains the number of appearances of that term in the document 

#5.1.1 Tidying DocumentTermMatrix objects

data("AssociatedPress", package = 'topicmodels') #load in the data

AssociatedPress#look at the data

terms <- Terms(AssociatedPress) #gather the terms as an object 

head(terms)#take a peek

ap_td <- tidy(AssociatedPress) #tidy up the data
ap_td

#now that it's tidy we can do things do it like sentiment analysis 
ap_sentiments <-  ap_td %>% 
  inner_join(get_sentiments('bing'), by = c(term = 'word')) #binding by word

ap_sentiments

#we can now visualize this 
ap_sentiments %>% 
  count(sentiment, term, wt = count) %>% 
  ungroup() %>% 
  filter(n >=200) %>% 
  mutate(n = ifelse(sentiment=='negative',-n,n)) %>% 
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL)

#looking at document-term matrices
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- data_corpus_inaugural %>%
  quanteda::tokens() %>%
  quanteda::dfm(verbose = FALSE)
inaug_dfm

#we can then tokenize it 
inaug_td <- tidy(inaug_dfm)
inaug_td

#We may be interested in finding the words most specific to each of the inaugural speeches. 
#This could be quantified by calculating the tf-idf of each term-speech pair using the bind_tf_idf() function, as described in Chapter 3.

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))
inaug_tf_idf

#plot for 4 presidents 
inaug_tf_idf %>%
  filter(document == "1861-Lincoln"|document == '1933-Roosevelt'|document=='1961-Kennedy'|document == '2009-Obama') %>% 
  group_by(document) %>% #
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, term, fill = document)) + #need to change word to bigram, trigram, etc.
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol = 2, scales = "free") + #facet wrap by book 
  labs(x = "tf-idf", y = NULL)

#As another example of a visualization possible with tidy data, we could extract the year from each document’s name, 
#and compute the total number of words within each year.

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

#we can also plot this 
year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() + #sing data smoothing to plot thos 
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% frequency of word in inaugural address")


#casing tidy text data into a matrix 
#For example, we could take the tidied AP dataset and cast it back into a document-term matrix using the cast_dtm() function.
ap_td %>% 
  cast_dfm(document, term, count)

#Similarly, we could cast the table into a dfm object from quanteda’s dfm with cast_dfm().
ap_td %>%
  cast_dfm(document, term, count)
#> Document-feature matrix of: 2,246 documents, 10,473 features (98.72% sparse) and 0 docvars.
library(Matrix)

# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)

dim(m)

#we can also convert other datatypes to matrices
austen_dtm <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word) %>% 
  cast_dtm(book, word, n)
austen_dtm

#This casting process allows for reading, filtering, and processing to be done using dplyr and other tidy tools, 
#after which the data can be converted into a document-term matrix for machine learning applications. 

#dealing with metadata
#read in the data

data('acq')

#look at the first documetn 
acq[[1]]

#we can also tidy the data to make the corpus object easier to read
acq_td <- tidy(acq)
acq_td

#we can then tokenize the data 
acq_tokens <- acq_td %>% 
  select(-places) %>% #remove places
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = 'word') #remove stop words

#most comon words 
acq_tokens %>% 
  count(word, sort = T)

#we can also get tf-dif 
acq_tokens %>% 
  count(id, word) %>% 
  bind_tf_idf(word, id, n) %>% 
  arrange(desc(tf_idf))

#mining financial articles 

#Corpus objects are a common output format for data ingesting packages, which means the tidy() 
#function gives us access to a wide variety of text data. One example is tm.plugin.webmining, 
#which connects to online feeds to retrieve news articles based on a keyword. 
#For instance, performing WebCorpus(GoogleFinanceSource("NASDAQ:MSFT")) allows us to retrieve the 20 most recent articles 
#related to the Microsoft (MSFT) stock


#set the companies' names and symbols that we are interested in and save as objects
library(devtools)
install_github("mannau/tm.plugin.webmining") #need to use github bc it's no longer on CRAN
library(tm.plugin.webmining)
library(purrr)
company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook", 
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol  <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", 
             "TWTR", "IBM", "YHOO", "NFLX")
download_articles <- function(symbol) {
  WebCorpus(YahooFinanceSource(paste0("NASDAQ:", symbol))) #used yahoo instead of google
}

stock_articles <- tibble(company = company,
                         symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))

#let's clean this all up then 
stock_tokens <- stock_articles %>%
  mutate(corpus = map(corpus, tidy)) %>%
  unnest(cols = (corpus)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading) #keep running into the same issue 
