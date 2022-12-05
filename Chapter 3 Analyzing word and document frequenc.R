#Analyzing word and document frequency 

#A central question in text mining and natural language processing is how to quantify what a document is about

#One measure of how important a word may be is its term frequency (tf), 
#how frequently a word occurs in a document.

#Another measure is inverse document frequency (idf), which decreases the weight 
#for commonly used words and increases the weight for words that are not used very much in a collection of documents.

#these two can be combined  to calculate a term’s tf-idf (the two quantities multiplied together), 
#the frequency of a term adjusted for how rarely it is used.

#let's get started by loading in the packages we will need 
library(pacman)
pacman::p_load(tidyverse, tidytext, janeaustenr, forcats, gutenbergr, install = TRUE)

#Let’s start by looking at the published novels of Jane Austen and examine first term frequency, then tf-idf.
book_words <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word, sort = T)
book_words #most common words (tokenized) grouped by book 

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
total_words #getting the WC of each book and storing as an object 

book_words <- left_join(book_words, total_words) #joining two datasets

book_words#take a look #now we have the total # of occurrences of words in each book and we can compare them to the total

#now we can build a metric to compare the frequency of unique words to the total and graph w
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

#There are very long tails to the right for these novels (those extremely rare words!) 
#that we have not shown in these plots. These plots exhibit similar distributions for all the novels, 
#with many words that occur rarely and fewer words that occur frequently.

#Zipf's Law 
#the frequency that a word appears is inversely proportional to its rank.

#Let's look at Zipf's law with our data 
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()
freq_by_rank

#plot zipfs law 
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#Let’s see what the exponent of the power law is for the middle section of the rank range.
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

#get a coefficient close to -1 (or constant)

#but now let's combine this with the other graph
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, #inputting our slope manually 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#The deviations we see here at high rank are not uncommon for many kinds of language;
#a corpus of language often contains fewer rare words than predicted by a single power law. 
#The deviations at low rank are more unusual. 
#Jane Austen uses a lower percentage of the most common words than many collections of language.

#now let's look at The bind_tf_idf() function

#The idea of tf-idf is to find the important words for the content of each document 
#by decreasing the weight for commonly used words and increasing the weight for words
#that are not used very much in a collection or corpus of documents, in this case, 
#the group of Jane Austen’s novels as a whole. Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common.

#let's execute out function 
book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n) #word contains the tokens, book is what we group by, n = the number of occurances 
book_tf_idf #Notice that idf and thus tf-idf are zero for these extremely common words. 
#These are all words that appear in all six of Jane Austen’s novels, 
#so the idf term (which will then be the natural log of 1) is zero. 

#now let's look at words with high tf_idf 
book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#let's visualize this now 
book_tf_idf %>%
  group_by(book) %>% #group by book 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") + #facet wrap by book 
  labs(x = "tf-idf", y = NULL)

#What measuring tf-idf has done here is show us that Jane Austen used similar language across her six nov 
#the names of people and places. 

#now let's see what terms are important in other texts, physics in particular 
physics <- gutenberg_download(c(37729, 14725, 13476, 30155),  #pulling our texts 
                              meta_fields = "author")

#let's tokenize stuff now 
physics_words <- physics %>% 
  unnest_tokens(word, text) %>% 
  count(author, word, sort = T)
physics_words

#let's visualize the td-idf for these works 
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
plot_physics %>% #now plot it 
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free")

physics %>% 
  filter(str_detect(text, "_k_")) %>% #looking at when the special symbol occurs
  select(text)

physics %>% 
  filter(str_detect(text, "RC")) %>% 
  select(text)

#now we should remove some of the meaningless stop words with a custom list 

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))
physics_words <- anti_join(physics_words, mystopwords, 
                           by = "word") #now remove them 

plot_physics <- physics_words %>% #tidy the data first before we graph 
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>% #removing underscores 
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

#plotting Highest tf-idf words in classic physics texts
ggplot(plot_physics, aes(tf_idf, word, fill = author)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#now let's apply it to the CEO dataset 

df <- read.csv("Tech_CEOS copy.csv") 
#get rid of the LIWC output :(
df <- df[, c(1:6)] #we trimmed it down 

str(df)#make sure it worked

#let's start to build the datasets that we need
data_words <-  df %>% 
  filter(Speaker == "Elon Musk"|Speaker == "Jack Dorsey"|Speaker == "Tim Cook") %>%  #trim it bc this dataset's huge
  unnest_tokens(word, Text) %>% 
  count(Speaker, word, sort = T)

total_words <- data_words %>% 
  group_by(Speaker) %>% 
  summarize(total = sum(n))

call_words <- left_join(data_words, total_words) #join the two together 

#let's plot it all now 
ggplot(call_words, aes(n/total, fill = Speaker)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~Speaker, ncol = 2, scales = "free_y")
#again, we see the really long tails of the distributions 

#now let's rank the most common words 
freq_by_rank <- call_words %>% 
  group_by(Speaker) %>% 
  mutate(rank = row_number(), #ranking by the row number they appear in 
         `term frequency` = n/total) %>%
  ungroup()
freq_by_rank

#now let's plot zipf's law
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Speaker)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

#fit the line with the graph we built earlier 
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Speaker)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
#we can see that it's pretty close to the line of best fit 

#let's look at tf-idf now to see what the important words are 
call_tf_idf <- call_words %>%
  bind_tf_idf(word, Speaker, n) #make sure to include Speaker!
call_tf_idf

#now let's prune this to be the words that are higher in tf_idf
call_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#again, we see a tonnnn of proper nouns, but let's visualize this 
call_tf_idf %>%
  group_by(Speaker) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Speaker)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Speaker, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
