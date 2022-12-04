#CH2 Sentiment Analysis

#use get_sentiments... to get the sentiments 
library(tidytext) #load in our packages 
library(tidyverse)
library(textdata)
get_sentiments("afinn") #The AFINN lexicon assigns words with a score that runs between -5 and 5, 
#with negative scores indicating negative sentiment and positive scores indicating positive sentiment.

get_sentiments("bing") #gives us the valence of it with simple negative or positive 

get_sentiments("nrc") #gives us different dimensions  

#now lets look at some of jane austen's work and how they correspond with the NRC lexicon 
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) #Notice that we chose the name word for the output column from unnest_tokens(). 
#This is a convenient choice because the sentiment lexicons and stop word datasets have columns named word; 
#performing inner joins and anti-joins is thus easier.

#now that we have tidy data, let's start to do the sentiment analysis

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy") #selecting the joy words and storing as an object 

tidy_books %>% 
  filter(book == "Emma") %>% #selecting just EMMA 
  inner_join(nrc_joy) %>% 
  count(word, sort = T)
#output gives us the joy words 

#Next, we count up how many positive and negative words there are in defined sections of each book. 
#We define an index here to keep track of where we are in the narrative; this index (using integer division) 
#counts up sections of 80 lines of text.


#Small sections of text may not have enough words in them to get a good estimate 
#of sentiment while really large sections can wash out narrative structure. 
#For these books, using 80 lines works well, but this can vary depending on individual texts, 
#how long the lines were to start with, etc. 
#We then use pivot_wider() 
#so that we have negative and positive sentiment in separate columns, and lastly calculate a net sentiment (positive - negative).


jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>% #The %/% operator does integer division (x %/% y is equivalent to floor(x/y)) so the index keeps track of which 80-line section of text we are counting up negative and positive sentiment in.
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) #so the larger the number the more positive the sentiment is

#we can also plot these 
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) + #remember index is linenumber %/% 80
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") #we are allowing the x axis to vary 


#we can also compare the three sentiment dictionaries to themselves

#let's examine how the sentiment changes across pride and prejudice 

pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

#doing afinn 
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber%/%80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

#bing and nrc 
bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#stich all three datasets together and then graph 
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") #method is the grouping variable 

#we can also look at the biases for the lexicons 
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

#Both lexicons have more negative than positive words, but the ratio of negative 
#to positive words is higher in the Bing lexicon than the NRC lexicon. 
#This will contribute to the effect we see in the plot above, as will any systematic difference in word matches

#look at the most common positive and negative words in the datasets

#getting the word counts for bing words 

bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup() #removing the group (negative or positive) to just see the raw counts

#we can also pipe this straight into making a graph 
#as we can see, the word miss (used as a noun) is being miscoded as a negative word, so we can fix that 
bing_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
#make a custom stop word list 
custom_stop_words <- bind_rows(tibble(word = c("miss"),  #the word(s) we want to include and add them to the stop list
                                      lexicon = c("custom")), 
                               stop_words)
custom_stop_words

#building wordclouds 
library(wordcloud)


tidy_books %>% 
  anti_join(stop_words) %>% #joining stop words
  count(word) %>% 
  with(wordcloud(word,n,max.words = 100))

#comparing positive and negative words via wordcloud 
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)


#looking at units beyond words 

#for example we can look at entire sentences
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

p_and_p_sentences$sentence[2]

#but there seems to be some trouble at times 
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n()) #recover the number of chapters

#let’s make a data frame of how many words are in each chapter so we can 
#normalize for the length of chapters. Then, let’s find the number of negative words 
#in each chapter and divide by the total words in each chapter. 
#For each book, which chapter has the highest proportion of negative words?

bingnegative <- get_sentiments("bing") %>% #pull the negative wordsform the bing sentiment analysis 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>% #getting wordcounts for each chapter
  group_by(book, chapter) %>%
  summarize(words = n())

#looking at each book at chapter, which has the highest amount of negative words
tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% 
  ungroup()

#now let's apply all of this to the CEO data. It's important to practice!
df <- read.csv("Tech_CEOS copy.csv") 
#get rid of the LIWC output :(
df <- df[, c(1:6)] #we trimmed it down 

#let's start to get the token-level sentiments for speakers
tidy_df <- df %>% 
  unnest_tokens(word, Text) %>%  # we dont need to group bc the data is structured for us already :)
  anti_join(stop_words)

nrc_joy <- get_sentiments("nrc") %>% # lets build an object with the joy words 
  filter(sentiment == "joy")

nrc_neg<- get_sentiments("nrc") %>% # lets build an object with the joy words 
  filter(sentiment == "negative")#but let's also get negative words bc why not 

#lets get the freq of joy and negative words 
#joy
tidy_df %>% 
  filter(Speaker == 'Shantanu Narayen' | Speaker == "Tim Cook"| Speaker == "Elon Musk") %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = T)

#negative
tidy_df %>% 
  filter(Speaker == 'Shantanu Narayen' | Speaker == "Tim Cook"| Speaker == "Elon Musk") %>% 
  inner_join(nrc_neg) %>% 
  count(word, sort = T)

#now let's get a varying length of text to look at sentiments 
ceo_sentiments <- tidy_df %>%
  filter(Speaker == 'Shantanu Narayen' | Speaker == "Tim Cook"| Speaker == "Elon Musk") %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(Speaker, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) #larger value = more positive 

#let's graph this now. it'll look different from the sample data bc we didn't use an index variable
ggplot(ceo_sentiments, aes(Speaker, sentiment, fill = Speaker)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Speaker, ncol = 2, scales = "free_x")

#let's compare how the sentiment analyses vary. first we need to pick a spaker so let's go with Elon
elon_data <- tidy_df %>% 
  filter(Speaker == "Elon Musk")

elon_data #check the data out 

#now let's calcualte the sentiments for the differetn versions using Elon's data 
afinn <- elon_data %>% 
  inner_join(get_sentiments("afinn")) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")
afinn

bing_and_nrc <- bind_rows(
  elon_data %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  elon_data %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method,sentiment) %>% #just remove index from the original chunk and youre fine 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

#now we need to combine the two datasets and graphs 
bind_rows(afinn, bing_and_nrc) %>% 
  ggplot(aes(method, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") #again, free y allows the y axis to vary freely 

#now let's get the most common positive and negative words 
bing_word_counts <- tidy_df %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup() %>%  # so the unfiltered version gives us a ton of words, let's trim this now to n > 200
  filter(n > 200)
bing_word_counts

#visualize it 
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + #grouping by sentiment 
  labs(x = "Contribution to sentiment",
       y = NULL)

#we can see that cloud is mis-coded as negative, so we *should* make a custom stop list that removes it from being counted
#in the context of this dataset, cloud refers to a server 

#making custom stop list
custom_stop_words <-  bind_rows(tibble(word = c("cloud"),
                                       lexicon = c("custom")), #what we are callintgthe lexicon that we are adding to stop words
                                stop_words)

#now let's build a word cloud of the positive and negative words in the dataset 

tidy_df %>% 
  inner_join(get_sentiments("bing")) %>% 
  anti_join(custom_stop_words) %>% #add the custom stop words! we don't want cloud in our word...cloud!
  count(word, sentiment, sort = T) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words = 100)

###That's all for now!
  
  
=======
>>>>>>> 480f2db2750f239a5f395380efeb2d49a2d969fe
=======
>>>>>>> 480f2db2750f239a5f395380efeb2d49a2d969fe
